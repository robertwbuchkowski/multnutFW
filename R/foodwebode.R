#' A function to simulation the food webs away from equilibrium.
#'
#' @param t The ODE time.
#' @param y The ODE simulation start.
#' @param pars The ODE parameters.
#' @details
#' The food web model for simulating over time: requires \code{y} inputs and \code{pars} parameters from the \code{\link{getPARAMS}} function.
#' @return The changes in each node biomass along with parameters and mineralization rates.
#' @export
foodwebode <- function(t,y,pars){

  ymat = matrix(y[1:(nrow(pars$pmat)*ncol(pars$pmat))], nrow = nrow(pars$pmat), ncol = ncol(pars$pmat))

  Qmat = sweep(ymat, 1, ymat[, 1], "/")

  # Losses from detritus:
  detloss = matrix(0, nrow = nrow(pars$pmat), ncol = ncol(pars$pmat))
  detloss[which(pars$detplant$isDetritus == 1),] = pars$detritusloss

  # Calculate the consumption rates:
  predC = matrix(ymat[,1], nrow = nrow(pars$cij), ncol = ncol(pars$cij))
  preyC = t(predC)
  consumption_Carbon = pars$cij*predC*preyC/(1 + pars$cij*pars$h*preyC)

  # Calculate consumption rates:
  consumption = lapply(seq(1, ncol(Qmat),1),
                       function(lai) {
                         consumption_Carbon*matrix(Qmat[,lai], nrow = nrow(pars$cij), ncol = ncol(pars$cij), byrow = T)
                       })


  rm(predC, preyC, consumption_Carbon)

  netwithoutmineralization =

    # Gains from outside the system:
    pars$externalinputs -

    # Loss from detritus:
    detloss*ymat +

    # Gains from consumption:
    pars$pmat*sapply(Map(function(XX,YY) XX*YY, consumption, pars$assimilation), rowSums) -
    # Losses from predation:
    sapply(consumption, colSums) -
    # Natural death:
    matrix(pars$death[,1]*(1-pars$death[,3])*ymat[,1] + # density-independent
             pars$death[,2]*(pars$death[,3])*ymat[,1]*ymat[,1], # density-dependent
           nrow = nrow(ymat), ncol = ncol(ymat))*Qmat +
    # Detritus recycling:
    matrix(pars$detplant$FecalRecycling, nrow = nrow(ymat), ncol = ncol(ymat))* # A matrix to allocate the detritus recycling appropriately
    matrix(
      colSums(sapply(Map(function(XX,YY) XX*YY, consumption, lapply(pars$assimilation, function(X) 1 - X)), rowSums)), # A vector of unassimilated material (i.e., faeces)
      nrow = nrow(ymat), ncol = ncol(ymat),byrow = T) +
        matrix(pars$detplant$NecromassRecycling, nrow = nrow(ymat), ncol = ncol(ymat))* # A matrix to allocate the detritus recycling appropriately
        matrix(
        colSums(matrix(pars$death[,1]*(1-pars$death[,3])*ymat[,1] + # density-independent death
                         pars$death[,2]*pars$death[,3]*ymat[,1]*ymat[,1], nrow = nrow(ymat), ncol = ncol(ymat))*Qmat), # density-dependent death
      nrow = nrow(ymat), ncol = ncol(ymat),byrow = T) - # arrange in a matrix by row so that the elements are in the columns.

    # Respiration based on biomass:
    matrix(c(pars$ECarbon*ymat[,1], rep(0, nrow(ymat)*(ncol(ymat)-1))), nrow = nrow(ymat), ncol = ncol(ymat))

  # Assign the inorganic nutrient pools:
  inorganicSV = y[c((nrow(pars$pmat)*ncol(pars$pmat))+1):length(y)]

  # Calculate the total amount of inorganic nutrient available to the food web:
  netinog = inorganicSV + pars$inorganicinputs - inorganicSV*pars$inorganicloss

  # Create a function to expand the matrix:
  expand_mat <- function(matrices){

    matrix_rows = nrow(matrices[[1]])
    matrix_cols = ncol(matrices[[1]])

    # Define the larger matrix
    larger_matrix <- matrix(0, nrow = length(matrices) * matrix_rows, ncol = length(matrices) * matrix_cols)

    # Place matrices on the diagonal of the larger matrix
    for (i in 1:length(matrices)) {
      start_row <- (i - 1) * matrix_rows + 1
      end_row <- i * matrix_rows
      start_col <- (i - 1) * matrix_cols + 1
      end_col <- i * matrix_cols
      larger_matrix[start_row:end_row, start_col:end_col] <- matrices[[i]]
    }

    return(larger_matrix)
  }

  f.rhs.a = c(t((netwithoutmineralization[,1]*Qmat - netwithoutmineralization)[,-1]))

  # This constraint matrix contains the stoichiometric balance. There is the C:X ratio and then mineralization followed by immobilization (negative mineralization). lpSolve requires all solution values to be positive, so we need a separate value for immobilization.
  f.con.a = expand_mat(lapply(1:dim(Qmat)[1],
                              function(X){
                                cbind(
                                  cbind(Qmat[X,-1], diag(x=-1, nrow = dim(Qmat)[2]-1, ncol = dim(Qmat)[2]-1)),
                                  diag(x=unname(pars$canIMMmat[X,2:dim(pars$canIMMmat)[2]]), nrow = dim(Qmat)[2]-1, ncol = dim(Qmat)[2]-1))
                              }))

  f.dir.a = rep("=", length(f.rhs.a))

  # Switch depending on whether we are limiting inorganic nutrients:
  if(any(is.na(netinog))){ # When NOT tracking inorganic nutrients:

    f.rhs.b = as.vector(t(unname(cbind(matrix(0, nrow = nrow(pars$canIMMmat), ncol = ncol(pars$canIMMmat)-1), pars$canIMMmat))))

    # Which constaints to keep:
    ctk = which(f.rhs.b == 0)

    f.rhs.b = f.rhs.b[ctk]


    f.con.b = diag(ncol(f.con.a))[ctk,]

    f.dir.b = rep(c(rep(">=", ncol(Qmat)), rep("<=", ncol(Qmat)-1)), nrow(Qmat))[ctk]

  }else{ # When tracking inorganic nutrients:
    f.rhs.b = as.vector(t(unname(cbind(matrix(0, nrow = nrow(pars$canIMMmat), ncol = ncol(pars$canIMMmat)-1), sweep(pars$canIMMmat,2,as.vector(netinog)*1, "*"))))) # This code puts the total available inorganic nutrients into the rhs for any node that can immobilize it. It is negative because the mineralization must be greater than that and negative mineralization is immobilization.

    f.con.b = diag(ncol(f.con.a))

    f.dir.b = rep(c(rep(">=", ncol(Qmat)), rep("<=", ncol(Qmat)-1)), nrow(Qmat))
  }



  # Now we need to add constraints for any potential conflict between nodes:
  if(any(colSums(pars$canIMMmat) > 1) & !any(is.na(netinog))){

    f.con.c = matrix(0, nrow = dim(Qmat)[2], ncol = ncol(f.con.b))

    te01 = unname(cbind(cbind(pars$canIMMmat[,1],matrix(data = 0, nrow = nrow(pars$canIMMmat), ncol = ncol(pars$canIMMmat)-1)),pars$canIMMmat[,-1]))

    for(ei in ncol(pars$canIMMmat):ncol(te01)){
      te02 = matrix(0, nrow = nrow(te01), ncol = ncol(te01))
      te02[,ei] = te01[,ei]
      f.con.c[(ei - ncol(pars$canIMMmat)+1),] = c(t(te02))
    }

    f.dir.c = rep("<=", dim(Qmat)[2])

    f.rhs.c = as.vector(netinog)

    c_to_keep = which(rowSums(f.con.c)>1)

    f.con.c = f.con.c[c_to_keep,]
    f.dir.c = f.dir.c[c_to_keep]
    f.rhs.c = f.rhs.c[c_to_keep]

    f.con.b = rbind(f.con.b, f.con.c)
    f.dir.b = c(f.dir.b, f.dir.c)
    f.rhs.b = c(f.rhs.b, f.rhs.c)
  }

  f.obj = rep(0, dim(Qmat)[2]+dim(Qmat)[2]-1); f.obj[1] = 1; f.obj = rep(f.obj, times = dim(Qmat)[1])

  min_sol = lpSolve::lp(direction = "min", f.obj,
                        rbind(f.con.a, f.con.b),
                        c(f.dir.a, f.dir.b),
                        c(f.rhs.a, f.rhs.b))

  mineralization = matrix(min_sol$solution, nrow = nrow(Qmat), byrow = T)

  mineralization = cbind(mineralization[,1], mineralization[,-1][,1:(ncol(mineralization[,-1])/2)] - mineralization[,-1][,(1+ncol(mineralization[,-1])/2):ncol(mineralization[,-1])])

  mineralization = mineralization*matrix(1-pars$detplant$isDetritus, nrow = nrow(ymat), ncol = ncol(ymat))

  # Calculate the mineralization rate given the change in carbon and fixed C:X ratio for all non-detritus nodes:
  mineralization2 = (netwithoutmineralization - matrix(netwithoutmineralization[,1], nrow = nrow(ymat), ncol = ncol(ymat))*Qmat)*matrix(1-pars$detplant$isDetritus, nrow = nrow(ymat), ncol = ncol(ymat))

    # Calculate changes in inorganic pools:
  if(any(is.na(c(pars$inorganicinputs,pars$inorganicloss)))){
    dinorganic = colSums(mineralization)
  }else{
    dinorganic = colSums(mineralization) + pars$inorganicinputs - inorganicSV*pars$inorganicloss
  }

  # Calculate the net changes with mineralization:
  netwithmineralization = netwithoutmineralization - mineralization

  dy = c(netwithmineralization, dinorganic)
  names(dy) = names(y)

  return(list(dy))
}
