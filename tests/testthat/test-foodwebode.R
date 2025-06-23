test_that("stoichiometry works", {
  ei = rbind(matrix(0, nrow = 4, ncol = 4), c(150, 150*0.05, 150*0.016, 150*0.018))
  ii = c(0,10,0.5,0.01)
  io = c(0,0.1, 0.1, 0.1)
  parameters = getPARAMS(intro_comm, externalinputs = ei, inorganicinputs = ii, inorganicloss = io, densitydependence = c(0,0,0,0,0))

  with(list(y = parameters$yeqm*c(rep(c(1,1,1,1,1.1), 4),1,1,1,1), pars = parameters$parameters), {
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
    consumption = lapply(c(1,2,3,4),
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

    f.con.a = expand_mat(lapply(1:dim(Qmat)[1],
                                function(X){
                                  cbind(Qmat[X,-1], diag(x=-1, nrow = dim(Qmat)[2]-1, ncol = dim(Qmat)[2]-1))
                                }))

    f.dir.a = rep("=", length(f.rhs.a))

    f.rhs.b = rep(0, length(Qmat))

    f.con.b = expand_mat(
      replicate(dim(Qmat)[1],
                diag(x=1, nrow = dim(Qmat)[2], ncol = dim(Qmat)[2]),
                simplify = F)
    )

    f.dir.b = rep(">=", length(Qmat))

    f.obj = rep(0, dim(Qmat)[2]); f.obj[1] = 1; f.obj = rep(f.obj, times = dim(Qmat)[1])

    min_sol = lpSolve::lp(direction = "min", f.obj,
                          rbind(f.con.a, f.con.b),
                          c(f.dir.a, f.dir.b),
                          c(f.rhs.a, f.rhs.b))

    mineralization2 = matrix(min_sol$solution, nrow = nrow(Qmat), byrow = T)

    # Calculate mineralization:
    mineralization_list = vector('list', length = dim(Qmat)[1])

    for(ii in 1:length(mineralization_list)){
      f.rhs.a = unname(c((netwithoutmineralization[,1]*Qmat - netwithoutmineralization)[ii,-1]))

      f.con.a = cbind(Qmat[ii,-1], diag(x=-1, nrow = dim(Qmat)[2]-1, ncol = dim(Qmat)[2]-1))

      f.dir.a = rep("=", dim(f.con.a)[1])

      f.rhs.b = rep(0, dim(Qmat)[2])
      #unname(pars$canIMMmat[ii,-1]*netinog[-1]*-1)
      #rep(0, dim(Qmat)[2]) # where to add inorganic...

      f.con.b = diag(x=1, nrow = dim(Qmat)[2], ncol = dim(Qmat)[2])

      f.dir.b = rep(">=", dim(Qmat)[2])

      f.obj = rep(0, dim(Qmat)[2]); f.obj[1] = 1

      min_sol = lpSolve::lp(direction = "min", f.obj,
                            rbind(f.con.a, f.con.b),
                            c(f.dir.a, f.dir.b),
                            c(f.rhs.a, f.rhs.b))

      mineralization_list[[ii]] = min_sol$solution
    }


    mineralization3 = do.call("rbind", mineralization_list)

    expect_equal(mineralization2, mineralization3)

  }
  )
})
