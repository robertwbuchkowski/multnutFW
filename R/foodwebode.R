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
    matrix(pars$detplant$DetritusRecycling, nrow = nrow(ymat), ncol = ncol(ymat))* # A matrix to allocate the detritus recycling appropriately
    matrix(
      colSums(sapply(Map(function(XX,YY) XX*YY, consumption, lapply(pars$assimilation, function(X) 1 - X)), rowSums)) + # A vector of unassimilated material (i.e., faeces)
        colSums(matrix(pars$death[,1]*(1-pars$death[,3])*ymat[,1] + # density-independent death
                         pars$death[,2]*pars$death[,3]*ymat[,1]*ymat[,1], nrow = nrow(ymat), ncol = ncol(ymat))*Qmat), # density-dependent death
      nrow = nrow(ymat), ncol = ncol(ymat),byrow = T) - # arrange in a matrix by row so that the elements are in the columns.

    # Respiration based on biomass:
    matrix(c(pars$ECarbon*ymat[,1], rep(0, nrow(ymat)*(ncol(ymat)-1))), nrow = nrow(ymat), ncol = ncol(ymat))

  # Calculate the mineralization rate given the change in carbon and fixed C:X ratio for all non-detritus nodes:
  mineralization = (netwithoutmineralization - matrix(netwithoutmineralization[,1], nrow = nrow(ymat), ncol = ncol(ymat))*Qmat)*matrix(1-pars$detplant$isDetritus, nrow = nrow(ymat), ncol = ncol(ymat))


  # Assign the inorganic nutrient pools:
  inorganicSV = y[c((nrow(pars$pmat)*ncol(pars$pmat))+1):length(y)]

  # Calculate the total amount of inorganic nutrient available to the food web:
  netinog = inorganicSV + pars$inorganicinputs - inorganicSV*pars$inorganicloss

  # Add in any available mineral nutrient to the species that can immobilize it:
  if(any(netinog <colSums(pars$canIMMmat*mineralization))) stop("Not enough inorganic nutrient to meet demand.")

  # Calculate the limiting nutrient:
  mineralization2 = mineralization
  mineralization2[abs(mineralization2) < 1e-8] = 0
  mineralization2[pars$canIMMmat == 1] = 1 # make this bigger than C, so that it can't be the limiting nutrient

  limitingnutrient = apply(mineralization2, 1, which.min)

  minlimnut = rep(NA, dim(ymat)[1])

  for(i in 1:dim(ymat)[1]){
    minlimnut[i] = netwithoutmineralization[i, limitingnutrient[i]]
  }

  # Confirm that immobilization is only happening for species for which it is possible:



  if(any(mineralization2*(1- pars$canIMMmat) < 0)) stop("Immobilization is occuring in error")
  rm(mineralization2)



  # Calculate changes in inorganic pools:
  dinorganic = colSums(mineralization) + pars$inorganicinputs - inorganicSV*pars$inorganicloss

  # Calculate the net changes with mineralization:
  netwithmineralization = netwithoutmineralization - mineralization

  dy = c(netwithmineralization, dinorganic)
  names(dy) = names(y)

  return(list(dy, limitingnutrient = limitingnutrient))
}
