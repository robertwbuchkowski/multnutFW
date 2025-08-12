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

  yunstd = y*pars$eqmStandard

  biomass = yunstd[1:nrow(pars$pmat)]

  Det_Qmat = yunstd[(nrow(pars$pmat)+1):(nrow(pars$pmat)+sum(pars$detplant$isDetritus)*(ncol(pars$pmat)-1))]

  Det_Qmat = Det_Qmat/biomass[pars$detplant$isDetritus == 1]

  # ymat = matrix(y[1:(nrow(pars$pmat)*ncol(pars$pmat))], nrow = nrow(pars$pmat), ncol = ncol(pars$pmat))

  # Qmat = sweep(ymat, 1, ymat[, 1], "/")

  Qmat = pars$Qmat

  Qmat[pars$detplant$isDetritus == 1,] = c(Qmat[pars$detplant$isDetritus == 1, 1],Det_Qmat)

  ymat = biomass*Qmat

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
    pars$nodeloss*ymat +

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
      nrow = nrow(ymat), ncol = ncol(ymat),byrow = T) # arrange in a matrix by row so that the elements are in the columns.

  if(any(is.na(c(pars$inorganicinputs,pars$inorganicloss)))){
    immobilization = -pars$canIMMmat*netwithoutmineralization
    immobilization[immobilization < 0] = 0
  }else{
    stop("not working yet")
    # Assign the inorganic nutrient pools:
    inorganicSV = y[c((nrow(pars$pmat)*ncol(pars$pmat))+1):length(y)]

    # Calculate the total amount of inorganic nutrient available to the food web:
    netinog = inorganicSV + pars$inorganicinputs - inorganicSV*pars$inorganicloss

    immobilization = -pars$canIMMmat*netwithoutmineralization
    immobilization[immobilization < 0] = 0

    n_groups <- nrow(immobilization)
    n_nutrients <- ncol(immobilization)

    allocation <- matrix(0, nrow = n_groups, ncol = n_nutrients)

    for (j in seq_len(n_nutrients)) {
      nutrient_available <- netinog[j]
      nutrient_demand <- immobilization[, j]
      total_demand <- sum(nutrient_demand)

      if (nutrient_available >= total_demand) {
        # Enough nutrient to meet all demand
        allocation[, j] <- nutrient_demand
      } else {
        stop("Not working yet.")
        # Not enough nutrient: allocate by biomass weight
        weights <- ymat[,1] / sum(ymat[,1])
        alloc <- weights * nutrient_available
        allocation[, j] <- pmin(alloc, nutrient_demand)

        # Optional: redistribute remainder if some demand is still unmet
        remainder <- nutrient_available - sum(allocation[, j])
        if (remainder > 0) {
          unmet <- nutrient_demand - allocation[, j]
          unmet_weights <- ifelse(unmet > 0, ymat[,1], 0)
          if (sum(unmet_weights) > 0) {
            unmet_weights <- unmet_weights / sum(unmet_weights)
            allocation[, j] <- allocation[, j] + unmet_weights * remainder
            allocation[, j] <- pmin(allocation[, j], nutrient_demand)
          }
        }
      }
    }
    immobilization = allocation
  }

  # Add in immobilized nutrients:
  netwithoutmineralization = netwithoutmineralization + immobilization

  # Minimum respiration based on biomass:
  minresp = pars$ECarbon*ymat[,1]

  # Apply stoichiometry to get actual carbon mineralization:
  actualresp = pmin(-minresp,
                    apply(netwithoutmineralization*Qmat # Convert all losses to carbon
                          -netwithoutmineralization[,1], # Subtract net carbon gain or loss
                          1, min) # Get the minimum required respiration
  ) # Take either the basal respiration rate or the minimum respiration rate from nutrient limitation.

  # Remove detritus respiration if created:
  actualresp[which(pars$detplant$isDetritus == 1)] = 0

  netwithrespiration = netwithoutmineralization + # Positive because respiration is already negative.
    matrix(c(actualresp, rep(0, nrow(ymat)*(ncol(ymat)-1))), nrow = nrow(ymat), ncol = ncol(ymat))

  mineralization = netwithrespiration - netwithrespiration[,1]/Qmat

  # Remove detritus mineralization (does not exhibit this stoichiometry):

  mineralization[which(pars$detplant$isDetritus == 1),] = 0

  # Calculate changes in inorganic pools:
  if(any(is.na(c(pars$inorganicinputs,pars$inorganicloss)))){
    dinorganic = colSums(mineralization) - colSums(immobilization)
  }else{
    dinorganic = colSums(mineralization) - colSums(immobilization) + pars$inorganicinputs - inorganicSV*pars$inorganicloss
  }

  # Calculate the net changes with mineralization:
  netwithmineralization = netwithrespiration - mineralization

  D_element_biomass = (netwithmineralization[which(pars$detplant$isDetritus == 1),-1])

  if(any(is.na(c(pars$inorganicinputs,pars$inorganicloss)))){
    dy = c(netwithmineralization[,1],D_element_biomass)/pars$eqmStandard
    names(dy) = names(y)
    return(list(dy, dinorganic = dinorganic))
  }else{
    stop("not working yet")
    dy = c(netwithmineralization[,1],D_element_biomass, dinorganic)
    names(dy) = names(y)
    return(list(dy))
  }
}
