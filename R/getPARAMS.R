#' A function to get the parameters for a food web model.
#'
#' @param usin The community you want to simulate.
#' @param DIETLIMITS The diet limits matrix for the stoichiometry correction (proportion of diet)?
#' @param diet_correct Boolean: Does the organism correct it's diet?
#' @param Conly Boolean: Is the model meant for carbon only?
#' @param densitydependence Which nodes have density dependence? NA default means none of them do. Should be a vector of 0 (no DD) and 1 (DD) for each node.
#' @param functionalresponse The type of functional response to be used in the model simulation. Either NA to signify a Type I functional response or a matrix of values of the handling time to use a Type II functional response.
#' @param returnnet A Boolean to determine if the goal is to return the net change in the food web. Used to check equilibrium.
#' @return A list with two elements: (1) a vector of parameters to run the model away from equilibrium and (2) a vector of equilibrium biomasses that can be modified and passed to the simulator.
#' @details
#' A function to get the parameters of a food web model for simulation purposes. It does not correct stoichiometry, so the user must do this beforehand if they want.
#' @examples
#' # Basic call.
#' getPARAMS(intro_comm)
#' @export
getPARAMS <- function(usin,
                      DIETLIMITS = NA,
                      diet_correct = TRUE,
                      Conly = FALSE,
                      densitydependence = NA,
                      functionalresponse = NA,
                      returnnet = FALSE){

  # Set the diet limits if they are not included
  if(any(is.na(DIETLIMITS))){
    DIETLIMITS = usin$imat
    DIETLIMITS[DIETLIMITS > 0] = 1
  }


  # If the model has other nutrients, correct stoichiometry:
  if(!Conly){
    # If the model allows diet correction, start there:
    if(diet_correct){
      usin = correct_diet(usin, dietlimits = DIETLIMITS)
    }

    usin = correct_respiration(usin, output_type = F)[[1]]
  }

  Nnodes = dim(usin$imat)[1] # Number of nodes
  Nelements = length(usin$prop$assimilation) # Number of elements

  # Check to make sure the density-dependence vector is the right length or make a single value a vector of the right length
  if(any(is.na(densitydependence))){
    densitydependence = rep(0, Nnodes)}

  stopifnot(length(densitydependence) == Nnodes)

  death = cbind(d = usin$prop$general$Carbon$d,
                dd = usin$prop$general$Carbon$d/usin$prop$general$Carbon$B,
                densitydependence = densitydependence)

  if(is.na(functionalresponse)){
    cij = Cijfcn(usin) # Get the consumption rate matrix for the base parameters (units 1/ (gC * time))
  }else{
    cij = Cijfcn(usin, h = functionalresponse)
  }

  if(any(is.na(functionalresponse))){
    hmat = usin$imat
    hmat[hmat > 0] = 0
  }else{
    hmat = functionalresponse
  }

  # Get the vector of equilibrium biomass:
  eqm_biomass = usin$prop$general$Carbon$B

  # Get the matrix of Q parameters:
  Qmat = sapply(usin$prop$general, FUN = function(X){X$Q})

  eqm_biomass = matrix(eqm_biomass, nrow = nrow(Qmat), ncol = ncol(Qmat))

  eqm_biomass = eqm_biomass*sweep(Qmat, 1, Qmat[, 1], "/")

  eqm_biomass = rbind(eqm_biomass)

  rownames(eqm_biomass) = rownames(usin$imat)

  # Flatten the matrix and set names
  combined_vector <- as.vector(eqm_biomass)
  names(combined_vector) <- outer(rownames(eqm_biomass), colnames(eqm_biomass), paste, sep = "_")
  eqm_biomass = combined_vector; rm(combined_vector)

  # Get the matrix of p parameters:
  pmat = sapply(usin$prop$general, FUN = function(X){X$p})

  # Get assimilation parameters:
  assimilation = usin$prop$assimilation

  # Get detritus parameters:
  detplant = usin$prop$general$Carbon[, c("DetritusRecycling", "isDetritus", "isPlant")]

  # Immobilization parameters:
  canIMMmat = sapply(usin$prop$general, FUN = function(X){X$canIMM})

  # Get E parameters: (NOT IN USE ATM)
  biomass_exrete = (do.call("cbind",comana(usin)$mineralization) -
    (sapply(usin$prop$general, FUN = function(X){X$p})-1)*sapply(Map(function(x,y) x*y, comana(usin)$fmat, usin$prop$assimilation), rowSums))

  # Confirm net change:
  if(sum(detplant$DetritusRecycling ) > 1) stop("DetritusRecycling must sum to 1")

  # Gains from consumption:
  net = pmat*sapply(Map(function(x,y) x*y, comana(usin)$fmat, usin$prop$assimilation), rowSums) -
    # Losses from predation:
    sapply(comana(usin)$fmat, colSums) -
    # Natural death:
    matrix(usin$prop$general$Carbon$d*usin$prop$general$Carbon$B, nrow = nrow(Qmat), ncol = ncol(Qmat))*sweep(Qmat, 1, Qmat[, 1], "/") -
    # Excretion:
    biomass_exrete +

    # Detritus recycling:
    matrix(detplant$DetritusRecycling, nrow = nrow(Qmat), ncol = ncol(Qmat))* # A matrix to allocate the detritus recycling appropriately
    matrix(
      colSums(sapply(Map(function(x,y) x*y, comana(usin)$fmat, lapply(usin$prop$assimilation, function(X) 1 - X)), rowSums)) + # A vector of unassimilated material (i.e., faeces)
             colSums(matrix(usin$prop$general$Carbon$d*usin$prop$general$Carbon$B, nrow = nrow(Qmat), ncol = ncol(Qmat))*sweep(Qmat, 1, Qmat[, 1], "/")) + # A vector of carcases
        colSums(biomass_exrete), # A vector of mineralization rates also back into the detritus pool
      nrow = nrow(Qmat), ncol = ncol(Qmat),byrow = T) # arrange in a matrix by row so that the elements are in the columns.

  if(returnnet){
    return(net)
  }else{
    return(list(yeqm = eqm_biomass,
                parameters =
                  list(cij = cij,
                       h = hmat,
                       death = death,
                       pmat = pmat,
                       assimilation = assimilation,
                       detplant = detplant,
                       canIMMmat = canIMMmat,
                       ECarbon = usin$prop$general$Carbon$E)))
  }
}
