#' A function to get the parameters for a food web model.
#'
#' @param usin The community you want to simulate.
#' @param DIETLIMITS The diet limits matrix for the stoichiometry correction (proportion of diet)?
#' @param diet_correct Boolean: Does the organism correct it's diet?
#' @param Conly Boolean: Is the model meant for carbon only?
#' @param densitydependence Which nodes have density dependence? NA default means none of them do. Should be a vector of 0 (no DD) and 1 (DD) for each node.
#' @param functionalresponse The type of functional response to be used in the model simulation. Either NA to signify a Type I functional response or a vector of values of the handling time to use a Type II functional response.
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
                      functionalresponse = NA){

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

    usin = correct_respiration(usin)
  }

  Nnodes = dim(usin$imat)[1] # Number of nodes

  # Check to make sure the density-dependence vector is the right length or make a single value a vector of the right length
  if(any(is.na(densitydependence))){
    densitydependence = rep(0, Nnodes)
  }else{
    stopifnot(length(densitydependence) == Nnodes)
  }

  death = cbind(d = usin$prop$general$Carbon$d,
                dd = usin$prop$general$Carbon$d/usin$prop$general$Carbon$B,
                densitydependence = densitydependence)

  if(is.na(functionalresponse)){
    cij = Cijfcn(usin) # Get the consumption rate matrix for the base parameters (units 1/ (gC * time))
  }else{
    cij = Cijfcn(usin, h = functionalresponse)
  }

  # Get the vector of equilibrium biomass:
  eqm_biomass = usin$prop$general$Carbon$B

  # names(eqm_biomass) = c(colnames(usin$imat), "Inorganic")

  # Get the matrix of Q parameters:
  Qmat = sapply(usin$prop$general, FUN = function(X){X$Q})

  eqm_biomass = matrix(eqm_biomass, nrow = nrow(Qmat), ncol = ncol(Qmat))

  eqm_biomass = eqm_biomass*sweep(Qmat, 1, Qmat[, 1], "/")

  eqm_biomass = rbind(eqm_biomass, rep(0, ncol(eqm_biomass)))

  rownames(eqm_biomass) = c(rownames(usin$imat), "Inorganic")

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

  # Don't both zeroing out for this.
  # biomass_exrete[abs(biomass_exrete) < 1e-10] <- 0
  #
  # Emat = biomass_exrete/matrix(usin$prop$general$Carbon$B, nrow = Nnodes, ncol = length(usin$prop$general))
  #
  # rm(biomass_exrete)

  # try(if(!any(usin$prop$general$Carbon$E - unname(Emat[,1]) > 1e-10)) stop("Error in calculating excretion rate for carbon."))

  # Calculate input rates:

  # browser()

  # Gains from consumption:
  net = pmat*sapply(Map(function(x,y) x*y, comana(usin)$fmat, usin$prop$assimilation), rowSums) -
    # Losses from predation:
    sapply(comana(usin)$fmat, colSums) -
    # Natural death:
    matrix(usin$prop$general$Carbon$d*usin$prop$general$Carbon$B, nrow = nrow(Qmat), ncol = ncol(Qmat))*sweep(Qmat, 1, Qmat[, 1], "/") -
    # Excretion:
    biomass_exrete

  # NEXT STEPS HERE ARE TO ADD RECYCLING DETRITUS AND ALSO USE biomass_exrete TO DETERMINE THE IMMOBILIZATION OF ALL RELEVANT ELEMENTS.

  browser()

  return(list(yeqm = eqm_biomass,
              parameters =
                list(cij = cij,
                     h = functionalresponse,
                     death = death,
                     pmat = pmat,
                     assimilation = assimilation,
                     detplant = detplant,
                     canIMMmat = canIMMmat,
                     ECarbon = usin$prop$general$Carbon$E),
              net = net))
}
