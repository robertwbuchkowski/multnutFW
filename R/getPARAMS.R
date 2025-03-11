#' A function to get the parameters for a food web model.
#'
#' @param usin The community you want to simulate.
#' @param DIETLIMITS The diet limits matrix for the stoichiometry correction (proportion of diet)?
#' @param diet_correct Boolean: Does the organism correct it's diet?
#' @param Conly Boolean: Is the model meant for carbon only?
#' @param densitydependence Which nodes have density dependence? NA default means none of them do. Should be a vector of 0 (no DD) and 1 (DD) for each node.
#' @param functionalresponse The type of functional response to be used in the model simulation. Either NA to signify a Type I functional response or a matrix of values of the handling time to use a Type II functional response.
#' @param inorganic_eqm The amount of each chemical element at equilibrium in the same units as node biomass in the community.
#' @param inorganicexport A vector with the export rate for each chemical element in inorganic form. Default NA means zero. Carbon is meaningless because it is not available for uptake.
#' @param modelinputs A vector of inputs for each node in the food web at the same rate.
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
                      inorganic_eqm = NA,
                      inorganicexport = NA,
                      modelinputs = NA){

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
  Nelements = length(usin$prop$assimilation) # Number of elements

  # Check to make sure the density-dependence vector is the right length or make a single value a vector of the right length
  if(any(is.na(densitydependence))){
    densitydependence = rep(0, Nnodes)}

  stopifnot(length(densitydependence) == Nnodes)

  # Check to make sure the inorganic equilibrium vector is the right length or make a single value a vector of the right length
  if(any(is.na(inorganic_eqm))){
    inorganic_eqm = rep(0, Nelements)
  }

  stopifnot(length(inorganic_eqm) == Nelements)

  # Check to make sure the inorganic export vector is the right length or make a single value a vector of the right length
  if(any(is.na(inorganicexport))){
    inorganicexport = rep(0, Nelements)
  }

  stopifnot(length(inorganicexport) == Nelements)

  # Check to make sure the model inputs vector is the right length or make a single value a vector of the right length
  if(any(is.na(modelinputs))){
    modelinputs = rep(0, Nnodes)}

  stopifnot(length(modelinputs) == Nnodes)

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

  # names(eqm_biomass) = c(colnames(usin$imat), "Inorganic")

  # Get the matrix of Q parameters:
  Qmat = sapply(usin$prop$general, FUN = function(X){X$Q})

  eqm_biomass = matrix(eqm_biomass, nrow = nrow(Qmat), ncol = ncol(Qmat))

  eqm_biomass = eqm_biomass*sweep(Qmat, 1, Qmat[, 1], "/")

  eqm_biomass = rbind(eqm_biomass, inorganic_eqm)

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
  canIMMmat = rbind(sapply(usin$prop$general, FUN = function(X){X$canIMM}), rep(0, Nelements))

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
             colSums(matrix(usin$prop$general$Carbon$d*usin$prop$general$Carbon$B, nrow = nrow(Qmat), ncol = ncol(Qmat))*sweep(Qmat, 1, Qmat[, 1], "/")), # A vector of carcases
      nrow = nrow(Qmat), ncol = ncol(Qmat), byrow = T) # arrange in a matrix by row so that the elements are in the columns.

  # Reset small fluxes to zero:
  net[abs(net) < 1e-12] = 0

  # Add in the inorganic gains/losses
  net = rbind(net, colSums(biomass_exrete))

  rownames(net) = c(rownames(biomass_exrete), "Inorganic")

  # Add in inorganic losses
  net[dim(net)[1],]  = net[dim(net)[1],] - inorganicexport*inorganic_eqm

  # Add the detritus inputs:
  # browser()
  net = net + rbind(matrix(modelinputs, nrow = nrow(Qmat), ncol = ncol(Qmat))*Qmat, rep(0, Nelements))


  # Add in inorganic N
  # Internal function to add inorganic to matrix
  add_inorganic <- function(mat, val = 0) {
    # Add a row of zeros
    mat <- rbind(mat, Inorganic = rep(val, ncol(mat)))

    # Add a column of zeros
    mat <- cbind(mat, Inorganic = rep(val, nrow(mat)))

    return(mat)
  }

  cij = add_inorganic(cij)

  hmat = add_inorganic(hmat)
  rownames(hmat) = rownames(cij)

  pmat = add_inorganic(pmat, val = 1)
  rownames(pmat) = rownames(cij)

  detplant = rbind(detplant, c(0,0,0))
  rownames(detplant) = rownames(cij)

  assimilation = lapply(assimilation, add_inorganic, val = 1)

  death = rbind(death, c(0,0,0))
  rownames(death) = rownames(cij)

  rownames(canIMMmat) = rownames(cij)

  ECarbon = c(usin$prop$general$Carbon$E, 0)
  names(ECarbon) = rownames(cij)

  # browser()

  # NEXT STEP IS TO CALCULATE THE NET CHANGES AND INPUT RATES. ALSO NEED TO DEAL WITH DETRITUS.

  return(list(yeqm = eqm_biomass,
              parameters =
                list(cij = cij,
                     h = hmat,
                     death = death,
                     pmat = pmat,
                     assimilation = assimilation,
                     detplant = detplant,
                     canIMMmat = canIMMmat,
                     ECarbon = ECarbon,
                     inorganicexport = inorganicexport),
              net = net))
}
