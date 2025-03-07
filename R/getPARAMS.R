#' A function to get the parameters for a food web model.
#'
#' @param usin The community you want to simulate.
#' @param DIETLIMTS The diet limits matrix for the stoichiometry correction (proportion of diet)?
#' @param diet_correct Boolean: Does the organism correct it's diet?
#' @param Conly Boolean: Is the model meant for carbon only?
#' @param densitydependence Which nodes have density dependence? NA default means none of them do. Should be a vector of 0 (no DD) and 1 (DD) for each node.
#' @param functionalresponse The type of functional response to be used in the model simulation. Either NA to signify a Type I functional response or a vector of values of the half saturation coefficient to signify a Type II functional response.
#' @return A list with two elements: (1) a vector of parameters to run the model away from equilibrium and (2) a vector of starting biomasses.
#' @details
#' A function to get the parameters of a food web model for simulation purposes. It does not correct stoichiometry, so the user must do this beforehand if they want.
#' @examples
#' # Basic call.
#' getPARAMS(intro_comm)
#' @export
getPARAMS <- function(usin,
                      DIETLIMTS = NA,
                      diet_correct = TRUE,
                      Conly = FALSE,
                      densitydependence = NA,
                      functionalresponse = NA){

  # Set the diet limits if they are not included
  if(any(is.na(DIETLIMTS))){
    DIETLIMTS = usin$imat
    DIETLIMTS[DIETLIMTS > 0] = 1
  }


  # If the model has other nutrients, correct stoichiometry:
  if(!Conly){
    # If the model allows diet correction, start there:
    if(diet_correct){
      usin = correct_diet(usin)
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

  if(is.na(functionalresponse)){
    cij = Cijfcn(usin) # Get the consumption rate matrix for the base parameters (units 1/ (gC * time))
  }else{
    cij = Cijfcn(usin, K = functionalresponse)
  }

  # Save the parameter values:
  FMAT = comana(usin)$fmat$Carbon
  p = usin$prop$general$Carbon$p
  a = usin$prop$a

  # I CAN LEAVE ALL THE MATRICIES AS IS AND INPUT DIRECTLY INTO the ode FUNCTION!!

  # Death rate for each node is set my densitydependence. 1 means density dependent, 0 means density independent.
  dd = densitydependence*usin$prop$d/usin$prop$B
  d = (1-densitydependence)*usin$prop$d

  # Get values from the community that are needed in the list of parameters:
  B = usin$prop$B # Equilibrium biomass
  isDetritus = usin$prop$isDetritus # Detritus ID
  DetritusRecycling = usin$prop$DetritusRecycling # Detritus Recycling
  isPlant = usin$prop$isPlant # Plant ID
  canIMM = usin$prop$canIMM # Can the node immobilize N?

  # Rescale DetritusRecycling to sum to 1
  if(any(isDetritus > 0)){
    DetritusRecycling = DetritusRecycling/sum(DetritusRecycling)
  }

  # Calculate IN parameters for equilibrium
  IN = rep(0, length(B))
  delta = a*p*rowSums(FMAT) - colSums(FMAT) - (densitydependence*d*B*B + (1-densitydependence)*d*B) # This calculates the extra C needed to be input to keep the change over time zero when the equilibrium biomass (B) is put into the differential equation

  # Fix the delta term for detritus, because detritus receives inputs from within the food web as necromass and excreta
  delta[isDetritus>0] =
    delta[isDetritus>0] +
    DetritusRecycling[isDetritus>0]*
    sum(c((1-a)*rowSums(FMAT),
          densitydependence*d*B*B + (1-densitydependence)*d*B))
  IN[rowSums(FMAT)==0] = -1*delta[rowSums(FMAT)==0]

  # Modify inputs for plants to make them per biomass if there is no inorganic nitrogen:
  cpn = r_i = rep(0, length(B)) # Create a new parameter class r_P for growth of a pool based on it's own biomass gain = r_i*X_i. Also create a vector cpn for inorganic nitrogen uptake rates if necessary. Used for plants.
  if(any(isPlant>0 & isFALSE(has_inorganic_nitrogen))){
    r_i[isPlant>0] = IN[isPlant>0]/B[isPlant >0]
    IN[isPlant>0] = 0
  }

  # If inorganic nitrogen is included, then calculate the amount available and the parameters at equilibrium:
  if(has_inorganic_nitrogen){

    if(sum(is.na(inorganic_nitrogen_properties)) > 1) stop("You must define two of the three parameters q, INN, and eqmN to solve the system.")

    if(any(isPlant>0)){
      IN_plant = sum(IN[isPlant >0]/CN[isPlant>0]) # Plant N uptake
    }else{
      IN_plant = 0 # Zero if there are no plants
    }

    # Get the inorganic nitrogen parameters (two of the possible variables must be values):
    Nmin = sum(comana(usin, shuffleTL = F)$Nminmat)
    INN = inorganic_nitrogen_properties$INN
    if(!is.na(INN)){
      if(INN < 0) warning("INN < 0 is a constant loss rate of inorganic nitrogen.")
    }
    q = inorganic_nitrogen_properties$q
    if(!is.na(q)){if(q < 0) stop("q should be positive")}
    eqmN = inorganic_nitrogen_properties$eqmN
    if(!is.na(eqmN)){if(eqmN < 0) stop("eqmN (equilibrium nitrogen) should be positive")}

    # Depending on the variable that is unknown, the if statements below choose the correct formula to calculate the third parameter.
    if(is.na(INN)){
      INN = IN_plant + q*eqmN - Nmin
    }

    if(is.na(q)){
      q = (INN - IN_plant + Nmin)/eqmN
    }

    if(is.na(eqmN)){
      eqmN = (INN - IN_plant + Nmin)/q
    }

    if(verbose){
      print(paste0("Nitrogen parameters are: INN= ", INN, ", q= ", q, ", eqmN= ", eqmN))
    }

    # Get the plant coefficients cpn (cpn is indexed in units of plant carbon):
    cpn[isPlant > 0] = (IN[isPlant >0]/CN[isPlant>0])/(B[isPlant >0]*eqmN)
    IN[isPlant >0] = 0

    plant_N_growth = cpn*B*eqmN
  }else{
    plant_N_growth = rep(0,length(B))
    INN = inorganic_nitrogen_properties$INN
    q = inorganic_nitrogen_properties$q
  }

  # Add back in the system inputs
  delta = delta + IN + r_i*B + plant_N_growth*CN

  if(max(abs(delta)) > 1e-5) warning("Initial equilibrium has error rate larger than 1e-5. Run a test simulation with no modifications to ensure that the equilibrium is stable.")

  # Output the parameter list
  PARS = c(Nnodes,d,a,pin,B,CN,isDetritus, isPlant,DetritusRecycling, canIMM, densitydependence,cij,DIETLIMTS, IN, r_i, sum(diet_correct), sum(Conly), has_inorganic_nitrogen, cpn,INN,q, NA)

  # Add the names to the parameter list
  names(PARS) = c("Nnodes",
                  paste0("d_", 1:Nnodes),
                  paste0("a_", 1:Nnodes),
                  paste0("p_", 1:Nnodes),
                  paste0("B_", 1:Nnodes),
                  paste0("CN_", 1:Nnodes),
                  paste0("isDetritus_", 1:Nnodes),
                  paste0("isPlant_", 1:Nnodes),
                  paste0("DetritusRecycling_", 1:Nnodes),
                  paste0("canIMM_", 1:Nnodes),
                  paste0("densitydependence_", 1:Nnodes),
                  paste0("c_", rep(1:Nnodes, each = Nnodes), rep(1:Nnodes, Nnodes)),
                  paste0("DIETLIMITS_", rep(1:Nnodes, each = Nnodes), rep(1:Nnodes, Nnodes)),
                  paste0("IN_", 1:Nnodes),
                  paste0("r_", 1:Nnodes),
                  "diet_correct",
                  "Conly",
                  "hasIORGN",
                  paste0("cpn_", 1:Nnodes),
                  "INN",
                  "q",
                  "DetExpt")


  # Return the state variables and the parameters:
  return(list(PARS = PARS, yint = yint))
}
