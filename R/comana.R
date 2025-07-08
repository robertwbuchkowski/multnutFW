#' A function to calculate carbon and nitrogen fluxes in the food web.
#'
#' @param  usin The community that you are analyzing: contains a matrix of interactions and a data frame of properties in a list.
#' @param biomass_weight_preference Should the preference matrix be weighted by biomass inside this function? Default, False, assumes that you have already done this with the function biomass_weight_preferences or don't want to weight by biomass.
#' @return A list of consumption rates, carbon mineralization, nitrogen mineralization, carbon and nitrogen consumption rates, and the modified community if zeros where removed or sorting occurred.
#' @examples
#' comana(intro_comm)
#' @export

comana <- function(usin, biomass_weight_preference = FALSE){

  # Weight preferences if needed:
  if(biomass_weight_preference){
    usin = biomass_weight_preferences(usin)
  }

  # Separate the imat and prop:
  imat = usin$imat # row values of imat sets predator feeding preferences!
  prop = usin$prop$general # properties of each trophic species
  assim = usin$prop$assimilation # the assimilation efficiencies
  Nnodes = dim(imat)[1] # Number of nodes in the food web

  # Confirm that all rows of imat sum to 1:
  if(!all(rowSums(imat) %in% c(0,1))){
    warning("All rows of imat must sum to 1 or 0, so feeding preferences are being rescaled.")

    imat = sweep(imat, 1, rowSums(imat), FUN = "/")
    imat[!is.finite(imat)] = 0 # Replace non-finite values with 0 because total consumption was zero in this case

  }

  # Input the preference matrix:
  temp_mat = -1*t(imat)

  temp_mat[!is.finite(temp_mat)] = 0 # Replace non-finite values with 0 because total consumption was zero in this case

  # Save the preference matrix:
  preference_matrix = -1*t(temp_mat)

  # Calculate the vector weighted assimilation efficiencies:
  assimpref = rowSums(assim$Carbon*preference_matrix)

  # Replace any zeros with 1, because they mean no assimilation but are necessary for a solution:
  assimpref[assimpref == 0] = 1

  # Create a vector for the consumption rates
  diag(temp_mat) = prop$Carbon$p*prop$Carbon$assimhat*assimpref + diag(temp_mat) # Add in production and assimilation efficiency terms on the diagonal.

  consumption = base::solve(temp_mat,(prop$Carbon$d*prop$Carbon$B + prop$Carbon$E*prop$Carbon$B + prop$Carbon$Ehat*prop$Carbon$B))

  # Confirm that this solution is unique by showing Ax = 0 produces x = 0
  if(any(solve(temp_mat,rep(0, Nnodes)) != 0)){
    warning("Solution to the web is not unique!")
  }

  names(consumption) = colnames(imat) # Names match the trophic species names

  element_list = names(prop)

  # Create an fmat vector
  fmat = vector(mode = "list", length = length(element_list))
  names(fmat) = element_list

  # Create a vector for AIJ:
  AIJ = fmat

  # Create a mineralization vector
  mineralization = fmat

  # Create a new matrix for feeding rates with the same dimensions as the food web matrix
  fmat[[1]] = imat*consumption

  fmat[[1]][!is.finite(fmat[[1]])] = 0 # Replace NaN with 0 for no feeding

  # Fix the detritus calculations: detritus receives dead material and fecaes from all other trophic levels and so consumption is the losses minus the inputs it already gets from inefficient eating and dead biomass. This value can be negative indicating that inputs from outside the ecosystem are necessary to meet internal demand for C.
  if(any(prop$Carbon$isDetritus >0)){
    detritusPOS = which(prop$Carbon$isDetritus >0)

    for(i in detritusPOS){
      consumption[i] = sum(fmat[[1]][,i]) - prop$Carbon$FecalRecycling[i]*(sum((1-(prop$Carbon$assimhat*assim$Carbon))*fmat[[1]]) + prop$Carbon$NecromassRecycling[i]*sum(prop$Carbon$d*prop$Carbon$B))
    }
  }


  # Calculating nutrient fluxes, fmat, for other elements:
  for(i in 2:length(element_list)) {
    current_element_properties = prop[[which(names(prop) == element_list[i])]]

    Qhat = prop$Carbon$Q/current_element_properties$Q

    fmat[[i]] = fmat[[1]]/matrix(Qhat, nrow = Nnodes, ncol = Nnodes, byrow = T)
  }

  # Calculate the AIJ matrices:
  for(i in 2:length(element_list)) {
    current_element_properties = prop[[which(names(prop) == element_list[i])]]

    current_element_assimilation = assim[[which(names(prop) == element_list[i])]]

    Qhat = prop$Carbon$Q/current_element_properties$Q

    AIJ[[i]] = matrix(Qhat, nrow = Nnodes, ncol = Nnodes)* # Predator C:X ratio
      current_element_assimilation*
                  matrix(current_element_properties$p, nrow = Nnodes, ncol = Nnodes)/ # Predator X assimilation and production rates
                  matrix(Qhat, nrow = Nnodes, ncol = Nnodes, byrow = T) - # Prey C:X ratio
                  assim$Carbon*matrix(prop$Carbon$p*prop$Carbon$assimhat, nrow = Nnodes, ncol = Nnodes) # Predator C assimilation and production rates
  }

  # Calculate carbon mineralization using the production efficiency
  mineralization[[1]] = (1-prop$Carbon$p)*prop$Carbon$assimhat*rowSums(assim$Carbon*fmat$Carbon) + prop$Carbon$E*prop$Carbon$B + prop$Carbon$Ehat*prop$Carbon$B

  # Calculate the mineralization rates of the various elements using the comparison to carbon:

  for(i in 2:length(element_list)) {
    current_element_properties = prop[[which(names(prop) == element_list[i])]]

    current_element_assimilation = assim[[which(names(prop) == element_list[i])]]

    Qhat = prop$Carbon$Q/current_element_properties$Q

    mineralization[[i]] = (1-current_element_properties$p)*rowSums(current_element_assimilation*fmat[[i]]) + (prop$Carbon$E*prop$Carbon$B + prop$Carbon$Ehat*prop$Carbon$B + # Carbon mineralization rate based on a fixed proportion of biomass
                             rowSums((AIJ[[i]])* # Net element gain from feeding
                                       fmat$Carbon))/ # consumption rates
      Qhat* # divide by C:X ratio to get back to units of X
      as.numeric(rowSums(imat)>0) # Make X mineralization zero for all nodes without prey items.
  }

  return(list(fmat = fmat, consumption = consumption, AIJ = AIJ, mineralization = mineralization))
}
