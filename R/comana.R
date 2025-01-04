#' A function to calculate carbon and nitrogen fluxes in the food web.
#'
#' @param  usin The community that you are analyzing: contains a matrix of interactions and a data frame of properties in a list.
#' @param shuffleTL A Boolean stating whether the community should be sorted.
#' @param rmzeros A Boolean determining whether trophic species with zero biomass should be removed from the community before analysis.
#' @param eqmtolerance A value used to set the equilibrium tolerance for the food web verification. If NA, the default value used by the function all.equal is used, which is approximately 1.5e-8.
#' @return A list of consumption rates, carbon mineralization, nitrogen mineralization, carbon and nitrogen consumption rates, and the modified community if zeros where removed or sorting occurred.
#' @examples
#' comana(intro_comm)
#' @export

comana <- function(usin,
                   shuffleTL = FALSE,
                   rmzeros = TRUE,
                   eqmtolerance = NA
){

  # Separate the imat and prop:
  imat = usin$imat # row values of imat sets predator feeding preferences!
  prop = usin$prop # properties of each trophic species
  Nnodes = dim(imat)[1] # Number of nodes in the food web

  # Create a vector for the consumption rates
  temp_mat =
    -1*t(imat)*matrix(prop$Carbon$B, nrow = Nnodes, ncol = Nnodes)/matrix(rowSums(imat*matrix(prop$Carbon$B, nrow = Nnodes, ncol = Nnodes, byrow = T)), nrow = Nnodes, ncol = Nnodes, byrow = T)

  temp_mat[!is.finite(temp_mat)] = 0 # Replace non-finite values with 0 because total consumption was zero in this case

  diag(temp_mat) = prop$Carbon$a*prop$Carbon$p + diag(temp_mat) # Add in a*p term

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
  fmat[[1]] = (imat*matrix(prop$Carbon$B, nrow = Nnodes, ncol = Nnodes, byrow=TRUE)/rowSums(imat*matrix(prop$Carbon$B, nrow = Nnodes, ncol = Nnodes, byrow=TRUE)))*consumption

  fmat[[1]][!is.finite(fmat[[1]])] = 0 # Replace NaN with 0 for no feeding

  # Fix the detritus calculations: detritus receives dead material from all other trophic levels and so consumption is the losses minus the inputs it already gets from inefficient eating and dead biomass. This value can be negative indicating that inputs from outside the ecosystem are necessary to meet internal demand for C.
  if(any(prop$Carbon$isDetritus >0)){
    detritusPOS = which(prop$Carbon$isDetritus >0)

    for(i in detritusPOS){
      consumption[i] = sum(fmat[[1]][,i]) - prop$Carbon$DetritusRecycling[i]*(sum((1-prop$Carbon$a)*consumption) + sum(prop$Carbon$d*prop$B))
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

    Qhat = prop$Carbon$Q/current_element_properties$Q

    AIJ[[i]] = matrix(Qhat, nrow = Nnodes, ncol = Nnodes)* # Predator C:X ratio
                  matrix(current_element_properties$a*current_element_properties$p, nrow = Nnodes, ncol = Nnodes)/ # Predator X assimilation and production rates
                  matrix(Qhat, nrow = Nnodes, ncol = Nnodes, byrow = T) - # Prey C:X ratio
                  matrix(prop$Carbon$a*prop$Carbon$p, nrow = Nnodes, ncol = Nnodes) # Predator C assimilation and production rates
  }

  # Calculate carbon mineralization using the production efficiency
  mineralization[[1]] = prop$Carbon$a*(1-prop$Carbon$p)*consumption + prop$Carbon$E*prop$Carbon$B + prop$Carbon$Ehat*prop$Carbon$B

  # Calculate the mineralization rates of the various elements using the comparison to carbon:

  for(i in 2:length(element_list)) {
    current_element_properties = prop[[which(names(prop) == element_list[i])]]

    Qhat = prop$Carbon$Q/current_element_properties$Q

    mineralization[[i]] = (1-current_element_properties$p)*current_element_properties$a*rowSums(fmat[[i]]) + (prop$Carbon$E*prop$Carbon$B + prop$Carbon$Ehat*prop$Carbon$B + # Carbon mineralization rate based on a fixed proportion of biomass
                             rowSums((AIJ[[i]])* # Net element gain from feeding
                                       fmat$Carbon))* # consumption rates
      Qhat* # multiply by C:X ratio to get back to units of X
      as.numeric(rowSums(imat)>0) # Make X mineralization zero for all nodes without prey items.
  }

  return(list(fmat = fmat, consumption = consumption, AIJ = AIJ, mineralization = mineralization))
}
