#' A function to calculate carbon and nitrogen fluxes in the food web.
#'
#' @param  usin The community that you are analyzing: contains a matrix of interactions and a data frame of properties in a list.
#' @return The community that you are analyzing with the biomass weighted preferences includes in the interaction matrix instead of the raw unweighted preferences.
#' @examples
#' biomass_weight_preferences(intro_comm)
#' @export

biomass_weight_preferences <- function(usin){

  # Separate the imat and prop:
  imat = usin$imat # row values of imat sets predator feeding preferences!
  prop = usin$prop$general # properties of each trophic species
  assim = usin$prop$assimilation # the assimilation efficiencies
  Nnodes = dim(imat)[1] # Number of nodes in the food web

  # Calculate the preference matrix using biomass re-scaling:
  temp_mat = t(imat)*matrix(prop$Carbon$B, nrow = Nnodes, ncol = Nnodes)/matrix(rowSums(imat*matrix(prop$Carbon$B, nrow = Nnodes, ncol = Nnodes, byrow = T)), nrow = Nnodes, ncol = Nnodes, byrow = T)

  temp_mat[!is.finite(temp_mat)] = 0 # Replace non-finite values with 0 because total consumption was zero in this case

  # Replace the input matrix with the biomass weighted preferences matrix.
  usin$imat = t(temp_mat)

  return(usin)
}
