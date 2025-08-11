#' A function to convert between respiration by biomass carbon (E) and consumption (p).
#'
#' @param  usin The community that you are analyzing: contains a matrix of interactions and a data frame of properties in a list.
#' @return The community where E is now zero and p is not 1.
#' @examples
#' E_to_p(intro_comm)
#' @export

E_to_p <- function(usin){

  # Calculate the fluxes for use in converting E to p:
  coutput = comana(usin)

  # Set E to zero in the community:
  usin$prop$general$Carbon$E = 0

  # Solve for p by rearranging the carbon equation for each species:
  pout = (usin$prop$general$Carbon$d*usin$prop$general$Carbon$B + usin$prop$general$Carbon$E*usin$prop$general$Carbon$B + usin$prop$general$Carbon$Ehat*usin$prop$general$Carbon$B + colSums(coutput$fmat$Carbon))/rowSums(usin$prop$assimilation$Carbon*coutput$fmat$Carbon)

  # Replace p for basal organisms that are not detritus with respiration terms:
  BO = unname(which(TLcheddar(usin$imat) == 1 & usin$prop$general$Carbon$isDetritus == 0))
  for(i in BO){
    pout[i] = c((usin$prop$general$Carbon$d*usin$prop$general$Carbon$B + usin$prop$general$Carbon$E*usin$prop$general$Carbon$B + usin$prop$general$Carbon$Ehat*usin$prop$general$Carbon$B + colSums(coutput$fmat$Carbon))/coutput$consumption)[i]
  }

  # Remove infinity that occurs for nodes without food sources:
  pout = ifelse(is.infinite(pout), 1,pout)

  # Add to the community:
  usin$prop$general$Carbon$p = pout

  return(usin)
}
