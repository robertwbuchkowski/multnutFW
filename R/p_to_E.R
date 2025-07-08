#' A function to convert between respiration by consumption (p) and biomass carbon (E).
#'
#' @param  usin The community that you are analyzing: contains a matrix of interactions and a data frame of properties in a list.
#' @return The community where p is now 1 and E is not 0.
#' @examples
#' p_to_E(E_to_p(intro_comm))
#' @export

p_to_E <- function(usin){

  # Calculate the fluxes for use in converting p to E:
  coutput = comana(usin)

  # Set p to zero in the community:
  usin$prop$general$Carbon$p = 1

  # Solve for p by rearranging the carbon equation for each species:
  Eout = (usin$prop$general$Carbon$p*rowSums(usin$prop$assimilation$Carbon*coutput$fmat$Carbon) -  usin$prop$general$Carbon$d*usin$prop$general$Carbon$B -usin$prop$general$Carbon$Ehat*usin$prop$general$Carbon$B - colSums(coutput$fmat$Carbon))/usin$prop$general$Carbon$B

  # No respiration for detritus:
  Eout = ifelse(usin$prop$general$Carbon$isDetritus == 1, 0, Eout)

  # Add to the community:
  usin$prop$general$Carbon$E = Eout

  return(usin)
}
