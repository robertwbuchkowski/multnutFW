#' A function to calculate carbon and nitrogen fluxes in the food web.
#'
#' @param  usin The community that you are analyzing: contains a matrix of interactions and a data frame of properties in a list. Make biomass_weighted feeding and stoichiometric correction first!
#' @return The same community returned with the necromass and feces separated out from the detritus pools into which they original went.
#' @examples
#' sep_necro_feces(intro_comm)
#' @export

sep_necro_feces <- function(usin){

  testcomm = correct_respiration(intro_comm2)

  fluxes = comana(testcomm, biomass_weight_preference = F)

  # Get fecal and necromass fluxes:
  elements = names(testcomm$prop$general)

  fe = rep(NA, length(elements))
  ne = fe
  # Get the elemental fluxes:
  for(el in 1:length(elements)){
    fe[el] = sum(testcomm$prop$general$Carbon$d * testcomm$prop$general$Carbon$B * 1/testcomm$prop$general$Carbon$Q * testcomm$prop$general[[elements[el]]]$Q)

    ne[el] = sum((1-testcomm$prop$assimilation[[elements[el]]]) * fluxes$fmat[[elements[el]]])
  }



  return(usin)
}
