#' A function to create data tables for food web inputs
#'
#' @param nodenames A vector of node names.
#' @param elements A vector of elements used in the model.
#' @return A list of data tables for feeding matrices.
#' @export

build_foodweb_inputs <- function(nodenames, elements){

  # Build the input feedinglist:
  feedinglist = expand.grid(Predator = nodenames, Prey = nodenames)

  feedinglist[,"Preference"] = 0

  for(el in elements){
    feedinglist[,paste0("a", el)] = 1
  }


  properties = expand.grid(ID = nodenames, Element = "Carbon", Parameter = c("E", "Q", "d", "B", "canIMM", "isDetritus", "isPlant", "FecalRecycling", "NecromassRecycling"), Value = 0)

  for(el in elements[-1]){
    toadd = expand.grid(ID = nodenames, Element = el, Parameter = c("Q", "canIMM"), Value = 0)
    properties = rbind(properties, toadd)

  }

  return(list(feedinglist = feedinglist, properties = properties))

}
