#' Direct and indirect contributions to mineralizations
#'
#' @param usin The community in which we want to calculate mineralization rates.
#' @param selected A vector of names for which you want to calculate the direct and indirect effects. Default NULL means all of them. Useful for excluding nodes whose removal breaks the community (i.e., basal nodes)
#' @param elements The names of the elements that you want to analyze in a vector. The default value "All" calculates mineralization for all of them.
#' @return A table of node effects on mineralization rates.
#' @details
#' The results are labeled as follows with direct contributions calculated from the full food web and indirect contributions calculated from the food web without that node. Indirect contributions do not include the direct contribution (i.e., it is subtracted).
#'
#'\describe{
#'   \item{Direct}{The direct contribution to mineralization.}
#'   \item{Indirect}{The indirect contribution to mineralization.}
#' }
#' The indirect contributions are calculated as the total mineralization of the community with the trophic species minus the trophic species direct mineralization minus the total mineralization without the trophic species all divided by the total mineralizaiton with the trophic species.
#'
#' @examples
#' # Basic example for the introductory community:
#' whomineralizes(intro_comm) # For all elements
#' whomineralizes(intro_comm, element = "Nitrogen") # For a specific element
#' whomineralizes(intro_comm, selected = c("Pred", "Prey1")) # For certain nodes only
#' @export
whomineralizes <- function(usin, elements = "All", selected = NULL){
  Nnodes = dim(usin$imat)[1] # Get the number of nodes
  Nnames = usin$prop$Carbon$ID # Get the names

  # Select only the chosen nodes:
  if(!is.null(selected)){
    if(!all(selected %in% Nnames)) stop("All selected nodes must be present in the community.")
    Nnames = selected
  }

  if(elements == "All" | elements == "all"){
    elements = names(usin$prop)
  }

  output = vector(mode = "list", length = length(elements))
  for(el in 1:length(elements)){
    element = elements[el]

    res1 <- comana(usin) # Calculate the C and N fluxes

    if(any(unname(usin$prop$Carbon$ID) != names(res1$usin$prop$Carbon$ID))){
      stop("Sorting of trophic levels not matching up")
    }

    # Calculate the direct and indirect mineralization rates
    output[[el]] = data.frame(
      ID = unname(usin$prop$Carbon$ID), # Name of the node
      Element = element,
      Direct = res1$mineralization[[element]]/sum(res1$mineralization[[element]]), # Direct C mineralization--the amount that the species respires over the total
      Indirect = NA) # Save space for indirect
    rownames(output) = NULL # remove rownames

    output[[el]] = output[[el]][(output[[el]]$ID %in% Nnames),]

    # Calculate the indirect effects by removing a single node from the community and calculating the difference
    for(rmnode in Nnames){
      usinmod = removenodes(usin, rmnode) # Remove a code
      res2 = comana(usinmod) # Calculate the new fluxes

      # Indirect carbon flux: accounts for change caused by removing the node less the node direct effect.
      output[[el]][output[[el]]$ID == rmnode, "Indirect"] =
        (sum(res1$mineralization[[element]]) - res1$mineralization[[element]][rmnode] - sum(res2$mineralization[[element]]))/sum(res1$mineralization[[element]])
    }
  }

  # Combine into one data frame:
  output = do.call("rbind", output)
  rownames(output) = NULL

  return(output)
}
