#' The properties database for an example community
#'
#' The community contains 5 nodes and 4 chemical elements.
#'
#' @format A single dataframe with columns ID, Element, Parameter, and Value.
#' \describe{
#'   \item{properties_example1}{The entire dataframe.}
#' }
#' @source Example not based on real empirical data.
"properties_example1"

#' The feeding list for an example community
#'
#' The community contains 5 nodes and 4 chemical elements.
#'
#' @format A single dataframe with columns Predator, Prey, Preference, aCarbon, aNitrogen, aPhosphorus, and aCalcium. The columns beginning in a are the assimilation efficiencies for each trophic interaction for that element.
#' \describe{
#'   \item{feedinglist}{The entire dataframe.}
#' }
#' @source Example not based on real empirical data.
"feedinglist"

#' The example community produced by build_foodweb.
#'
#' The community contains 5 nodes and 4 chemical elements.
#'
#' @format A list containing the feeding matrix and properties
#' \describe{
#'   \item{imat}{The feeding matrix.}
#'   \item{prop}{A list of property databases for each element. There are two parts to this list. The general part includes parameters at the node level and the assimilation part refers to the feeding-specific assimilation efficiency.}
#' }
#' @source Example not based on real empirical data.
"intro_comm"

#' The very simple 3-species example community produced.
#'
#' The community contains 3 nodes and 2 chemical elements.
#'
#' @format A list containing the feeding matrix and properties
#' \describe{
#'   \item{imat}{The feeding matrix.}
#'   \item{prop}{A list of property databases for each element. There are two parts to this list. The general part includes parameters at the node level and the assimilation part refers to the feeding-specific assimilation efficiency.}
#' }
#' @source Example not based on real empirical data.
"intro_comm2"
