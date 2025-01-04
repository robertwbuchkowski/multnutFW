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
#' @format A single dataframe with columns Predator, Prey, Preference.
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
#'   \item{prop}{A list of property databases for each element.}
#' }
#' @source Example not based on real empirical data.
"intro_comm"
