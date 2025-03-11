#' #' A function to simulation the food webs away from equilibrium.
#' #'
#' #' @param t The ODE time.
#' #' @param y The ODE simulation start.
#' #' @param pars The ODE parameters.
#' #' @details
#' #' The food web model for simulating over time: requires \code{y} inputs and \code{pars} parameters from the \code{\link{getPARAMS}} function.
#' #' @return The changes in each node biomass along with parameters and mineralization rates.
#' #' @export
#' foodwebode <- function(t,y,pars){
#'
#'   y = matrix(y, nrow = nrow(pars$pmat), ncol = ncol(pars$pmat))
#'
#'   # Calculate consumption rates:
#'   consumption = lapply(c(1,2,3,4),
#'                        function(lai) {
#'                          predC = matrix(y[,lai], nrow = nrow(pars$cij), ncol = ncol(pars$cij))
#'                          preyC = t(predC)
#'                          consumption = pars$cij*predC*preyC/(1 + pars$cij*pars$h*preyC)
#'                        })
#'
#'   netwithoutmineralization =
#'     # Gains from consumption:
#'     pars$pmat*sapply(Map(function(x,y) x*y, consumption, pars$assimilation), rowSums) -
#'     # Losses from predation:
#'     sapply(consumption, colSums) -
#'     # Natural death:
#'     matrix(pars$death[,1]*(1-pars$death[,3]), nrow = nrow(y), ncol = ncol(y))*y - # density-independent
#'     matrix(pars$death[,2]*(pars$death[,3]), nrow = nrow(y), ncol = ncol(y))*y*y + # density-dependent
#'
#'     # Detritus recycling:
#'     matrix(pars$detplant$DetritusRecycling, nrow = nrow(y), ncol = ncol(y))* # A matrix to allocate the detritus recycling appropriately
#'     matrix(
#'       colSums(sapply(Map(function(x,y) x*y, consumption, lapply(pars$assimilation, function(X) 1 - X)), rowSums)) + # A vector of unassimilated material (i.e., faeces)
#'         colSums(matrix(pars$death[,1]*(1-pars$death[,3]), nrow = nrow(y), ncol = ncol(y))*y - # density-independent
#'                   matrix(pars$death[,2]*(pars$death[,3]), nrow = nrow(y), ncol = ncol(y))*y*y), # density-dependent + # A vector of carcases
#'       nrow = nrow(y), ncol = ncol(y),byrow = T) - # arrange in a matrix by row so that the elements are in the columns.
#'
#'     # Respiration based on biomass:
#'     matrix(c(pars$ECarbon*y[,1], rep(0, nrow(y)*(ncol(y)-1))), nrow = nrow(y), ncol = ncol(y))
#'
#'   # Calcualte the mineralization rate given the change in carbon and fixed C:X ratio for all non-detritus nodes:
#'   mineralization = (netwithoutmineralization - matrix(netwithoutmineralization[,1], nrow = nrow(y), ncol = ncol(y))*pars$Qmat)*matrix(1-pars$detplant$isDetritus, nrow = nrow(y), ncol = ncol(y))
#'
#'   print(mineralization)
#'
#'   netwithmineralization = netwithoutmineralization - mineralization + colSums(mineralization)*matrix(pars$detplant$isDetritus, nrow = nrow(y), ncol = ncol(y))
#'
#'   io = matrix(0, nrow = nrow(y), ncol = ncol(y))
#'   io[5,1] = 2.07
#'
#'   dy = netwithmineralization #+ io
#'
#'   return(list(c(dy)))
#' }
#'
#'
#' pars = getPARAMS(intro_comm)$parameters
#' y = getPARAMS(intro_comm)$yeqm
#'
#' foodwebode(1,y,pars)
#'
#' deSolve::ode(y, 1:10, func = foodwebode, parms = pars)
#'
#'
#'
#'
#'
