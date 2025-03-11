#' Calculates the trophic level for each tropospecies
#'
#' @param W A matrix of trophic species. Rows eat columns.
#' @return A vector of trophic level assignments. The base of the food chain is 0.
#' @details
#' This function is a subset of code provided in the package Cheddar written by Lawrence Hudson, Dan Reuman and Rob Emerson. It is licensed under a BSD_2_clause, the text of which is provided as a comment in the function code. The original package can be found on CRAN or at https://github.com/quicklizard99/cheddar/
#' @export
#' @examples
#' TLcheddar(intro_comm$imat)
TLcheddar <- function(W){

  # Original license for this code:
  # YEAR:2014-2016
  # COPYRIGHT HOLDER:  Lawrence Hudson, Dan Reuman and Rob Emerson
  #
  #   and specify as
  #
  # License: BSD_2_clause + file LICENSE
  #
  # Copyright (c) 2014-2016, Lawrence Hudson, Dan Reuman and Rob Emerson
  #
  #   Redistribution and use in source and binary forms, with or without
  # modification, are permitted provided that the following conditions are
  # met:
  #
  #   Redistributions of source code must retain the above copyright
  # notice, this list of conditions and the following disclaimer.
  #
  # Redistributions in binary form must reproduce the above copyright
  # notice, this list of conditions and the following disclaimer in
  # the documentation and/or other materials provided with the
  # distribution.
  #
  # THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  # "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  # LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
  # A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
  # HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
  # SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
  # LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
  # DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
  # THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  # (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  # OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


  # Make each row sum to one.
  rs <- rowSums(W)
  W <- W / matrix(rs, ncol=ncol(W), nrow=nrow(W))
  W[0==rs,] <- 0      # Fix NA resulting from div by zero

  # Identity matrix
  I <- diag(ncol(W))

  # Invert the matrix. From Rich 2011-08-17: "If this matrix inversion
  # fails, there is an important problem with the network topology.
  # For a food web to be energetically feasible, every node must be
  # connected to a basal node. When the inversion fails it is because
  # there is at least one node that has no connection to a basal node."
  result <- tryCatch(solve(I-W), error = function(e) e)

  if('error' %in% class(result))
  {
    tl <- rep(NA, ncol(W))
    names(tl) <- colnames(W)
  }
  else
  {
    # Returned vector is named by node
    tl <- rowSums(result)   # Isolated nodes have tl of 1
  }

  return (tl)
}

#' Sorts the trophic levels from lowest to highest
#'
#' @param usin The community to be sorted.
#' @return The community returned after sorting
#' @examples
#' TLsort(intro_comm)
#' @export
TLsort <- function(usin){

  # Identify the cannibals and mutual predators if not already done
  if(!("MutualPred" %in% colnames(usin$prop$general$Carbon))){
    usin = can_mutfeed(usin)
  }

  imat = usin$imat # row values of imat sets predator feeding preferences!
  prop = usin$prop

  Nnodes = dim(imat)[1] # Number of nodes in the food web

  # Calculate the trophic level
  TL = seq(Nnodes,1,-1)
  names(TL) = colnames(imat)

  TL_temp = TLcheddar(imat)

  if(!(any(is.na(TL_temp)))){
    TL = TL_temp
  }else{
    print("Error in TL sorting: usually comes from a poorly defined matrix where feeding relationships are not possible at equilibrium.")
  }

  rm(TL_temp)

  # Shuffle the trophic levels

  # First sort by trophic level to get the order close
  imat = imat[order(-TL),order(-TL)]

  # Then confirm that there are no cases where a low trophic level predator eats a higher trophic level prey
  positions <- 1:dim(imat)[1]
  rid = min(positions)
  stuckinloop = 0
  while(rid <= max(positions)){

    # Build a list of predators
    predatorlist = imat[,rid] > 0
    # Turn any predators that are on the mutual feeding list to false...these can't be sorted hierarchically
    predatorlist[names(predatorlist) %in%  stringr::str_split(prop$general$Carbon$MutualPred[rid], "/")[[1]]] = FALSE
    if(sum(predatorlist) == 0){
      rid = rid + 1
    }else{
      max_predator = max(which(predatorlist))
      if(max_predator > rid){
        part1 = min(positions):max_predator
        if(max_predator == max(positions)){
          part2 = NULL
        }else{
          part2 = (max_predator+1):max(positions)
        }
        new_order = c(part1[part1 != rid], rid, part2[part2 != rid])
        imat = imat[new_order,new_order]
        stuckinloop = stuckinloop + 1
        if(stuckinloop > 100){
          break
          warning("Fine level sorting not converging, returning early. Likely caused by extensive mutual feeding.")
        }
      }else{
        rid = rid + 1
        stuckinloop = 0
      }
    }
  }

  # Order of the colnames:
  for(ii in 1:length(prop$general)){
    prop$general[[ii]] = prop$general[[ii]][match(colnames(imat), prop$general[[ii]]$ID),]

    stopifnot(all(colnames(imat) == prop$general[[ii]]$ID))
  }

  # Order columns and rows in assimilation matrices:
  for(ii in 1:length(prop$assimilation)){
    prop$assimilation[[ii]] = prop$assimilation[[ii]][colnames(imat),colnames(imat)]

    stopifnot(all(colnames(imat) == colnames(prop$assimilation[[ii]])))
  }

  return(list(imat = imat, prop = prop))
}

#' A function that identifies cannibalism and mutual feeding
#'
#' @param usin The community in which to identify cannibalism and mutual feeding
#' @return The community with mutual feeding added to the properties database.
#' @examples
#' intro_comm_mod = intro_comm
#' intro_comm_mod$imat["Pred", "Pred"] = 1
#' can_mutfeed(intro_comm_mod)
#' @export
can_mutfeed <- function(usin){
  imat = usin$imat # row values of imat sets predator feeding preferences!
  prop = usin$prop

  Nnodes = dim(imat)[1] # Number of nodes in the food web

  MutualPred = rep(NA,dim(imat)[1])
  for(i in 1:dim(imat)[1]){
    MutualPred[i] = paste0(names(which(imat[imat[i,] > 0,i]>0)), collapse = "/")

  }

  MutualPred[MutualPred == ""] = NA

  prop$general$Carbon[,"MutualPred"] = MutualPred

  return(list(imat = imat, prop = prop))
}

#' A utility function to calculate the consumption rate of each species on all prey assuming a type I functional response.
#'
#' @param usin The community on which feeding rate calculations are made.
#' @param h The matrix of the handling times that you want to use a type II functional response. If NA, you are using a Type I functional response. If you want some Type I and some Type II functional responses, include the matrix and put values of 0 for all Type I functional responses.
#' @return A matrix of consumption rates with units set by the the biomass input units in biomass and time.
#' @examples
#' Cijfcn(intro_comm)
#' @export
Cijfcn <- function(usin, h = NA){ # Function only requires the community inputs

  imat = usin$imat # Feeding matrix
  prop = usin$prop # Properties of the species

  Nnodes = dim(imat)[1] # Number of nodes

  if(!all(is.na(h))){
    if(any(dim(h) != dim(imat))) stop("Dimensions of h must equal those of imat.")
  }

  Bpred = matrix(prop$general$Carbon$B, ncol = Nnodes, nrow = Nnodes) # A matrix of predators
  Bprey = matrix(prop$general$Carbon$B, ncol = Nnodes, nrow = Nnodes, byrow = T) # A matrix of prey
  fmat = comana(usin)$fmat$Carbon # Get the consumption matrix (units = gC/ time)
  if(all(is.na(h))){
    cij = fmat/(Bpred*Bprey) # Get the consumption rate matrix (units 1/ (gC * time))
  }else{
    cij = fmat / (Bprey * (Bpred - fmat * h)) # Get the consumption rate
  }
  return(cij) # Return the consumption rates (gC^-1 time^-1)
}

#' A function to rescale a vector.
#'
#' @param invec The vector to re-scale.
#' @param a The lower limit of the new scale.
#' @param b The upper limit of the new scale.
#' @return The scaled vector.
RESCALE <- function(invec, a = 0, b = 1){
  (b-a)*(invec - min(invec))/(max(invec)-min(invec)) + a
}

#' Remove nodes from community.
#'
#' @param COMM The community from which to remove nodes.
#' @param toremove A vector of nodes to remove using their names.
#' @return The community without the removed nodes.
#' @examples
#' removenodes(intro_comm, c("Pred"))
#' @export
removenodes <- function(COMM, toremove){
  whichtorm = !(COMM$prop$general$Carbon$ID %in% toremove)

  COMM$imat = COMM$imat[whichtorm,whichtorm]

  for(ii in 1:length(COMM$prop$general)){
    COMM$prop$general[[ii]] = subset(COMM$prop$general[[ii]], !(COMM$prop$general[[ii]]$ID %in% toremove))

    stopifnot(all(COMM$prop$general[[ii]]$ID == rownames(COMM$imat)))
    stopifnot(all(COMM$prop$general[[ii]]$ID == colnames(COMM$imat)))

    COMM$prop$assimilation[[ii]] = COMM$prop$assimilation[[ii]][whichtorm,whichtorm]

  }
  return(COMM)
}

#' Rename a node in a  community.
#'
#' @param COMM The community from which to remove nodes.
#' @param oldname The node's old name
#' @param newname The node's new name
#' @return The community with the new name.
#' @examples
#' renamenode(intro_comm, oldname = "Pred", newname = "NewPredator")
#' @export
renamenode <- function(COMM, oldname,newname){
  whichtorm = COMM$prop$general$Carbon$ID %in% oldname

  colnames(COMM$imat)[whichtorm] = rownames(COMM$imat)[whichtorm] = newname

  COMM$prop$general$Carbon$ID[whichtorm] = newname

  for(ii in 1:length(COMM$prop$general)){
    COMM$prop$general[[ii]]$ID[whichtorm] = newname

    stopifnot(all(COMM$prop$general[[ii]]$ID == rownames(COMM$imat)))
    stopifnot(all(COMM$prop$general[[ii]]$ID == colnames(COMM$imat)))

    colnames(COMM$prop$assimilation[[ii]]) = colnames(COMM$imat)
    rownames(COMM$prop$assimilation[[ii]]) = rownames(COMM$imat)

  }

  return(COMM)
}

#' Check the carbon flux equilibrium output by comana.
#'
#' @param usin The community to assess equilibrium via comana.
#' @param eqmtolerance A value used to set the equilibrium tolerance for the food web verification. If NA, the default value used by the function all.equal is used, which is 1.5e-8.
#' @return Boolean: Is the community at equilibrium?
#' @examples
#' checkeqm(intro_comm)
#' @export
checkeqm <- function(usin, eqmtolerance = NA){

  # Use the functionality in the getPARAMS function:
  !any(getPARAMS(usin = usin, returnnet = TRUE) > 1.5e-8)

}
