#' A function to correct the diet of trophic species.
#'
#' @param usin The input community in which to fix the diets.
#' @param dietlimits # A matrix the same size as imat that gives the diet limits as a proportion of the total diet. All values must be between 0 and 1. Leaving it as NA sets the limits of all diet items to 1.
#' @return The modified community with new diet preferences.
#' @export
correct_diet <- function(usin,dietlimits = c(NA)){

  # Setting up and verifying diet limits
  if(any(is.na(dietlimits))){
    dietlimits = usin$imat
    dietlimits[dietlimits > 0] = 1
  }else{
    if(!all(dim(dietlimits) == dim(usin$imat))) stop("dietlimits must have the same dimensions as imat")
    if(any(dietlimits > 1) | any(dietlimits < 0)) stop("dietlimits must be a proportion of the diet between 0 and 1")
  }
  #Identify the species that need correction by having negative mineralization and canIMM == 0 and more than 1 prey item
  species = unname(which(apply(do.call("rbind", comana(usin)$mineralization)* # This is the mineralizaiton
                                 do.call("rbind",lapply(usin$prop, function(x) (1-x$canIMM))), # This means that if canIMM == 1 the negative number is multiplied by zero and removed so that the test of needing correction fails. If canIMM ==0, then the numbers are left as is.
                               2, function(x) any(x < 0)) &
                           apply(usin$imat > 0, 1, sum) > 1 # Species must have more than one food item
  ))

  for(sp in species){


    # Separate the imat and prop:
    imat = usin$imat # row values of imat sets predator feeding preferences!
    prop = usin$prop # properties of each trophic species
    mineralization = comana(usin)$mineralization
    consumption = comana(usin)$consumption
    Nnodes = dim(imat)[1] # Number of nodes in the food web
    AIJ = comana(usin)$AIJ

    while(TRUE){
      food = imat[sp,] > 0 # Get the list of food items
      numfood = sum(food)
      if(numfood == 1) break()
      biomass = prop$Carbon$B[food] # Get the biomass of food items
      FT = unname(consumption[sp])
      limits = dietlimits[sp,food]

      # Try to see whether there is a solution where the diet can be optimized
      sol1 <- NULL

      # Build the constraints, because they change depending on how many elements are part of the model:

      AMAT = rbind(rep(1, numfood), # Constraint that consumption proportions sum to 1
                   -diag(nrow = numfood), # Diet limits
                   diag(nrow = numfood))

      for(i in 2:length(AIJ)){
        AMAT = rbind(AMAT, AIJ[[i]][sp, food]*FT)
      }

      # Transpose for the calculation below:
      AMAT = t(AMAT)

      # Calculate the bvec for the optimization:
      BVEC = c(1,
               -unname(limits),
               rep(0,numfood))

      for(i in 2:length(prop)){
        BVEC = c(BVEC, -(prop$Carbon$E[sp] + prop$Carbon$Ehat[sp])*prop$Carbon$B[sp])
      }

      try(
        sol1 <- quadprog::solve.QP(Dmat = diag(numfood), # We need the squared terms for each diet, use the identity matrix to get them f^T I f
                                   dvec = biomass/sum(biomass), # The diet is as close to the relative abundance as possible
                                   Amat = AMAT, # proportions sum to 1, Mineralization is zero, and none of the limits are exceeded
                                   bvec = BVEC, # proportions sum to 1, Mineralization is zero, and none of the limits are exceeded, and all values greater than zero
                                   meq = 1 # first position in Amat is an equality constraint)
        ),
        silent = T)

      # If there is no solution, then run a linear program to find the closest diet within the limits
      if(is.null(sol1)){

        # Build the objective function:
        #sum[i = Element] (E_i*C_i)
        # We can get rid of baseline mineralization terms E[k]*C[i] because they are all in units of carbon and increase the objective function, but don't change with F.

        sol1 <- lpSolve::lp(direction = "max",
                            objective.in =
                              c(colSums( # Add across the elements
                                do.call("rbind",lapply(AIJ, function(x) x[sp,food])))* # Draw the AIJ for each prey item.
                                  FT), # Multiply by total consumption rate.
                            const.mat =
                              rbind(rep(1,
                                        numfood),
                                    -diag(nrow = numfood),
                                    diag(nrow = numfood)),
                            const.dir = c("=",
                                          rep(">=", 2*numfood)),
                            const.rhs = c(1,
                                          -limits,
                                          rep(0, numfood))
        )
      }

      # Confirm that solution is positive. Sometimes the solution produces a tiny negative number because of a rounding error (e.g. 1e-18). Set this to zero.
      solcheck = sol1$solution
      if(any(solcheck < -1e-10)) stop("Solution is returning a large negative value. There has been an error in the optimization.")
      if(any(solcheck < 0)){
        solcheck[solcheck < 0] = 0
      }

      if(any(solcheck < 1e-10)){
        warning(paste0("Diet correction removes an item from the diet of species ",sp, " called ",colnames(imat)[sp],". May get strange model behavior. Check outputs for errors in diet proportions! This code just deletes the food item and distirbutes evenly across the other food items."))

        usin$imat[sp,food][which(solcheck < 1e-10)] = 0
        break()

      }else{
        break()
      }
    }

    FLIST = solcheck
    FLIST1 = FLIST/biomass
    FLIST1 = min(FLIST1[FLIST1 > 0])
    FLIST2 = (FLIST/biomass)/FLIST1

    usin$imat[sp,food] = FLIST2

    if(sum((comana(usin,shuffleTL = F)$fmat$Carbon[sp,food]/(comana(usin,shuffleTL = F)$consumption[[sp]]) - FLIST)^2) > 1e-10) stop("Check quadratic optimization. There is an issue.")

  }

  return(usin)

}
