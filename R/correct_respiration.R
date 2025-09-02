#' A function to reduce growth by increasing respiration.
#'
#' @param usin The input community.
#' @param output_type Should the nutrient limitation be printed (TRUE) or included in the output as a second object (FALSE)?
#' @param biomass_weight_preference Should the preference matrix be weighted by biomass inside this function? Default, False, assumes that you have already done this with the function biomass_weight_preferences or don't want to weight by biomass.
#' @return The modified community with a higher respiration rate.
#' @export
correct_respiration = function(usin, output_type = TRUE, biomass_weight_preference = FALSE){

  # Weight preferences if needed:
  if(biomass_weight_preference){
    usin = biomass_weight_preferences(usin)
  }

  # Produce a vector to print/output the nutrient limitation of each organism:
  nutlim <- rep(NA, dim(usin$imat)[1])

  #Identify the species that need correction by having negative mineralization and canIMM == 0 and more than 1 prey item
  species = unname(which(apply(do.call("rbind", comana(usin)$mineralization)* # This is the mineralizaiton
                                 do.call("rbind",lapply(usin$prop$general, function(x) (1-x$canIMM))), # This means that if canIMM == 1 the negative number is multiplied by zero and removed so that the test of needing correction fails. If canIMM ==0, then the numbers are left as is.
                               2, function(x) any(x < 0))
  ))

  # Solve the new respiration rates with consumption rates:

  # Separate the imat and prop:
  imat = usin$imat # row values of imat sets predator feeding preferences!
  prop = usin$prop$general # properties of each trophic species
  assim = usin$prop$assimilation # the assimilation matrices
  Nnodes = dim(imat)[1] # Number of nodes in the food web

  # Create a vector for the consumption rates
  temp_mat = -1*t(imat)

  temp_mat[!is.finite(temp_mat)] = 0 # Replace non-finite values with 0 because total consumption was zero in this case

  # Prepare feeding options for code below:
  temp_mat2 = imat

  temp_mat2[!is.finite(temp_mat2)] = 0 # Replace non-finite values with 0 because total consumption was zero in this case

  # Save the preference matrix:
  preference_matrix = -1*t(temp_mat)

  # Calculate the vector weighted assimilation efficiencies:
  assimpref = rowSums(assim$Carbon*preference_matrix)

  # Replace any zeros with 1, because they mean no assimilation but are necessary for a solution:
  assimpref[assimpref == 0] = 1

  # Create a vector for the consumption rates
  diag(temp_mat) = prop$Carbon$p*assimpref + diag(temp_mat) # Add in production and assimilation efficiency terms on the diagonal.

  # Correct columns to correct respiration:
  temp_mat3 = matrix(0, nrow = Nnodes, ncol = length(species))

  temp_mat4 = diag(c(prop$Carbon$B[species]), nrow = length(species), ncol = length(species))

  temp_mat5 = matrix(0, nrow = Nnodes, ncol = Nnodes)

  # Add in the limiting elements:
  for(sp in species){
    # Separate the imat and prop:
    imat = usin$imat # row values of imat sets predator feeding preferences!
    prop = usin$prop$general # properties of each trophic species
    assim = usin$prop$assimilation # the assimilation rates for each species
    # mineralization = comana(usin)$mineralization
    Fij = comana(usin)$fmat$Carbon
    Nnodes = dim(imat)[1] # Number of nodes in the food web
    AIJ = comana(usin)$AIJ

    Ek = lapply(AIJ, function(x) {(prop$Carbon$E[sp]*prop$Carbon$B[sp]) + sum(x[sp,]*Fij[sp,])})

    nutlim[sp] = names(AIJ)[which.min(Ek)]

    temp_mat5[sp,sp] = sum(AIJ[[which.min(Ek)]][sp,]*temp_mat2[sp,])

    temp_mat3[sp,which(species == sp)] = -prop$Carbon$B[sp]
  }


  # Combine to form the matrix Ahat:

  temp_mat = rbind(temp_mat, temp_mat5[species,])

  temp_mat = cbind(temp_mat, rbind(temp_mat3, temp_mat4))

  bvec = c(prop$Carbon$d*prop$Carbon$B + prop$Carbon$E*prop$Carbon$B, -prop$Carbon$E[species]*prop$Carbon$B[species])

  solution = base::solve(temp_mat,bvec)

  if(any(solution < 0)) stop("Correction was unsuccessful. This is likely because cannibalism is too large in one of the populations, thus making increased carbon loss impossible to maintain.")

  # Confirm that this solution is unique by showing Ax = 0 produces x = 0
  if(any(solve(temp_mat,rep(0, Nnodes + length(species))) != 0)){
    warning("Solution to the web is not unique!")
  }

  nutlim[is.na(nutlim)] = "Carbon"

  if(is.null(usin$prop$general$Carbon$Ehat)){
    Ehat = rep(0, Nnodes)
    warning("Adding Ehat to the community.")
  }else{
    Ehat = usin$prop$general$Carbon$Ehat
  }

  Ehat[species] = solution[c((Nnodes+1) : (Nnodes + length(species)))]

  usin$prop$general$Carbon$Ehat = Ehat

  if(output_type){
    print(data.frame(ID = colnames(usin$imat),
                     `Limiting_nutrient` = nutlim))
    return(usin)
  }else{

    limitation = data.frame(ID = colnames(usin$imat),
                        `Limiting_nutrient` = nutlim)

    return(list(usin,limitation))
  }
}
