#' A function to compile the food web from simple data inputs
#'
#' @param  feeding A data frame listing the feeding relationships in the food web. Only needs to contain the nodes that have prey items along with their names. Three columns in this data frame must be: Predator, Prey, and Preference. The data frame must also have columns for the assimilation efficiency of each feeding relationship for each element, coded "a" and the element name (e.g., aCarbon). The Predator column is the name of the predator, the Prey column is the name of the prey, and the Preference is the preference that the predator has for that prey item. Preference values are relative, so for each predator the preference value can be any number and all that matters is its proportion of the total of all preference values given to that predator, but it makes sense to have them sum to 1. Preference is meaningless if a predator only has one prey item. A good default value is 1 for everything if you don't want to set preferences. You can use the function biomass_weight_preferences to set preferences based on biomass.
#' @param properties A data frame listing the properties or parameters in the food web for each element. Must contain the following columns: ID, Element,Parameter, Value. For Carbon, the following parameters are required for each node: a, E, Q, canIMM, d, B, FecalRecycling,NecromassRecycling, isDetritus, and isPlant. For every other element the following parameters are required for each node: a, Q, canIMM. You can also include the parameters p and Emin for each element and Ehat for carbon. If these are not present, the model sets them to be trivial values that do not affect the calculations.
#' @return A community that is compatible with the functions of the package. This is a list with the feeding matrix imat first and the properties prop second. The properties list contains a data frame for every element.
#' @export

build_foodweb <- function(feeding,
                          properties){

  # Check that feeding has the right columns and values:
  if(!all(colnames(feeding) %in% c("Predator", "Prey", "Preference",paste0("a",unique(properties$Element))))) stop("feeding needs to have the columns: Predator, Prey, and Preference. Also, all elements need to have assimilation efficiencies for each trophic interaction. Add the colnumes as aElement for each Element in the properties data frame.")

  # Check that all of the feeding columns are the right type:
  if(!inherits(feeding$Preference, "numeric")) stop("feeding Preference needs to be numeric")
  if(!inherits(feeding$Predator, "character")) stop("feeding Preference needs to be a character")
  if(!inherits(feeding$Prey, "character")) stop("feeding Preference needs to be a character")

  # Check that there are na missing values:
  if(any(is.na(feeding$Preference))) stop("No feeding preference values can be NA")

  # Check that all of the properties columns are present:
  if(!all(c("ID", "Element", "Parameter", "Value") %in% colnames(properties))) stop("The properties data frame must contain all of the following columns: ID, Element, Parameter, Value.")

  if(any(is.na(properties$Value))) stop("No values in the properties data frame can be NA")

  # Check that all of the properties are present:
  if(!all(c("d","B", "Q", "FecalRecycling", "NecromassRecycling", "isDetritus", "isPlant", "canIMM") %in% unique(properties$Parameter))) stop("The properties data frame must contain all of the following columns: ID, d,p,B, CN, DetritusRecycling, isDetritus, isPlant, and canIMM.")

  # Check that all of the feeding relationships are listed in the properties data frame:
  if(!(all(unique(c(feeding$Predator, feeding$Prey)) %in% properties$ID))) stop("Node names listed in 'feeding' are not present in the properties data frame as listed in the ID column. All nodes in the feeding matrix must have properties.")

  # Create the feeding matrix using the data from properties as a guide:

  feedingmatrix = matrix(0,
                         dimnames = list(unique(properties$ID), unique(properties$ID)),
                         ncol = length(unique(properties$ID)), nrow = length(unique(properties$ID)), byrow = TRUE)

  #Run through the feeding list and add in the non-zero feeding links identified there.
  for(i in 1:dim(feeding)[1]){
    feedingmatrix[feeding$Predator[i],feeding$Prey[i]] = feeding$Preference[i]
  }

  # # Add in perfect production efficiency if p is not listed in the data frame:
  # if(!any(properties$Parameter == "p")){
  #   tdf = unique(properties[,c("ID", "Element")])
  #
  #   tdf[,c("Parameter")] = c("p")
  #   tdf[,c("Value")] = c(1)
  #
  #   properties = rbind(properties, tdf)
  #   rm(tdf)
  # }

  # Get unique combinations of ID and Element
  unique_combos <- unique(properties[c("ID", "Element")])

  # Initialize a list to store new rows
  new_rows <- list()

  for (i in seq_len(nrow(unique_combos))) {
    id <- unique_combos$ID[i]
    element <- unique_combos$Element[i]

    # Subset for this ID and Element
    subset_rows <- properties[properties$ID == id & properties$Element == element, ]
    params <- subset_rows$Parameter

    has_E <- "E" %in% params
    has_p <- "p" %in% params

    if (!has_E && !has_p && element == "Carbon") {
      stop(paste("Error: Combination ID =", id, "and Element =", element, "is missing both 'E' and 'p'."))
    }

    if (!has_E && has_p && element == "Carbon") {
      new_rows[[length(new_rows) + 1]] <- data.frame(ID = id, Element = element, Parameter = "E", Value = 0)
    }

    if (!has_p) {
      new_rows[[length(new_rows) + 1]] <- data.frame(ID = id, Element = element, Parameter = "p", Value = 1)
    }
  }

  # Combine original data with new rows if any
  if (length(new_rows) > 0) {
    new_data <- do.call(rbind, new_rows)
    properties <- rbind(properties, new_data)
  }



  # Add in a minimum of zero respiration if E is not listed in the data frame:
  if(!any(properties$Parameter == "Emin")){
    tdf = unique(properties[,c("ID", "Element")])

    tdf = subset(tdf, tdf$Element != "Carbon")

    tdf[,c("Parameter")] = c("Emin")
    tdf[,c("Value")] = c(0)

    properties = rbind(properties, tdf)
    rm(tdf)
  }

  # Add in an overflow respiration term if Ehat is not listed in the data frame:
  if(!any(properties$Parameter == "Ehat")){
    tdf = unique(properties[,c("ID", "Element")])

    tdf = subset(tdf, tdf$Element == "Carbon")

    tdf[,c("Parameter")] = c("Ehat")
    tdf[,c("Value")] = c(0)

    properties = rbind(properties, tdf)
    rm(tdf)
  }

  Nnodes = dim(feedingmatrix)[1] # Number of nodes in the food web

  # Break out the elements into a properties list for easier processing:
  element_list = unique(properties$Element)

  prop2 = vector(mode = "list", length = length(element_list))
  names(prop2) = element_list

  for(i in 1:length(prop2)){
    prop2[[i]] = subset(properties, properties$Element == element_list[i])

    prop2[[i]] = stats::reshape(prop2[[i]][, c("ID", "Parameter", "Value")],
                         idvar = "ID",
                         timevar = "Parameter",
                         direction = "wide")

    names(prop2[[i]]) <- gsub("Value.", "", names(prop2[[i]]))
  }

  properties_temp = prop2; rm(prop2)

  # Create the assimilation efficiency matricies using the data from properties as a guide:

  aelist = vector(mode = "list", length = length(element_list))

  aemat = matrix(1,
                         dimnames = list(unique(properties$ID), unique(properties$ID)),
                         ncol = length(unique(properties$ID)), nrow = length(unique(properties$ID)), byrow = TRUE)

  #Run through the feeding list and add in the non-zero feeding links identified there.

  for(jj in 1:length(element_list)){
    for(i in 1:dim(feeding)[1]){
      aemat[feeding$Predator[i],feeding$Prey[i]] = feeding[i, paste0("a", element_list[jj])]
    }
    aelist[[jj]] = aemat
  }

  names(aelist) = element_list

  properties = list(general = properties_temp, assimilation = aelist)

  # Build the community
  community <- list(imat = feedingmatrix,
                   prop = properties)

  return(community)

}
