#' A function to compile the food web from simple data inputs
#'
#' @param  feeding A data frame listing the feeding relationships in the food web. Only needs to contain the nodes that have prey items along with their names. Three columns in this data frame must be: Predator, Prey, and Preference. The Predator column is the name of the predator, the Prey column is the name of the prey, and the Preference is the preference that the predator has for that prey item after correcting for abundance. Preference values are relative, so for each predator the preference value can be any number and all that matters is its proportion of the total of all preference values given to that predator. Preference is meaningless if a predator only has one prey item. A good default value is 1 for everything if you don't want to set preferences beyond prey abundance.
#' @param properties A data frame listing the properties or parameters in the food web for each element. Must contain the following columns: ID, Element,Parameter, Value. For Carbon, the following parameters are required for each node: a, E, Q, canIMM, d, B, Detritusrecycling, isDetritus, and isPlant. For every other element the following parameters are required for each node: a, Q, canIMM. You can also include the parameters p and Emin for each element and Ehat for carbon. If these are not present, the model sets them to be trivial values that do not affect the calculations.
#' @return A community that is compatible with the functions of the package. This is a list with the feeding matrix imat first and the properties prop second. The properties list contains a data frame for every element.
#' @examples
#'# Creating a simple three node community:
#'
#'# Create a data frame of feeding relationships:
#'feedinglist = data.frame(
#'  Predator = c("Pred", "Pred", "Prey1", "Prey2", "Prey2","Prey1", #'"Microbe1"),
#'  Prey = c("Prey1", "Prey2", "Prey2", "Microbe1","Detritus","Detritus","Detritus"),
#'  Preference = c(1,1.2,1,1,1,1,1))
#'
#'# Load in a data frame of properties for each species:
#'# data(properties_example1)
#'
#'# Build the food web:
#'build_foodweb(feeding = feedinglist, properties = properties_example1)
#' @export

build_foodweb <- function(feeding,
                          properties){

  # Check that feeding has the right columns and values:
  if(!all(colnames(feeding) %in% c("Predator", "Prey", "Preference"))) stop("feeding needs to have the columns: Predator, Prey, and Preference")

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
  if(!all(c("d","a","B", "Q", "DetritusRecycling", "isDetritus", "isPlant", "canIMM") %in% unique(properties$Parameter))) stop("The properties data frame must contain all of the following columns: ID, d,a,p,B, CN, DetritusRecycling, isDetritus, isPlant, and canIMM.")

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

  # Add in perfect production efficiency if p is not listed in the data frame:
  if(!any(properties$Parameter == "p")){
    tdf = unique(properties[,c("ID", "Element")])

    tdf[,c("Parameter")] = c("p")
    tdf[,c("Value")] = c(1)

    properties = rbind(properties, tdf)
    rm(tdf)
  }

  # Add in a minimum of zero respiration if E is not listed in the data frame:
  if(!any(properties$Parameter == "Emin")){
    tdf = unique(properties[,c("ID", "Element")])

    tdf = subset(tdf, Element != "Carbon")

    tdf[,c("Parameter")] = c("Emin")
    tdf[,c("Value")] = c(0)

    properties = rbind(properties, tdf)
    rm(tdf)
  }

  # Add in an overflow respiration term if Ehat is not listed in the data frame:
  if(!any(properties$Parameter == "Ehat")){
    tdf = unique(properties[,c("ID", "Element")])

    tdf = subset(tdf, Element == "Carbon")

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

    prop2[[i]] = reshape(prop2[[i]][, c("ID", "Parameter", "Value")],
                         idvar = "ID",
                         timevar = "Parameter",
                         direction = "wide")

    names(prop2[[i]]) <- gsub("Value.", "", names(prop2[[i]]))
  }

  properties = prop2; rm(prop2)


  # Build the community
  community <- list(imat = feedingmatrix,
                   prop = properties)

  return(community)

}
