#'Trim name of point by removing spaces
#'
#'@title Trim name
#'@description Trim name of AWS by removing spaces
#'@param name Name of AWS
#'@param name.supplement OOptional. If an name.supplemental string needs to be added to AWS string name 
#'@author Jelle Stuurman
#'@return String with trimmed name
getName_trim <- function (name, name.supplement = ""){
  name_trim <- gsub(" ", "", name)
  if(name.supplement != ""){
    name_trim <- paste0(name_trim, "_", name.supplement)
  }
  return (name_trim)
}