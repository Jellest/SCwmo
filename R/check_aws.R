#'Check AWS name based on provided name
#'
#'@title Check existience of AWS
#'@description Check existiencew of existing AWS
#'@param name Required. Name of AWS
#'@param sensor.name. Optional. Default "site". Select coordinates of certain sensor
#'@author Jelle Stuurman
#'@return sf spatial point in RD New coordinates format
check_aws_names <- function(aws.df = AWS.df, name, sensor.name = "site"){
  aws <- aws.df[which(aws.df$name == name & aws.df$sensor == sensor.name),]
  if(nrow(aws) == 0 | nrow(aws) > 1){
    aws <- aws.df[which(aws.df$name == name & aws.df$sensor == "site"),]
    if(nrow(aws) == 0){
      aws <- aws.df[which(aws.df$name == name),]
      if(nrow(aws) == 0){
        stop("No AWS found with this name and/or sensor name.")
      } else if(nrow(aws) == 1){
        my_sensor_name <- aws[1,"sensor"]
        message(paste0("Single entry AWS name was for for", name, "using ", my_sensor_name," as sensor name."))
        return (aws) 
      } else{
        print(aws)
        stop(paste0("More than one entry has been found for ", name, ". Please select an AWS name (and sensor name) that returns one entry."))
      }
    } else {
      message(paste(sensor.name, "sensor is not found. 'site' is selected as sensor name."))
      sensor.name <- "site"
      return(aws)
    }
  } else if(nrow(aws) == 1){
    return(aws)
  }
}