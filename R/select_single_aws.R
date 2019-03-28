#'Select AWS point
#'
#'@title Select AWS point
#'@description Select coordinates of AWs based on provided name of AWS 
#'@param name Required. Name of AWS
#'@param sensor.name Optional. Name of sensor name.
#'@author Jelle Stuurman
#'@return sf spatial point in RD New coordinates format
select_single_aws <- function(aws.df = AWS.df, name, sensor.name = "site"){
  if(missing(sensor.name)){
    sensor.name <- "site"
    first_sensor_name <- "site"
  } else {
    first_sensor_name <- sensor.name
  }
  aws <- aws.df[which(aws.df$name == name & aws.df$sensor == sensor.name),] 
  if(nrow(aws) == 0){
    stop("No AWS found with this name and/or sensor name.")
    return (NULL)
  } else {
    if(missing(sensor.name)){
      sensor.name = "site"
    }
    single_aws.df <- aws.df[which(aws.df$sensor == sensor.name & aws.df$name == name),]
    
    if(nrow(single_aws.df) == 0 | nrow(single_aws.df) > 1){
      single_aws.df <- aws.df[which(aws.df$sensor == "site" & aws.df$name == name),]
      message(paste0(first_sensor_name, " sensor is not found. 'site' is selected as sensor name."))
      
    }
    
    if(nrow(single_aws.df) == 1){
      print(paste0("Getting AWS coordinates of ", name, " with '", sensor.name, "' as sensor name."))
      single_aws_rd.sf <- AWS.sf[which(aws.df$sensor == sensor.name & aws.df$name == name),]
      return(single_aws_rd.sf)
    } else {
      stop("No single entry AWS found with this AWS name.")
    }
  }
}