#select single AWS coordinates and select aws single row
select_single_aws <- function(aws.df, aws_name, sensor_name){
  single_aws.df<- selectSensor_row(aws_name = aws_name, sensor_name = sensor_name, aws.df = aws.df)
  
  single_aws_rd.sp<-data.frame(single_aws.df)
  coordinates(single_aws_rd.sp) <- ~X+Y
  crs(single_aws_rd.sp)<-CRS("+init=epsg:28992")
  
  single_aws_rd.sf <- st_as_sf(single_aws_rd.sp)
  single_aws_wgs.sf <- st_transform(single_aws_rd.sf, "+init=epsg:4326")
  return(list(aws.df = single_aws.df, aws_rd.sp = single_aws_rd.sp, aws_rd.sf = single_aws_rd.sf, aws_wgs.df = single_aws_wgs.sf))}

selectSensor_row <- function (aws_name, sensor_name, aws.df){
  selectedRow <- aws.df[which(aws.df$Sensor == sensor_name & aws.df$AWS == aws_name),]
  if(nrow(selectedRow) == 0 | nrow(selectedRow) > 1){
    selectedRow <- aws.df[which(aws.df$Sensor == "site" & aws.df$AWS == aws_name),]
  }
  return (selectedRow)}

getAWS_name_trim <- function (aws){
  aws_name_untrimmed <- AWS.df$AWS[which(AWS.df$AWS == aws)][1]
  if(is.na(aws_name_untrimmed) == TRUE) {
    aws_name_trim <- stop(paste("No aws station found with the following name:", aws))
  } else{
    aws_name_trim <- gsub(" ", "", aws_name_untrimmed)
  }
  return (aws_name_trim)}