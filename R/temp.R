determine_height <- function(aws.df = AWS.df, aws_name, sensor_name, AHN3 = FALSE, ahn_terrain, ahn_raw, height_instrument){
  if(AHN3 == TRUE){
    AHN <- "AHN3"
  } else{
    AHN <- "AHN2"
  }
  
  single_aws <- select_single_aws(aws.df = aws.df, aws_name = aws_name, sensor_name = sensor_name)
  aws_name_trim <- getAWS_name_trim(aws.df = aws.df, aws_name = aws_name)
  point.sp <- single_aws[["aws_rd.sp"]]
  value <- extract(ahn_terrain, point.sp, sp = T)@data[1,paste0(aws_name_trim, "_", AHN, "_terrain_ahn")]
  cellnr<-print(cellFromXY(ahn_raw, point.sp))
  
  ahn_raw[cellnr] <- 6.039392
  
  new_value <- extract(ahn_raw, point.sp, sp = T)@data[1,paste0(aws_name_trim, "_", AHN, "_raw_ahn")]
  
  
return(new_value)}


determine_height(aws.df = AWS.df,
                 aws_name = "De Bilt",
                 sensor_name = temperature_sensor_name,
                 ahn_terrain = raster("data/AHN2/DeBilt/terrain/DeBilt_AHN2_terrain_ahn.tif"), 
                 ahn_raw = raster("data/AHN2/DeBilt/raw/DeBilt_AHN2_raw_ahn.tif"),
                 height_instrument = 1.5)
