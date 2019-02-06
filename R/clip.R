clip_raster <- function(aws.df = AWS.df, aws_name, sensor_name = temperature_sensor_name, raster, radius){
  single_aws <- select_single_aws(aws.df = aws.df, aws_name, sensor_name)
  buffer <- raster:buffer(x = single_aws[["aws_rd.sp"]], width = radius)
  View(raster)
  #mask <- raster::mask(raster, buffer)
  
 #plot(mask)
  
}


clip_raster(aws_name = "De Bilt", raster = "data/De Bilt/AHN2/raw/De Bilt_AHN2_raw_ahn.tif", radius = 10)
