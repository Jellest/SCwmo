library(raster)
library(rgeos)
vegetation_height <- function(aws_name, radius){
  aws_name_trim <- getAWS_name_trim(aws_name)
  single_aws <- select_single_aws(aws.df = AWS.df, aws_name = "De Bilt", sensor_name = "temp_150cm")
  
  aws_buffer<-raster::buffer(single_aws[["aws_rd.sp"]], width = radius)
  
  ahn_raw <- raster(paste0("data/AHN2/", aws_name_trim, "/raw/", aws_name_trim, "_raw_ahn.tif"))
  ahn_terrain <- raster(paste0("data/AHN2/", aws_name_trim, "/terrain/", aws_name_trim, "_terrain_ahn.tif"))
  
  ahn_raw_crop<-raster::crop(ahn_raw,aws_buffer)
  ahn_raw_mask<-raster::mask(ahn_raw_crop,aws_buffer)
  
  ahn_terrain_crop<-raster::crop(ahn_terrain,aws_buffer)
  ahn_terrain_mask<-raster::mask(ahn_terrain_crop,aws_buffer)
  
  height <- ahn_raw_mask - ahn_terrain_mask
 
  stats <- data.frame(summary(height))
  df <- data.frame(minHeight = numeric(1), maxHeight = numeric(1)
                   #, avgHeight = numeric(nrow(vegetation_criteria))
                   )
  df[1, "maxHeight"] <- stats["Max.",]
  df[1, "minHeight"] <- stats["Min.",]
  df[1, "Median"] <- stats["Median",]
  df[1, "1stQu"] <- stats["1st Qu.",]
  df[1, "3rdQu"] <- stats["3rd Qu.",]
  

  #df[selected_row_number, "avgheight"]  

return (list("heightDifference" = height, "values" = df, "raw_mask" = ahn_raw_mask, "terrain_mask" = ahn_terrain_mask))
}

plot(heightDifference[["raw_mask"]])
plot(heightDifference[["terrain_mask"]])
plot(heightDifference[["heightDifference"]])