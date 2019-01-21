library(raster)
library(rgeos)
vegetation_height <- function(aws.df = AWS.df, aws_name, sensor_name, radius, AHN3 = FALSE, exportCSV = FALSE){
  if(AHN3 == TRUE){
    AHN <- "AHN3"
  } else {
    AHN <- "AHN2"
  }
  
  aws <- check_aws_names(aws.df = aws.df, aws_name = aws_name, sensor_name = sensor_name)
  aws_name <- aws[1,"AWS"]
  sensor_name <- aws[1,"Sensor"]  

  single_aws <- select_single_aws(aws.df = aws.df, aws_name = aws_name, sensor_name = sensor_name)
  
  aws_name_trim <- getAWS_name_trim(aws.df = aws.df, aws_name = aws_name)
  
  aws_buffer<-raster::buffer(single_aws[["aws_rd.sp"]], width = radius)
  
  ahn_raw <- raster(paste0("data/", AHN, "/", aws_name_trim, "/raw/", aws_name_trim, "_", AHN ,"_raw_ahn.tif"))
  ahn_terrain <- raster(paste0("data/", AHN, "/", aws_name_trim, "/terrain/", aws_name_trim, "_", AHN ,"_terrain_ahn.tif"))
  
  ahn_raw_crop<-raster::crop(ahn_raw,aws_buffer)
  ahn_raw_mask<-raster::mask(ahn_raw_crop,aws_buffer)
  
  ahn_terrain_crop<-raster::crop(ahn_terrain,aws_buffer)
  ahn_terrain_mask<-raster::mask(ahn_terrain_crop,aws_buffer)
  
  height_difference_raster <- ahn_raw_mask - ahn_terrain_mask
 
  stats <- data.frame(summary(height_difference_raster))
  df <- data.frame(minHeight = numeric(1), maxHeight = numeric(1)
                   #, avgHeight = numeric(nrow(vegetation_criteria))
                   )
  df[1, "maxHeight"] <- stats["Max.",]
  df[1, "minHeight"] <- stats["Min.",]
  df[1, "Median"] <- stats["Median",]
  df[1, "1stQu"] <- stats["1st Qu.",]
  df[1, "3rdQu"] <- stats["3rd Qu.",]
  
  if(exportCSV == TRUE){
    if(!dir.exists("output/vegetation_height")){
      dir.create("output/vegetation_height", showWarnings = FALSE)
    }
    if(!dir.exists(paste0("output/vegetation_height/", aws_name_trim))){
      dir.create(paste0("output/vegetation_height/", aws_name_trim), showWarnings = FALSE)
    }
    writeRaster(height_difference_raster, filename = paste0("output/vegetation_height/", aws_name_trim, "/", aws_name_trim, "_", AHN, "_", radius, "m_height_difference.tif"), overwrite = TRUE)
    fwrite(df, file = paste0("output/vegetation_height/", aws_name_trim, "/", aws_name_trim, "_", AHN, "_", radius ,"m_vegetation_height_stats.csv"))  
  }
  #df[selected_row_number, "avgheight"]  
  #View(summary(height_difference_raster))
  #View(df)
  #plot(height_difference_raster)
return (list("heightDifference" = height_difference_raster, "df" = df, "raw_mask" = ahn_raw_mask, "terrain_mask" = ahn_terrain_mask))
}

plot(heightDifference[["raw_mask"]])
plot(heightDifference[["terrain_mask"]])
plot(heightDifference[["heightDifference"]])