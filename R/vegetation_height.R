library(raster)
library(rgeos)
vegetation_height <- function(aws_name, coords, height_raw, height_terrain, lgn7, radius, vegetation_criteria){
  
  aws_mask<-raster::buffer(coords,width=radius)
  
  ahn2_raw_crop<-raster::crop(height_raw,aws_mask)
  ahn2_raw_mask<-raster::mask(ahn2_raw_crop,aws_mask)
  
  ahn2_terrain_crop<-raster::crop(height_terrain,aws_mask)
  ahn2_terrain_mask<-raster::mask(ahn2_terrain_crop,aws_mask)
  
  lgn7_crop <- raster::crop(lgn7,aws_mask)
  lgn7_mask <-raster::mask(lgn7,aws_mask)
  
  height <- ahn2_raw_mask - ahn2_terrain_mask
 
  df <- data.frame(minHeight = numeric(nrow(vegetation_criteria)), maxHeight = numeric(nrow(vegetation_criteria))
                   #, avgHeight = numeric(nrow(vegetation_criteria))
                   )
  selected_row_number <- which(vegetation_criteria == aws_name)
  df[selected_row_number, "maxHeight"] <- maxValue(height)
  df[selected_row_number, "minHeight"] <- minValue(height)
  #df[selected_row_number, "avgheight"] <-  
  vegetation_criteria.df <- cbind(vegetation_criteria, df)
  return_list <- list("heightDifference" = height, "criteria_table" = vegetation_criteria.df, "raw_mask" = ahn2_raw_mask, "terrain_mask" = ahn2_terrain_mask, lgn7 = lgn7_mask)
  
return (return_list)}

vegetation_height_criteria.df <- AWS.df[c(1,4)]
vegetation_height_criteria.df <- vegetation_height_criteria.df[-(1:10), ]
heightDifference <- vegetation_height("De Bilt", aws_debilt_rd.sp, ahn2_deBilt_sheet_raw, ahn2_deBilt_sheet_terrain, lgn7,
                            5, vegetation_height_criteria.df)

View(height[["criteria_table"]])

plot(heightDifference[["raw_mask"]])
plot(heightDifference[["terrain_mask"]])
plot(heightDifference[["heightDifference"]])
plot(heightDifference[["lgn7"]])

r<-height[["raw_mask"]]
mapview(r,layer.name="raw_mask")
h
h

