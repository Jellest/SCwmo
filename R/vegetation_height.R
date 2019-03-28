#'vegetation height
#'
#'@title vegetation height
#'@description Determine vegetation height 
#'@param name of AWS or location
#'@param spatialpoint single sf point in RD new coordinates
#'@param radius radius distance in metres
#'@param AHN3 Default TRUE. Set to FALSE if AHN2 needs to be used.
#'@author Jelle Stuurman
#'@return list
#' \enumerate{
#'   \item height_raster: Height raster determined by subtracting the terrain from the raw AHN raster
#'   \item df: summary statistics of results
#'   \item raw_mask: mask raster of raw AHN
#'   \item terrain_mask: mask raster of terrain AHN
#' }
vegetation_height <- function(spatialpoint, name, name.supplement = "", radius, AHN3 = TRUE, exportCSV = FALSE){
  if(AHN3 == TRUE){
    AHN <- "AHN3"
  } else {
    AHN <- "AHN2"
  }
  
  name_trim <- getName_trim(name = name, name.supplement = name.supplement)
  
  buffer<-sf::st_buffer(spatialpoint, dist = radius)
  
  ahn_raw <- raster::raster(paste0("datasets/", AHN, "/", name_trim, "/raw/", name_trim, "_", AHN ,"_raw_ahn.tif"))
  ahn_terrain <- raster::raster(paste0("datasetes/", AHN, "/", name_trim, "/terrain/", name_trim, "_", AHN ,"_terrain_ahn.tif"))
  
  ahn_raw_crop<-raster::crop(ahn_raw,buffer)
  ahn_raw_mask<-raster::mask(ahn_raw_crop,buffer)
  
  ahn_terrain_crop<-raster::crop(ahn_terrain,buffer)
  ahn_terrain_mask<-raster::mask(ahn_terrain_crop,buffer)
  
  height_difference_raster <- ahn_raw_mask - ahn_terrain_mask
 
  stats <- data.frame(summary(height_difference_raster))
  df <- data.frame(minHeight = numeric(1), maxHeight = numeric(1)
                   #, avgHeight = numeric(nrow(vegetation_criteria))
                   )
  df[1, "maxHeight"] <- base::abs(stats["Max.",])
  df[1, "minHeight"] <- base::abs(stats["Min.",])
  df[1, "Median"] <- base::abs(stats["Median",])
  df[1, "1stQu"] <- base::abs(stats["1st Qu.",])
  df[1, "3rdQu"] <- base::abs(stats["3rd Qu.",])
  
  if(exportCSV == TRUE){
    if(name_trim == ""){
      base:dir.create("output/vegetation_height", showWarnings = FALSE)
    }
    
    if(!dir.exists(paste0("output/", name_trim))){
      base:dir.create(paste0("output/", name_trim), showWarnings = FALSE)
    }
    if(!dir.exists(paste0("output/", name_trim, "/vegetation_height"))){
      base:dir.create(paste0("output/", name_trim, "/vegetation_height"), showWarnings = FALSE)
    }
    
    if(name_trim != ""){
      raster::writeRaster(height_difference_raster, filename = paste0("output/", name_trim, "/vegetation_height/", name_trim, "_", AHN, "_", radius, "m_height_difference.tif"), overwrite = TRUE)
      data.table::fwrite(df, file = paste0("output/", name_trim, "/vegetation_height/", name_trim, "_", AHN, "_", radius ,"m_vegetation_height_stats.csv"))  
    } else {
      raster::writeRaster(height_difference_raster, filename = paste0("output/vegetation_height/", name_trim, "/", name_trim, "_", AHN, "_", radius, "m_height_difference.tif"), overwrite = TRUE)
      data.table::fwrite(df, file = paste0("output/vegetation_height/", name_trim, "/", name_trim, "_", AHN, "_", radius ,"m_vegetation_height_stats.csv"))  
    }
  }
  #df[selected_row_number, "avgheight"]  
  #View(summary(height_difference_raster))
  #View(df)
  #plot(height_difference_raster)
return (list("height_raster" = height_difference_raster, "df" = df, "raw_mask" = ahn_raw_mask, "terrain_mask" = ahn_terrain_mask))
}