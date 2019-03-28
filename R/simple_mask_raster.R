#'@title simple mask raster
#'@description full circle mask of raster
#'@param spatialpoint single sf point in RD new coordinates
#'@param name of AWS or location
#'@param ahn_raster AHN raster
#'@param AHN3 Default TRUE. Set to FALSE if AHN2 needs to be used.
#'@param radius radius distance of raster in metres
#'@author Jelle Stuurman
#'@return ahn mask
simple_mask_raster <- function(spatialpoint, name, name.supplement = "", ahn_raster, AHN3 = TRUE, radius){
  if(AHN3 == TRUE){
    AHN <- "AHN3"
  } else{
    AHN <- "AHN2"
  }
  spatialpoint.sp <- sf::as_Spatial(spatialpoint) 
  print("Masking the raster object...")
  name_trim <- getName_trim(name = name, name.supplement = name.supplement)
  aws_mask<-raster::buffer(spatialpoint.sp, width=radius)
  ahn_crop<-raster::crop(ahn_raster, aws_mask)
  ahn_mask<-raster::mask(ahn_crop, aws_mask)
  raster::writeRaster(ahn_mask, paste0("output/", name_trim, "/solar_shadow_angles/rasters/", AHN, "/Masks/", name_trim,  "_", AHN, "_circleMask_.tif"), overwrite = TRUE)
  
  print("Masked the raster object.")
  return(ahn_mask)
}
