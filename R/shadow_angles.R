#'Shadow angles from a height grid around a point
#'
#'@title shadow angles
#'@description calculates the angles of to the obstacles for a point
#'@details something
#'@param spatialpoint point of AWS
#'@param name of AWS or location
#'@param LONLAT Optional. Default FALSE
#'@param ahn_mask raster of AHN.
#'@param angle in degrees, starting with 0 in the north over east
#'@param maxDist maximum Distance
#'@param AHN3 Default TRUE. Set to FALSE if AHN2 needs to be used
#'@return data frame with shadow angle
shadow_angles <- function(spatialpoint, name, name.supplement = "",  LONLAT = FALSE,
                          ahn_mask,
                          angle,
                          maxDist,
                          AHN3 = TRUE,
                          read_only = FALSE,
                          extract_method = 'bilinear'){
  requireNamespace("sp")
  requireNamespace("raster")
  requireNamespace("horizon")
  # requireNamespace("rgdal")

  # if(crs(spatialpoint)!=crs(rastergrid)){
  #   message("crs not equal")
  #   return(FALSE)
  # }
  if("sf" %in% class(spatialpoint)){
    spatialpoint.sp <- sf::as_Spatial(spatialpoint)
  } else {
    spatialpoint.sp <- spatialpoint
  }

  if(LONLAT == TRUE){
    spatialpoint.sp <- sp::spTransform(spatialpoint.sp, rgdal::CRSargs(CRS("+init=epsg:28992")))
  }

  if(missing(name)){
    name <- ""
    name_trim <- ""
  } else {
    name_trim <- getName_trim(name = name, name.supplement = name.supplement)
  }

  if(AHN3 == TRUE){
    AHN <- "AHN3"
  } else{
    AHN <- "AHN2"
  }

  shadow_angles_path <- paste0("output/", name_trim, "/solar_shadow_angles/rasters/", AHN, "/Shadows/", name_trim, "_", AHN,"_sa_", angle, ".tif")
  if(read_only == FALSE){
    print(paste0("Calculating shadow angles using the ", extract_method, " method..."))
    horizon_grid<-horizon::horizonSearch(x = ahn_mask,
                                         degrees= TRUE,
                                         maxDist = maxDist,
                                         azimuth = angle,
                                         ll=FALSE,
                                         filename = shadow_angles_path)

    shadows<-raster::stack(ahn_mask,horizon_grid)
    names(shadows)<-c("height","shadow_angle_raw")
    #print(str(shadows[["shadow_angle"]]))
    #plot(shadows)
    #writeRaster(shadows[["shadow_angle_raw"]], paste0("output/", name_trim, "/solar_shadow_angles/rasters/", AHN, "/Shadows/", name_trim, "_", AHN,"_sa_", angle, ".tif"), overwrite = TRUE, rasterOptions = rasterOptions(tmpdir = "Rtmp/"))
  } else {
    print(paste0("Only reading exisiting values using the ", extract_method, " method..."))
    sa_values <- raster::raster(shadow_angles_path)
    shadows<-raster::stack(ahn_mask,sa_values)
    names(shadows)<-c("height","shadow_angle_raw")
  }

  df<- raster::extract(x = shadows, y = spatialpoint.sp, method=extract_method)
  #df<- as.data.frame(matrix)
  return(df) #,"shadows"=shadows,))
}
