#'Shadow angles from a height grid around a point
#'
#'@title shadow angles
#'@description calculates the angles of to the obstacles for a point
#'@details something
#'@param spatialpoint point of AWS.
#'@param rastergrid ahn3 De Bilt
#'@param angle in degrees, starting with 0 in the north over east
#'@param maxDist maximum Distance
#'@examples
#' #ahn2 de bilt
#' library(sp)
#' library(raster)
#' 
#' data("ahn2_deBilt")
#' data("AWS.df")
#' deBilt<-AWS.df[which(AWS.df$DS_NAME == "De Bilt"),]
#' deBilt.sp<-data.frame(deBilt)
#' coordinates(deBilt.sp) <- ~DS_LON+DS_LAT
#' crs(deBilt.sp)<-CRS("+init=epsg:4326")
#' deBilt.rd <- spTransform(x = deBilt.sp, CRS = crs(ahn2_deBilt))
#' 
#' shadow_angles(spatialpoint=deBilt.rd,
#' rastergrid=ahn3_deBilt,
#' angle=50,
#' maxDist=100)
#'@export

mask_raster <- function(spatialpoint, ahn2, distance){
  aws_mask<-raster::buffer(spatialpoint,width=distance)
  ahn2_crop<-raster::crop(ahn2,aws_mask)
  ahn2_mask<-raster::mask(ahn2_crop,aws_mask)
  message("Masked the raster object.")
return(ahn2_mask)}


horizon_grid<-function(spatialpoint,
                        ahn_mask,
                        angle,
                        maxDist){
  requireNamespace("sp")
  requireNamespace("raster")
  requireNamespace("horizon")
  # requireNamespace("rgdal")

  # if(crs(spatialpoint)!=crs(rastergrid)){
  #   message("crs not equal")
  #   return(FALSE)
  # }  
  message("going to calculate horizon angles...")

  horizon_grid<<-horizon::horizonSearch(x = ahn_mask,  
                                       degrees= TRUE,
                                       maxDist = maxDist, 
                                       azimuth = angle,
                                       ll=FALSE)
  shadows<<-raster::stack(ahn_mask,horizon_grid)
  names(st)<-c("height","elevation")
  ahn2<-extract(x = shadows, y = spatialpoint, method='bilinear')
  aws<-cbind(deBilt.df,ahn2)
  
  return(list("shadows"=shadows,"df"=aws))
}

deBilt.df<-selectSensor_row("temp_150cm", AWS.df)
deBilt.sp<-data.frame(deBilt.df)
coordinates(deBilt.sp) <- ~X+Y
crs(deBilt.sp)<-CRS("+init=epsg:28992")
ahn_mask <- mask_raster(spatialpoint = deBilt.sp  , ahn2_deBilt_raw, distance = 300)

ah_solar_shadow_angles <- above_horizon_solar_angles
ah_azimuths <- ah_solar_shadow_angles$azimuth

ah_shadow_rasters <- list()
for (a in 1:length(ah_azimuths)){
  shadow_angles_DeBilt <- horizon_grid(spatialpoint = deBilt.sp, ahn_mask = ahn_mask, angle = ah_azimuths[a], maxDist = 300)
  ah_solar_shadow_angles[a,"height_shadow"] <- shadow_angles_DeBilt[["df"]]$height
  ah_solar_shadow_angles[a,"shadow_angles"] <- shadow_angles_DeBilt[["df"]]$elevation
  ah_shadow_rasters[a] <- shadow_angles_DeBilt[["shadows"]] 
}

#shadow_angles <- horizon_grid(spatialpoint = deBilt.sp, ahn_mask, angle = 0, maxDist = 300)

