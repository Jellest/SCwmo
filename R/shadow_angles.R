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


shadow_angles<-function(spatialpoint,
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
  horizon_grid<-horizon::horizonSearch(x = ahn_mask,  
                                       degrees= TRUE,
                                       maxDist = maxDist, 
                                       azimuth = angle,
                                       ll=FALSE)
  shadows<-raster::stack(ahn_mask,horizon_grid)
  names(shadows)<-c("height","elevation")
  df<-extract(x = shadows, y = spatialpoint, method='bilinear')
  return(list("shadows"=shadows,"df"=df))
}

projected_shade_class <- function(solar_shading_angles){
  class_1_shading <-subset(solar_shading_angles, elevation > 5)
  class_2_3_shading <- subset(solar_shading_angles, elevation > 7) 
  class_4_shading <- subset(solar_shading_angles, elevation > 20)
  df <- solar_shading_angles 
  
  df$meet_class1 <- TRUE
  df$meet_class2 <- TRUE
  df$meet_class3 <- TRUE
  df$meet_class4 <- TRUE
  
  if(df$elevation[1] < 5){
    meet_class1 <- TRUE
    meet_class2 <- TRUE
    meet_class3 <- TRUE
    meet_class4 <- TRUE
  }
  
  if(df$elevation[1] >= 5 & df$elevation[1] < 7 & df$shadow_angle[1] > df$elevation[1]){
    meet_class1 <- FALSE
    meet_class2 <- TRUE
    meet_class3 <- TRUE
    meet_class4 <- TRUE
  }# else if(df$elevation[1] >= 5 & df$elevation[1] < 7 & df$shadow_angle[1] < df$elevation[1]){
  #   meet_class1 <- TRUE
  #   meet_class2 <- TRUE
  #   meet_class3 <- TRUE
  #   meet_class4 <- TRUE
  # }
  
  if(df$elevation[1] >= 7 & df$elevation[1] < 20 & df$shadow_angle[1] > df$elevation[1]){
    meet_class1 <- FALSE
    meet_class2 <- FALSE
    meet_class3 <- FALSE
    meet_class4 <- TRUE
  }# else if(df$elevation[1] >= 7 & df$elevation[1] < 20 & df$shadow_angle[1] < df$elevation[1]){
  #   meet_class1 <- TRUE
  #   meet_class2 <- TRUE
  #   meet_class3 <- TRUE
  #   meet_class4 <- TRUE
  # }
  
  if(df$elevation[1] >= 20 & df$shadow_angle[1] > df$elevation[1]){
    meet_class1 <- FALSE
    meet_class2 <- FALSE
    meet_class3 <- FALSE
    meet_class4 <- FALSE
  }# else if(df$elevation[1] >= 20 & df$shadow_angle[1] < df$elevation[1]){
  #   meet_class1 <- FALSE
  #   meet_class2 <- FALSE
  #   meet_class3 <- FALSE
  #   meet_class4 <- TRUE
  # }
  
  df$meet_class1 <- meet_class1
  df$meet_class2 <- meet_class2
  df$meet_class3 <- meet_class3
  df$meet_class4 <- meet_class4
return (df)
}
deBilt.df<-selectSensor_row("temp_150cm", AWS.df)
deBilt.sp<-data.frame(deBilt.df)
coordinates(deBilt.sp) <- ~X+Y
crs(deBilt.sp)<-CRS("+init=epsg:28992")
ahn_mask <- mask_raster(spatialpoint = deBilt.sp  , ahn2_deBilt_raw, distance = 300)

ah_azimuths <- above_horizon_solar_angles$azimuth

ah_solar_shadow_angles <- data.frame(aws = character(0), sensor = character(0), lat = character(0), lon = character(0), altitude = numeric(0), julian_day = numeric(0), azimuth = numeric(0), zenith = numeric(0), elevation = numeric(0), shadow_height = numeric(0), shadow_angle = numeric(0), stringsAsFactors = FALSE)
ah_solar_shadow_angles <- ah_solar_shadow_angles[,c("aws", "sensor", "lat", "lon", "altitude", "julian_day", "azimuth", "zenith", "elevation", "shadow_height", "shadow_angle")]
ah_shadow_rasters <- list()
start_time <- Sys.time()
for (a in seq_along(ah_azimuths)){
  message(paste("Calculating shadow angle for azimuth angle: ", ah_azimuths[a],". ", a, " out of ", length(ah_azimuths),"...", sep=""))
  shadow_angles_DeBilt <- shadow_angles(spatialpoint = deBilt.sp, ahn_mask = ahn_mask, angle = ah_azimuths[a], maxDist = 300)
  shadow_ha <-data.frame(shadow_angles_DeBilt$df)
  heightShadow <- shadow_ha$height[1]
  shad_angle <- shadow_ha$elevation[1]
  ah_shadow_rasters[a] <- shadow_angles_DeBilt$shadows 
  sodf <- above_horizon_solar_angles[a,]
  shdf <- data.frame(shadow_height = heightShadow, shadow_angle = shad_angle)
  soshdf <- merge(above_horizon_solar_angles[a,], shdf)
  ah_solar_shadow_angles <- rbind(ah_solar_shadow_angles, soshdf)
  
  if(a == length(ah_azimuths)){
    ah_solar_shadow_angles <- ah_solar_shadow_angles[,c("aws", "sensor", "lat", "lon", "altitude", "julian_day", "azimuth", "zenith", "elevation", "shadow_height", "shadow_angle")]
    end_time <- Sys.time()
    elapsed_time <- end_time - start_time
    message(paste("shadow angle calculations finished. Elapsed Time:", elapsed_time))
  }
}
ah_solar_shadow_angles <- projected_shade_class(ah_solar_shadow_angles)


#shadow_angles <- horizon_grid(spatialpoint = deBilt.sp, ahn_mask, angle = 0, maxDist = 300)

