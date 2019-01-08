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

shadow_angles <- function(spatialpoint,
                          X,
                          Y,
                          LONLAT,
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
  if(missing(LONLAT)){
    LONLAT = FALSE
  }
  
  if(missing(spatialpoint)){
    spatialpoint <- data.frame("X"=X,"Y"=Y)
    coordinates(spatialpoint) <- ~X+Y
    if(LONLAT == TRUE){
      crs(spatialpoint) <- CRS("+init=epsg:4326")
    } else {
      crs(spatialpoint) <- CRS("+init=epsg:28992")
    }
  }
  if(LONLAT == TRUE){
    spatialpoint <- spTransform(spatialpoint, CRS = CRS("init:espg28992"))
    LONLAT = FALSE
  }
  
  
  horizon_grid<-horizon::horizonSearch(x = ahn_mask,  
                                       degrees= TRUE,
                                       maxDist = maxDist, 
                                       azimuth = angle,
                                       ll=LONLAT)
  shadows<-raster::stack(ahn_mask,horizon_grid)
  names(shadows)<-c("height","shadow_angle")
  df<-extract(x = shadows, y = spatialpoint, method='bilinear')
  #df<- as.data.frame(matrix)
  return(list("shadows"=shadows,"df"=df))
}

#test
ahn_deBilt_mask <- mask_raster(spatialpoint = deBilt_rd.sp  , ahn_deBilt[["raw"]], distance = 100)
shadow_angles_DeBilt <- shadow_angles(spatialpoint = deBilt_rd.sp, ahn_mask = ahn_deBilt_mask, angle = 180, maxDist = 100)
