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
#'
shadow_angles<-function(spatialpoint,
                        rastergrid,
                        angle,
                        maxDist){
  requireNamespace("sp")
  requireNamespace("raster")
  # requireNamespace("rgdal")

  # if(crs(spatialpoint)!=crs(rastergrid)){
  #   message("crs not equal")
  #   return(FALSE)
  # }  

  deBilt_mask<-raster::buffer(spatialpoint,width=maxDist)
  
  
  ahn2_crop<-raster::crop(rastergrid,deBilt_mask)
  ahn2_mask<-raster::mask(ahn2_crop,deBilt_mask)
  message("Masked the raster object, going to calculate horizon angles...")
  horizon_grid<-horizon::horizonSearch(x = ahn2_mask,  
                                       degrees= TRUE,
                                       maxDist = maxDist, 
                                       azimuth = angle,
                                       ll=FALSE)
  st<-raster::stack(ahn2_mask,horizon_grid)
  names(st)<-c("height","azimuth")
  ahn2<-extract(x = st, y = spatialpoint, method='bilinear')
  deBilt<-cbind(debilt,ahn2)
  
  return(list("st"=st,"df"=deBilt))
}
deBilt.df<-AWS.df[which(AWS.df$AWS == "De Bilt"),]
deBilt.sp<-data.frame(deBilt.df)
coordinates(deBilt.sp) <- ~X+Y
crs(deBilt.sp)<-CRS("+init=epsg:28992")
#deBilt.rd <- spTransform(x = deBilt.sp, CRS = crs(ahn2_deBilt))
shadow_angles(spatialpoint = deBilt.rd  , ahn2_deBilt, 10, 100)
