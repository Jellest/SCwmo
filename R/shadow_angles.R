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
#' #ahn3 de bilt
#' library(sp)
#' library(raster)
#' 
#' data("ahn3_deBilt")
#' data("AWS.df")
#' deBilt<-AWS.df[which(AWS.df$DS_NAME == "De Bilt"),]
#' deBilt.sp<-data.frame(deBilt)
#' coordinates(deBilt.sp) <- ~DS_LON+DS_LAT
#' crs(deBilt.sp)<-CRS("+init=epsg:4326")
#' deBilt.rd <- spTransform(x = deBilt.sp, CRS = crs(ahn3_deBilt))
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
  
  
  ahn3_crop<-raster::crop(rastergrid,deBilt_mask)
  ahn3_mask<-raster::mask(ahn3_crop,deBilt_mask)
  message("Masked the raster object, going to calculate horizon angles...")
  horizon_grid<-horizon::horizonSearch(x = ahn3_mask,  
                                       degrees= TRUE,
                                       maxDist = maxDist, 
                                       azimuth = angle,
                                       ll=FALSE)
  st<-raster::stack(ahn3_mask,horizon_grid)
  names(st)<-c("height","azimuth")
  ahn3<-extract(x = st, y = deBilt.rd, method='bilinear')
  deBilt<-cbind(deBilt,ahn3)
  
  return(list("st"=st,"df"=deBilt))
}
