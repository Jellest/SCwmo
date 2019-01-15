#general function

iterate_features = function(x){
  for(i in 1:nrow(x)){
    p = x[i,]
  }
  return(p)
}

calculate_area <- function(sp){
  for(i in 1:nrow(sp)){
    p = sp@data[i,]
    p[i,"Area"] <- gArea(p)
  }
  return(p)
}

CleanGlobEnvir <- function(pattern){
  rm(list = ls(envir=globalenv())[
  grep(pattern, ls(envir=globalenv()))], envir = globalenv())
}

# mask_raster <- function(spatialpoint, ahn, distance){
#   aws_mask<-raster::buffer(spatialpoint,width=distance)
#   ahn_crop<-raster::crop(ahn,aws_mask)
#   ahn_mask<-raster::mask(ahn_crop,aws_mask)
#   message("Masked the raster object.")
# return(ahn_mask)}

awsNameCheck <- function(aws_name, sensor_name){
  if(missing(aws_name)){
    aws_name <- NULL
    aws_name_trim <- NULL
  } else {
    aws_name_trim <- getAWS_name_trim(aws_name)
  }
  
  if(missing(sensor_name)){
    sensor_name <- NULL
  }
  return(list("aws_name" = aws_name, "aws_name_trim" = aws_name_trim, "sensor_name" = sensor_name))
}

create_SpatialPoint <- function(X, Y, LONLAT){
  if(missing(LONLAT)){
    LONLAT = FALSE
  }
  point.sp<-data.frame(X = X, Y = Y, stringsAsFactors=FALSE)
  coordinates(point.sp) <- ~X+Y
  if(LONLAT == FALSE){
    crs(point.sp)<-CRS(epsg_rd)
    point_rd.sp <- point.sp
    point_wgs.sp <- spTransform(point.sp, "+init=epsg:4326") 
  } else {
    crs(point.sp)<-CRS("+init=epsg:4326")
    point_rd.sp <- spTransform(point.sp, epsg_rd)
    point_wgs.sp <- point.sp
  }

  return (list("point_rd.sp" = point_rd.sp, "point_wgs.sp" = point_wgs.sp))
}
