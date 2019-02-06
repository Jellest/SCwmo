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

shadow_angles <- function(aws.df = AWS.df,
                          aws_name, addition = "",
                          spatialpoint,
                          X,
                          Y,
                          LONLAT,
                          ahn_mask,
                          angle,
                          maxDist,
                          AHN3 = FALSE,
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
  if(missing(LONLAT)){
    LONLAT = FALSE
  }
  
  if(missing(spatialpoint)){
    spatialpoint <- data.frame("X"=X,"Y"=Y)
    coordinates(spatialpoint) <- ~X+Y
    if(LONLAT == TRUE){
      crs(spatialpoint) <- CRS("+init=epsg:4326")
    } else {
      crs(spatialpoint) <- CRS(epsg_rd)
    }
  }
  if(missing(aws_name)){
    aws_name <- ""
    aws_name_trim <- ""
  } else {
    aws_name_trim <- getAWS_name_trim(aws.df = aws.df, aws_name = aws_name, addition = addition)
  }

  if(AHN3 == TRUE){
    AHN <- "AHN3"
  } else{
    AHN <- "AHN2"
  }
  
  shadow_angles_path <- paste0("output/", aws_name_trim, "/solar_shadow_angles/rasters/", AHN, "/Shadows/", aws_name_trim, "_", AHN,"_sa_", angle, ".tif")
  if(read_only == FALSE){
    print(paste0("Calculating shadow angles using the ", extract_method, " method..."))
    horizon_grid<-horizon::horizonSearch(x = ahn_mask,  
                                         degrees= TRUE,
                                         maxDist = maxDist, 
                                         azimuth = angle,
                                         ll=LONLAT,
                                         filename = shadow_angles_path)
    
    shadows<-raster::stack(ahn_mask,horizon_grid, rasterOptions = rasterOptions(tmpdir = "Rtmp/"))
    names(shadows)<-c("height","shadow_angle_raw")
    #print(str(shadows[["shadow_angle"]]))
    #plot(shadows)
    #writeRaster(shadows[["shadow_angle_raw"]], paste0("output/", aws_name_trim, "/solar_shadow_angles/rasters/", AHN, "/Shadows/", aws_name_trim, "_", AHN,"_sa_", angle, ".tif"), overwrite = TRUE, rasterOptions = rasterOptions(tmpdir = "Rtmp/"))
  } else {
    print(paste0("Only reading exisiting values using the ", extract_method, " method..."))
    sa_values <- raster(shadow_angles_path)
    shadows<-raster::stack(ahn_mask,sa_values, rasterOptions = rasterOptions(tmpdir = "Rtmp/"))
    names(shadows)<-c("height","shadow_angle_raw")
  }
  
  df<-extract(x = shadows, y = spatialpoint, method=extract_method)
  #View(df)
  #df<- as.data.frame(matrix)
  return(list("df"=df)) #,"shadows"=shadows,))
}

#test
ahn_deBilt_mask <- mask_raster(spatialpoint = deBilt_rd.sp  , ahn_deBilt[["raw"]], distance = 100)
shadow_angles_DeBilt <- shadow_angles(spatialpoint = deBilt_rd.sp, ahn_mask = ahn_deBilt_mask, angle = 180, maxDist = 100)
