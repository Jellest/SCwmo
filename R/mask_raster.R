mask_raster <- function(spatialpoint, ahn, azimuth, distance){
  #angles east and west from azimuth
  #plot(ahn)
  azimuth_west <- azimuth+5
  azimuth_east <- azimuth-5

  #Circle centre point
  X0 <- spatialpoint@coords[,"X"] 
  Y0 <- spatialpoint@coords[,"Y"]
  
  #X points East and West from line
  Xe <- X0 + (distance * sin((azimuth_east*(pi/180))))
  Xw <- X0 + (distance * sin((azimuth_west*(pi/180))))
  
  #Y points East and West from line  
  Ye <- Y0 + (distance * cos((azimuth_east*(pi/180))))
  Yw <- Y0 + (distance * cos((azimuth_west*(pi/180))))
    
  #coordinates
  x_coords <- c(X0, Xe, Xw, X0)
  y_coords <- c(Y0, Ye, Yw, Y0)

  #create Polygon
  #XYm <- cbind(x_coords, y_coords)
  #print(XYm)
  # p <- Polygon(XYm)
  # ps <-Polygons(list(p),1)
  # sps <- SpatialPolygons(list(ps))
  # proj4string(sps) <- CRS("+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.4171,50.3319,465.5524,-0.398957,0.343988,-1.8774,4.0725 +units=m +no_defs")
  # sps
  # plot(sps)
  
  # aws_buffer <-raster::buffer(spatialpoint,width=distance)
  
  # 
  coords <- matrix(c(X0, Y0,
                    Xe, Ye,
                    Xw, Yw,
                    X0, Y0),
                  ncol = 2, byrow = TRUE)
  #print(coords)
  P1 <- Polygon(coords)
  
  Ps1 <- SpatialPolygons(list(Polygons(list(P1), ID = "a")), proj4string=CRS(epsg_rd))
  #P1sdf <- SpatialPolygonsDataFrame(Ps1, data = as.data.frame(ID = "test"))
  
  aws_mask <- SpatialPolygonsDataFrame(Ps1, data.frame(row.names=c('a'), y=runif(1)))
  #writeOGR(obj = aws_mask, dsn = "test" , layer = "ahn_mask", driver = "ESRI Shapefile", overwrite_layer = TRUE)
  
  #plot(Ps1, axes = TRUE)
  #plot(ahn, Ps1)
  
  #plot(ahn, aws_mask, axes = TRUE)
  ahn_mask <- raster::mask(ahn, aws_mask)
  message("Masked the raster object.")
  print(summary(ahn_mask))
  return(ahn_mask)
}
ahn_deBilt_mask <- mask_raster(spatialpoint = deBilt_rd.sp  , cropped_ahn_DeBilt, azimuth = 120, distance = 300)
cropped_ahn_DeBilt <- raster("data/AHN2/DeBilt/raw/DeBilt_raw_ahn.tif")
