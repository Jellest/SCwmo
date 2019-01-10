mask_raster <- function(spatialpoint, ahn, azimuth, distance){
  #angles east and west from azimuth
  azimuth_west <- azimuth+5
  azimuth_east <- azimuth-5

  #Circle centre point
  X0 <- deBilt_rd.sp@coords[,"X"] 
  Y0 <- deBilt_rd.sp@coords[,"Y"]
  
  #X points East and West from line
  Xe <- X0 + (distance * sin(azimuth_east))
  Xw <- X0 + (distance * sin(azimuth_west))
  
  #X points East and West from line  
  Ye <- Y0 + (distance * cos(azimuth_east))
  Yw <- Y0 + (distance * cos(azimuth_west))
    
  #coodinates
  x_coords <- c(X0, Xe, Xw, X0)
  y_coords <- c(Y0, Ye, Yw, Y0)

  #create Polygon
  XYm <- cbind(x_coords, y_coords)
  print(XYm)
  p <- Polygon(XYm)
  ps <-Polygons(list(p),1)
  sps <- SpatialPolygons(list(ps))
  proj4string(sps) <- CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs")
  sps
  plot(sps)
  
  # aws_buffer <-raster::buffer(spatialpoint,width=distance)
  
  # 
  # coords <- matrix(c(X0, Y0,
  #                   Xw, Yw,
  #                   Xe, Ye,
  #                   X0, Y0), 
  #                 ncol = 2, byrow = TRUE)
  # 
  # P1 <- Polygon(coords)
  # Ps1 <- SpatialPolygons(list(Polygons(list(P1), ID = "a")), proj4string=CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs"))
  # plot(Ps1)
  
  # 
  # ahn_crop<-raster::crop(ahn,aws_buffer)
  # 
  # ahn_mask<-raster::mask(ahn_crop,aws_mask)
  # message("Masked the raster object.")
  return(ahn_mask)}
