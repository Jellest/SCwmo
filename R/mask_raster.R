simple_mask_raster <- function(spatialpoint, ahn, distance){
  aws_mask<-raster::buffer(spatialpoint,width=distance)
  ahn_crop<-raster::crop(ahn,aws_mask)
  ahn_mask<-raster::mask(ahn_crop,aws_mask)
  message("Masked the raster object.")
  return(ahn_mask)
}

mask_raster <- function(aws_name, spatialpoint, ahn, azimuth, radius){
  if(missing(aws_name)){
    aws_name <- ""
    aws_name_trim <- ""
  } else {
    aws_name_trim <- getAWS_name_trim(aws_name)
  }
  #angles east and west from azimuth
  azimuth_west <- azimuth+5
  azimuth_east <- azimuth-5

  #Circle centre point
  X0 <- spatialpoint@coords[,"X"] 
  Y0 <- spatialpoint@coords[,"Y"]
  
  #X points East and West from line
  PXe <- X0 + (radius * sin((azimuth_east*(pi/180))))
  PYe <- Y0 + (radius * cos((azimuth_east*(pi/180))))
  
  #Y points East and West from line  
  PXw <- X0 + (radius * sin((azimuth_west*(pi/180))))
  PYw <- Y0 + (radius * cos((azimuth_west*(pi/180))))
    
  s_radius <- 12
  
  #point other side
  TXe <- X0 + (s_radius * sin((360-(180 - azimuth_west))*(pi/180)))
  TYe <- Y0 + (s_radius * cos((360-(180 - azimuth_west))*(pi/180)))
  
  TXw <- X0 + (s_radius * sin((360-(180 - azimuth_east))*(pi/180)))
  TYw <- Y0 + (s_radius * cos((360-(180 - azimuth_east))*(pi/180)))

  #perpendicular points sun side
  PTXe <- TXe + ((radius + s_radius) * sin((azimuth*(pi/180))))
  PTYe <- TYe + ((radius + s_radius) * cos((azimuth*(pi/180))))
  
  PTXw <- TXw + ((radius + s_radius) * sin((azimuth*(pi/180))))
  PTYw <- TYw + ((radius + s_radius) * cos((azimuth*(pi/180))))
  
  #perpendicular points nearby aws other side
  PAXw <- TXw + ((s_radius - 3) * sin((azimuth*(pi/180))))
  PAYw <- TYw + ((s_radius - 3) * cos((azimuth*(pi/180))))
  
  PAXe <- TXe + ((s_radius - 3) * sin((azimuth*(pi/180))))
  PAYe <- TYe + ((s_radius - 3) * cos((azimuth*(pi/180))))
  
  #create Polygon
  coords <- matrix(c(  PAXw, PAYw
                     , PAXe, PAYe
                     , PTXe, PTYe
                     , PTXw, PTYw
                     , PAXw, PAYw),
                  ncol = 2, byrow = TRUE)
  P1 <- Polygon(coords)
  
  Ps1 <- SpatialPolygons(list(Polygons(list(P1), ID = "a")), proj4string=CRS(epsg_rd))
  aws_mask <- SpatialPolygonsDataFrame(Ps1, data.frame(row.names=c('a'), y=runif(1)))
  #plot(aws_mask, axes = TRUE)
  ahn_mask <- raster::mask(ahn, aws_mask)
  writeRaster(ahn_mask, paste0("output/solar_shadow_angles/",aws_name_trim,"/rasters/", aws_name_trim, "_ahnMask_", azimuth, ".tif"), overwrite = TRUE)
  print("Masked the raster object.")
  #print(summary(ahn_mask))
  return(ahn_mask)
}
ahn_deBilt_mask <- mask_raster(aws_name = "De Bilt", spatialpoint = deBilt_rd.sp  , cropped_ahn_DeBilt, azimuth = 120, radius = 300)
cropped_ahn_DeBilt <- raster("data/AHN2/DeBilt/raw/DeBilt_raw_ahn.tif")
