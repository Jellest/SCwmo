#'@title mask raster
#'@description section mask of raster
#'@param spatialpoint single sf point in RD new coordinates
#'@param name of AWS or location
#'@param ahn_raster AHN raster
#'@param AHN3 Default TRUE. Set to FALSE if AHN2 needs to be used.
#'@param azimuith solar azimuth angle in degrees
#'@param radius distance radius of raster in metres.
#'@author Jelle Stuurman
#'@examples
#'
#'
#'@return ahn mask
mask_raster <- function(spatialpoint, name, name.supplement = "", ahn_raster, AHN3 = TRUE, azimuth, radius){
  #View(spatialpoint)
  if(missing(name)){
    name <- ""
    name_trim <- ""
  } else {
    name_trim <- getName_trim(name = name, name.supplement = name.supplement)
  }

  if(AHN3 == TRUE){
    AHN <- "AHN3"
  } else{
    AHN <- "AHN2"
  }

  #angles east and west from azimuth
  azimuth_west <- azimuth+5
  azimuth_east <- azimuth-5

  #Circle centre point
  geom <- data.frame(sf::st_coordinates(spatialpoint))
  X0 <- geom$X
  Y0 <- geom$Y

  #View(X0)

  #X point East from line
  PXe <- X0 + (radius * sin((azimuth_east*(pi/180))))
  PYe <- Y0 + (radius * cos((azimuth_east*(pi/180))))

  #Y point West from line
  PXw <- X0 + (radius * sin((azimuth_west*(pi/180))))
  PYw <- Y0 + (radius * cos((azimuth_west*(pi/180))))

  s_radius <- 15

  #point other side
  TXe <- X0 + (s_radius * sin((360-(180 - azimuth_west))*(pi/180)))
  TYe <- Y0 + (s_radius * cos((360-(180 - azimuth_west))*(pi/180)))

  TXw <- X0 + (s_radius * sin((360-(180 - azimuth_east))*(pi/180)))
  TYw <- Y0 + (s_radius * cos((360-(180 - azimuth_east))*(pi/180)))

  #perpendicular points sun side
  PTXe <- TXe + ((s_radius + radius) * sin((azimuth*(pi/180))))
  PTYe <- TYe + ((s_radius + radius) * cos((azimuth*(pi/180))))

  PTXw <- TXw + ((s_radius + radius) * sin((azimuth*(pi/180))))
  PTYw <- TYw + ((s_radius + radius) * cos((azimuth*(pi/180))))

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
  P1 <- sp::Polygon(coords)

  Ps1 <- sp::SpatialPolygons(list(Polygons(list(P1), ID = "a")), proj4string=CRS(rgdal::CRSargs(CRS("+init=epsg:28992"))))
  aws_mask <- sp::SpatialPolygonsDataFrame(Ps1, data.frame(row.names=c('a'), y=runif(1)))
  #plot(aws_mask, axes = TRUE)
  ahn_mask <- raster::mask(ahn_raster, aws_mask)

  raster::writeRaster(ahn_mask, paste0("output/", name_trim, "/solar_shadow_angles/rasters/", AHN, "/Masks/", name_trim, "_", AHN,"_Mask_", azimuth, ".tif"), overwrite = TRUE)
  print("Masked the raster object.")
  #print(summary(ahn_mask))
  return(ahn_mask)
}
