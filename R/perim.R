perim <- function(){
  perim <- st_read(dsn = "data/coordinates/aws_Vlissingen_perim.shp")
  
  #ahn <- raster("output/Vlissingen/solar_shadow_angles/rasters/AHN3/Shadows/Vlissingen_AHN3_sa_171.526696898183.tif")
  ahn <- raster("data/AHN3/Vlissingen/raw/Vlissingen_AHN3_raw_ahn.tif")
  
  mask <- raster::mask(ahn, perim)
  crs(mask) <- epsg_rd
  writeRaster(mask, filename = "data/AHN3/Vlissingen/raw/Vlissingen_aws_AHN3_raw_ahn.tif", overwrite = TRUE)
  #plot(mask)
return (mask)}

perim()


ahn_diff <- perim()
ahn2_sa <- perim() 
ahn3_sa <- perim()
plot(ahn_diff, xlim=c(30455,30475), ylim=c(385080,385140))


stack <- raster::stack(ahn_diff, ahn2_sa, ahn3_sa)

plot(stack, xlim=c(30455,30475), ylim=c(385080,385140))
