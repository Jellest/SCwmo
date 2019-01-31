diff <- function(ahn2, ahn3){
  
  difference <- ahn3 - ahn2
  
  plot(difference)
  writeRaster(difference, "output/Wijkaanzee/ahn3_ahn2_diff.tif")
}


diff(
  ahn2 = raster("data/AHN2/Wijkaanzee/raw/Wijkaanzee_AHN2_raw_ahn.tif"),
  ahn3 = raster("data/AHN3/Wijkaanzee/raw/Wijkaanzee_AHN3_raw_ahn.tif")
)

