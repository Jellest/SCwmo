ahn3_ahn2_difference <- function(ahn2, ahn3, aws_name_trim){
  difference <- ahn3 - ahn2
  
  #plot(difference)
  writeRaster(difference, paste0("output/", aws_name_trim, "/", aws_name_trim, "_ahn3_ahn2_diff.tif"))
}



for(s in 1:length(sAWSahn3_names)){
  aws <- sAWSahn3_names[s]
  aws_name_trim <- getAWS_name_trim(aws_name = aws)
  ahn3_ahn2_difference(
    ahn2 = raster(paste0("data/AHN2/", aws_name_trim, "/raw/", aws_name_trim, "_AHN2_raw_ahn.tif")),
    ahn3 = raster(paste0("data/AHN3/", aws_name_trim, "/raw/", aws_name_trim, "_AHN3_raw_ahn.tif")),
    aws_name_trim = aws_name_trim
  )
}

