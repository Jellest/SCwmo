####test data

###test coordinates

## De Bilt
# X <- 140760.56
# Y <- 456754.84
# LAT <- 52.09886
# LON <- 5.17939
deBilt_rd.sp <- data.frame("X"=140760.56,"Y"=456754.84)
coordinates(deBilt_rd.sp) <- ~X+Y
crs(deBilt_rd.sp) <- CRS("+init=epsg:28992")
deBilt.sf <- st_as_sf(deBilt_rd.sp)

ahn_deBilt <- import_ahn(aws_name = "De Bilt",
                          station_coords = deBilt_rd.sp,#station_coords = selected_aws_temp[["aws_rd.sp"]],
                          resolution = 0.5, radius = 500,
                          raw_ahn = TRUE, terrain_ahn = TRUE,
                          AHN3 = FALSE)

## test coordinates
test_point.sp <- data.frame("X"=135000.000,"Y"=456250.000)
coordinates(test_point.sp) <- ~X+Y
crs(test_point.sp) <- CRS("+init=epsg:28992")
test_point.sf <- st_as_sf(test_point.sp)