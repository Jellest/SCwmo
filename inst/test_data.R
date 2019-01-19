####test data

###test coordinates

## De Bilt
# X <- 140760.56
# Y <- 456754.84
# LAT <- 52.09886
# LON <- 5.17939
deBilt_rd.sp <- data.frame("X"=140760.56,"Y"=456754.84)
coordinates(deBilt_rd.sp) <- ~X+Y
crs(deBilt_rd.sp) <- CRS(epsg_rd)
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

select_single_aws(AWS.df, "Schiphol", "temp_150cm")

aws_list <- dplyr::filter(AWS.df, DS_DESC == "AWS" & Aparatuur == "site")[,1]


##solar amnd shadow angles
testangles <- solar_angles(X = 5.17939,
                           Y = 52.09886,
                           day = 21,
                           month = 12,
                           year = 2018,
                           minutes_interval = 15,
                           LONLAT = TRUE)
fwrite(testangles[["all angles"]], "testangles.csv")

test_sa <- multiple_Moments_solarAngles(aws_name = "De Bilt",
                                        sensor_name = "temp_150cm",
                                        years = c(2018),
                                        months = c(12,1:6),
                                        days = c(21),
                                        exportCSV = TRUE,
                                        printChart = FALSE)

test_so_sh_angles <- multipleShadowAngles(test_sa[["all ah angles"]], radius = 300)

#solar and shadow angles chart
sun_shade_angles_chart("output/solar_shadow_angles/DeBilt/DeBilt_ah_solar_shadow_angles.csv", "De Bilt")

#shadow classifcation
projected_shade_class(data_path = "output/solar_shadow_angles/DeBilt/DeBilt_ah_solar_shadow_angles.csv", aws_name = "De Bilt")

#vegetation height
vegetation_height_criteria.df <- AWS.df[c(1,4)]
vegetation_height_criteria.df <- vegetation_height_criteria.df[-(1:10), ]
heightDifference <- vegetation_height(aws_name = "De Bilt", radius = 100)
vegetation_classes(df = heightDifference[[2]],aws_name = "De Bilt", exportCSV = TRUE)

vegetation_classes(df = vegetation_height(aws_name = "De Bilt", radius = 10, exportCSV = TRUE)[["df"]], aws_name = "De Bilt", exportCSV = TRUE)

# land use
presence_objects(aws_name = "De Bilt")

#create classifcation
create_classifications("De Bilt", sensor_name = "temp_150cm")
#summary classifcation
summary_classifcation(aws_name = "De Bilt", sensor_name = "temp_150cm")

