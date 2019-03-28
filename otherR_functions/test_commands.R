
#projectes shade classes
test_ssa.csv <- fread("data/solar_shadow_angles/solar_shadow_angles.csv", data.table = FALSE)
test_ssa_criteria <- projected_shade_class(test_ssa.csv)


##import bgt
BGT_DeBilt <- import_bgt("De Bilt", "temp_150cm")
BGT_HvH <-import_bgt("Hoek van Holland", "site")

BGT_DeBilt <- import_single_bgt("De Bilt", "site", delete_raw_gmls = TRUE)


#correct projected shade classes
for(s in 1:length(sAWSahn3_names)){
  aws_name <- sAWSahn3_names[s]
  print(paste("calculating for", aws_name))
  aws_name_trim <- getAWS_name_trim(aws_name = aws_name)
  ahn <- "AHN3"
  my_angles <- fread(paste0("output/", aws_name_trim, "/solar_shadow_angles/", aws_name_trim, "_", ahn, "_ah_solar_shadow_angles.csv"), data.table = FALSE)
  colnames(my_angles)[colnames(my_angles)=="shadow_angle"] <- "shadow_angle_raw"
  aws <- select_single_aws(aws.df = AWS.df, aws_name = aws_name, sensor_name = temperature_sensor_name)
  for(a in 1:nrow(my_angles)){
    correction_angle <- highest_shadow_angle(aws, aws_name = aws_name,
                                             sensor_name = temperature_sensor_name,
                                             x = my_angles[a, "azimuth"],
                                             shadow_angle_raw = my_angles[a, "shadow_angle_raw"]
    )
    my_angles[a,"shadow_angle"] <- correction_angle
  }
  fwrite(my_angles, file = paste0("output/", aws_name_trim, "/solar_shadow_angles/", aws_name_trim, "_", ahn, "_ah_solar_shadow_angles.csv"))
  fwrite(my_angles, file = paste0("output/", aws_name_trim, "/solar_shadow_angles/", aws_name_trim, "_", ahn, "_ah_solar_shadow_angles_backup.csv"))
}
