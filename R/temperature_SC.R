sAWS <- create_temperature_SC(aws.df = AWS.df,
                                   aws_list = c(sAWS_names), addition = "", summary_addition = "", 
                                   sensor_name = temperature_sensor_name, criteria_columnName = "Criteria_Value", class_selection = "final_class",
                                   AHN3 = FALSE, import_ahn = FALSE, redownload_ahn = FALSE, ahn_resolution = 0.5, ahn_radius = 500, delete_ahn_sheets = TRUE,
                                   import_bgt = FALSE, redownload_bgt = FALSE, bgt_radius = 150, delete_bgt_gmls = TRUE,
                                   solar_angles = TRUE, angle_selection_byIndexNr = c("all"),
                                   years = c(2018), months = c(12, 1:6), days = c(21),
                                   s_hour = 0, f_hour = 23, minutes_interval = 15,
                                   include_shadow_angles = TRUE, calculate_shadow_angles = FALSE, read_only_shadow_values = FALSE,
                                   shadow_radius = 300, full_circle_mask = FALSE, extract_method = 'bilinear',
                                   sensor_height = 0,
                                   vegetation_radius = 10,
                                   exportShp = TRUE, exportCSV = TRUE, printChart = FALSE,
                                   test = TRUE)
sAWS_ahn3 <- create_temperature_SC(aws.df = AWS.df,
                            aws_list = c(sAWSahn3_names), addition = "", summary_addition = "", 
                            sensor_name = temperature_sensor_name, criteria_columnName = "Criteria_Value", class_selection = "final_class",
                            AHN3 = TRUE, import_ahn = FALSE, redownload_ahn = FALSE, ahn_resolution = 0.5, ahn_radius = 500, delete_ahn_sheets = TRUE,
                            import_bgt = FALSE, redownload_bgt = FALSE, bgt_radius = 150, delete_bgt_gmls = TRUE,
                            solar_angles = TRUE, angle_selection_byIndexNr = c("all"),
                            years = c(2018), months = c(12, 1:6), days = c(21),
                            s_hour = 0, f_hour = 23, minutes_interval = 15,
                            include_shadow_angles = TRUE, calculate_shadow_angles = FALSE, read_only_shadow_values = FALSE,
                            shadow_radius = 300, full_circle_mask = FALSE, extract_method = 'bilinear',
                            sensor_height = 0,
                            vegetation_radius = 10,
                            exportShp = TRUE, exportCSV = TRUE, printChart = FALSE,
                            test = TRUE)

cs <- create_temperature_SC(aws.df = AWS.df,
                                   aws_list = c("De Bilt"), addition = "", summary_addition = "", 
                                   sensor_name = temperature_sensor_name, criteria_columnName = "Criteria_Value", class_selection = "final_class",
                                   AHN3 = FALSE, import_ahn = FALSE, redownload_ahn = FALSE, ahn_resolution = 0.5, ahn_radius = 500, delete_ahn_sheets = TRUE,
                                   import_bgt = FALSE, redownload_bgt = FALSE, bgt_radius = 150, delete_bgt_gmls = TRUE,
                                   solar_angles = TRUE, angle_selection_byIndexNr = c("all"),
                                   years = c(2018), months = c(12, 1:6), days = c(21),
                                   s_hour = 0, f_hour = 23, minutes_interval = 15,
                                   include_shadow_angles = TRUE, calculate_shadow_angles = FALSE, read_only_shadow_values = FALSE,
                                   shadow_radius = 300, full_circle_mask = FALSE, extract_method = 'bilinear',
                                   sensor_height = 0,
                                   vegetation_radius = 10,
                                   exportShp = TRUE, exportCSV = TRUE, printChart = FALSE,
                                   test = TRUE)


#sResults
  View(cs[["summary"]])
  View(cs[["overview_shading_table"]])
  View(cs[["complete_shading_table"]])
  plot(cs[["shading_chart"]])
  cs[["map"]]
  View(cs[["land_use"]])
  View(cs[["vegetation_height"]])

mapshot(cs[["map"]], file = "DeBilt_vegetation_height.png", remove_url = TRUE, removeControls = c("zoomControl", "layersControl", "homeButton"))
  
  
temp_map <- map_rd(aws_name = "De Bilt", sensor_name = temperature_sensor_name, vegetation_height_raster = TRUE, vegetation_radius = 10)
mapshot(temp_map, file = "DeBilt_vegetation_height.png", remove_url = TRUE, removeControls = c("zoomControl", "layersControl", "homeButton"))

  


    
###
  
selected_aws <- "De Bilt"
View(cs[[selected_aws]][["summary"]])
View(cs[["AWS"]][[selected_aws]][["summary"]])
View(cs[["AWS"]][[selected_aws]][["overview_shading_table"]])
View(cs[["AWS"]][[selected_aws]][["complete_shading_table"]])
plot(cs[["AWS"]][[selected_aws]][["shading_chart"]])
cs[["AWS"]][[selected_aws]][["map"]]
View(cs[["AWS"]][[selected_aws]][["land_use"]])
View(cs[["AWS"]][[selected_aws]][["vegetation_height"]])

View(cs)
