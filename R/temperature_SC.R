results <- function(results, aws_name){
  if(length(results) > 2){
    View(results[["summary"]])
    View(results[["overview_shading_table"]])
    View(results[["complete_shading_table"]])
    #plot(results[["shading_chart"]])
    results[["map"]]
    View(results[["land_use_table"]])
    View(results[["vegetation_table"]])
  } else if (length(results) == 2){
    View(results[["summary"]])
    View(results[["AWS"]][[aws_name]][["summary"]])
    View(results[["AWS"]][[aws_name]][["overview_shading_table"]])
    View(results[["AWS"]][[aws_name]][["complete_shading_table"]])
    #plot(results[["AWS"]][[aws_name]][["shading_chart"]])
    results[["AWS"]][[aws_name]][["map"]]
    View(results[["AWS"]][[aws_name]][["land_use_table"]])
    View(results[["AWS"]][[aws_name]][["vegetation_table"]])
  }
}

singleAWS <- create_temperature_SC(aws_list = c("De Bilt"),
                            aws.df = AWS.df, addition = "", summary_addition = "", 
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


SC_ahn2 <- create_temperature_SC(aws.df = AWS.df,
                                 aws_list = c(AWS_temperature_names), addition = "", summary_addition = "", 
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
                                 exportShp = FALSE, exportCSV = TRUE, printChart = FALSE,
                                 test = TRUE)

SC_ahn3 <- create_temperature_SC(aws.df = AWS.df,
                                   aws_list = AWS_temperature_ahn3Only_names, addition = "", summary_addition = "", 
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
                                   exportShp = FALSE, exportCSV = TRUE, printChart = FALSE,
                                   test = TRUE)



mapshot(singleAWS[["map"]], file = "DeBilt_vegetation_height.png", remove_url = TRUE, removeControls = c("zoomControl", "layersControl", "homeButton"))
temp_map <- map_rd(aws_name = "De Bilt", sensor_name = temperature_sensor_name, buffers = c(30, 10, 5, 3), vegetation_height_raster = FALSE, vegetation_radius = 10)
mapshot(temp_map, file = "DeBilt_landUse.png", remove_url = TRUE, removeControls = c("zoomControl", "layersControl", "homeButton"))

#single AWS Results
View(singleAWS[["summary"]])
View(singleAWS[["overview_shading_table"]])
View(singleAWS[["complete_shading_table"]])
plot(singleAWS[["shading_chart"]])
singleAWS[["map"]]
View(singleAWS[["land_use_table"]])
View(singleAWS[["vegetation_table"]])


selected_aws <- "Leeuwarden"
#AHN2
View(sAWS[["summary"]])
View(sAWS_ahn3[["summary"]])
View(sAWS[["AWS"]][[selected_aws]][["summary"]])
View(sAWS[["AWS"]][[selected_aws]][["overview_shading_table"]])
View(sAWS[["AWS"]][[selected_aws]][["complete_shading_table"]])
plot(sAWS[["AWS"]][[selected_aws]][["shading_chart"]])
SC_ahn2[["AWS"]][[selected_aws]][["map"]]
mapshot(sAWS[["AWS"]][[selected_aws]][["map"]], file = "test2.png", remove_url = TRUE, removeControls = c("zoomControl", "layersControl", "homeButton"))

View(sAWS[["AWS"]][[selected_aws]][["land_use_table"]])
View(sAWS[["AWS"]][[selected_aws]][["vegetation_table"]])

#AHN3
View(sAWS_ahn3[["summary"]])
View(sAWS_ahn3[["AWS"]][[selected_aws]][["summary"]])
View(sAWS_ahn3[["AWS"]][[selected_aws]][["overview_shading_table"]])
View(sAWS_ahn3[["AWS"]][[selected_aws]][["complete_shading_table"]])
plot(sAWS_ahn3[["AWS"]][[selected_aws]][["shading_chart"]])
sAWS_ahn3[["AWS"]][[selected_aws]][["map"]]
View(sAWS_ahn3[["AWS"]][[selected_aws]][["land_use"]])
View(sAWS_ahn3[["AWS"]][[selected_aws]][["vegetation_height"]])


#all sites
View(SC_ahn2[["summary"]])
View(select(SC_ahn2[["summary"]], c("AWS", "shades_class", "objects_class", "final_class", "Manual_class_R", "Exact match", "Partial match")))



View(SC_ahn3[["summary"]])


## results

results(sAWS, "De Bilt")
results(sAWS_ahn3, "De Bilt")  