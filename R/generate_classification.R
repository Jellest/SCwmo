generate_classifications <- function(aws.df = AWS.df, aws_name, sensor_name, criteria_columnName = "Criteria_Value", import_ahn = FALSE, import_bgt = FALSE, redownload_ahn = FALSE, redownload_bgt= FALSE, AHN3 = FALSE, solar_angles = FALSE, angle_selection_byIndexNr = "all", years, months, days, s_hour = 0, f_hour = 23, minutes_interval = 60, ahn_resolution = 0.5, ahn_radius = 500, bgt_radius = 150, calculate_shadow_angles = FALSE, shadow_radius = 300, read_only_shadow_values = FALSE, extract_method = ' bilinear', full_circle_mask = FALSE, vegetation_radius = 10, exportShp = FALSE, exportCSV = FALSE, printChart = FALSE, delete_ahn_sheets = TRUE, delete_bgt_gmls = TRUE){
  aws_name_trim <- getAWS_name_trim(aws.df = aws.df, aws_name = aws_name)
  
  if(AHN3 == TRUE){
    AHN <- "AHN3"
  } else {
    AHN <- "AHN2"
  }
  
  ##shades
  #import AHN
  if(import_ahn == TRUE){
    multiple_import_ahn(aws.df = aws.df,
                        aws_list = c(aws_name),
                        sensor_name = sensor_name,
                        radius = ahn_radius,
                        resolution = ahn_resolution,
                        AHN3 = AHN3,
                        raw_ahn = TRUE, terrain_ahn = TRUE, 
                        delete_sheets = delete_ahn_sheets,
                        redownload_ahn = redownload_ahn)
  }
  #calculate solar angles
  if(solar_angles == TRUE){
    
  }
  #calculate solar and shadow angles
  if(calculate_shadow_angles == TRUE){
    multipleSolar_shadow_angles(aws.df = aws.df,
                                aws_list = c(aws_name),
                                sensor_name = sensor_name, angle_selection_byIndexNr = angle_selection_byIndexNr,
                                years = years,
                                months = months,
                                days = days,
                                s_hour = s_hour,
                                f_hour = f_hour,
                                minutes_interval = minutes_interval,
                                radius = shadow_radius,
                                AHN3 = AHN3,
                                read_only_shadow_values = read_only_shadow_values,
                                extract_method = extract_method,
                                full_circle_mask = full_circle_mask,
                                printChart = printChart,
                                exportCSV = exportCSV)
  }
  #calcluate classes
  sa_sha_csv_path <- paste0("output/", aws_name_trim,"/solar_shadow_angles/", aws_name_trim, "_", AHN, "_ah_solar_shadow_angles.csv")
  shading_table <- projected_shade_class(aws.df = aws.df, data_path = sa_sha_csv_path, aws_name = aws_name, criteria_columnName = criteria_columnName, AHN3 = AHN3)
  
  # create chart
  angles_csv_path <- paste0("output/", aws_name_trim, "/solar_shadow_angles/", aws_name_trim, "_", AHN,"_ah_solar_shadow_angles.csv") 
  chart <- sun_shade_angles_chart(aws.df = aws.df, data_path = angles_csv_path, aws_name = aws_name, AHN3 = AHN3, extract_method = extract_method)
  
  
  ##land use
  #import BGT
  if(import_bgt == TRUE){
    import_single_bgt(aws.df = aws.df, aws_name = aws_name, sensor_name = sensor_name, radius = bgt_radius, delete_raw_gmls = delete_bgt_gmls, redownload = redownload_bgt)
  }
  #find objects and calculate classifications
  presence_objects <- multiple_intersect_bgt(aws.df = aws.df, aws_list = c(aws_name), sensor_name = sensor_name, criteria_columnName = criteria_columnName, exportCSV = exportCSV, exportShp = exportShp)
  
  
  ##vegetation
  #Calculate vegetation height
  vegetation_height <- vegetation_height(aws.df = aws.df, aws_name = aws_name, sensor_name = sensor_name, radius = vegetation_radius, AHN3 = AHN3, exportCSV = exportCSV)
  
  #calculalte classes
  vegetation_classes(aws.df = aws.df, df = vegetation_height[["df"]], aws_name = aws_name, criteria_columnName = criteria_columnName, AHN3 = AHN3, exportCSV = exportCSV)
  
  #create summary classification
  summary <- summary_classifcation(aws.df = aws.df, aws_name = aws_name, sensor_name = sensor_name, AHN3 = AHN3)
  
  #View(summary)
  return (list("summary" = summary, "ssc"=chart))#, "sst"= shading_tables))
}

summary_classifcation <- function(aws.df = AWS.df, aws_name, sensor_name, class_selection = "final_class", AHN3 = FALSE){
  if(AHN3 == TRUE){
    AHN <- "AHN3"
  } else {
    AHN <- "AHN2"
  }
  
  aws_name_trim <- getAWS_name_trim(aws.df = aws.df, aws_name = aws_name)
  
  shc <- fread(paste0("output/", aws_name_trim, "/solar_shadow_angles/", aws_name_trim, "_", AHN, "_ah_solar_shadow_angles_classes.csv"), data.table = FALSE)
  luc <- fread(paste0("output/", aws_name_trim, "/land_use/", aws_name_trim, "_landUse_classes.csv"), data.table = FALSE)
  vhc <- fread(paste0("output/", aws_name_trim, "/vegetation_height/", aws_name_trim, "_", AHN, "_vegetation_height_classes.csv"), data.table = FALSE)
  
  shc_finalClass <- shc[1,class_selection] 
  luc_finalClass <- luc[1,class_selection]
  vhc_finalClass <- vhc[1,class_selection]
  
  indiv_classes <- c(shc_finalClass, luc_finalClass, vhc_finalClass)
  
  max_class <- max(indiv_classes)
  df <- data.frame(AWS = character(0), sensor_name = character(0), shades_class = numeric(0), objects_class = numeric(0), vegetation_height_class = numeric(0), final_class = numeric(0))
  column_names <- c("AWS", "Sensor", "shades_class", "objects_class", "vegetation_height_class", "final_class")
  colnames(df) <- column_names
  
  results <- data.frame(aws_name, sensor_name, indiv_classes[1], indiv_classes[2], indiv_classes[3], max_class)
  names(results) <- column_names
  
  df <- rbind(df, results)
  return(df)
}

create_temperature_SC <- function(aws.df = AWS.df, aws_list, sensor_name, criteria_columnName = "Criteria_Value", import_ahn = FALSE, import_bgt = FALSE, redownload_bgt = FALSE, redownload_ahn = FALSE, AHN3 = FALSE, solar_angles = FALSE, angle_selection_byIndexNr = "all", years, months, days, s_hour = 0, f_hour = 23, minutes_interval = 60, ahn_resolution = 0.5, ahn_radius = 500, bgt_radius = 150, calculate_shadow_angles = FALSE, read_only_shadow_values = FALSE, extract_method = ' bilinear', shadow_radius = 300, full_circle_mask = FALSE, vegetation_radius = 10, exportShp = FALSE, exportCSV = FALSE, printChart = FALSE, delete_ahn_sheets = TRUE, delete_bgt_gmls = TRUE){
  if(AHN3 == TRUE){
    AHN <- "AHN3"
  } else {
    AHN <- "AHN2"
  }
  summary_classifcations <- data.frame(AWS = character(0), sensor_name = character(0), shades_class = numeric(0), objects_class = numeric(0), vegetation_height_class = numeric(0), final_class = numeric(0), manual_class = numeric(0))
  column_names <- c("AWS", "Sensor", "shades_class", "objects_class", "vegetation_height_class", "final_class", "manual_class")
  colnames(summary_classifcations) <- column_names
  
  get_manualValue <- function(aws_name){
    manual_class <- dplyr::filter(manual_SC_values, AWS == aws_name)[1,"Manual_Temperature_SC"]
    return (manual_class)
  }
  for (c in 1:length(aws_list)){
    print(" ")
    aws <- check_aws_names(aws.df = aws.df, aws_name = aws_list[c], sensor_name = sensor_name)
    aws_name <- aws[1,"AWS"]
    sensor_name <- aws[1,"Sensor"]
    print(paste0("Creating classifcations for ", aws_list[c], " with sensor ", sensor_name,"..."))
    classifications <- generate_classifications(aws.df = aws.df, aws_name = aws_list[c], sensor_name = sensor_name, criteria_columnName = criteria_columnName, import_ahn = import_ahn, import_bgt = import_bgt, redownload_bgt = redownload_bgt, redownload_ahn = redownload_ahn, AHN3 = AHN3, solar_angles = solar_angles, angle_selection_byIndexNr  = angle_selection_byIndexNr, years = years, months = months, days = days, s_hour = s_hour, f_hour = f_hour, minutes_interval = minutes_interval, ahn_resolution = ahn_resolution, ahn_radius = ahn_radius, bgt_radius = bgt_radius, calculate_shadow_angles = calculate_shadow_angles, shadow_radius = shadow_radius, read_only_shadow_values = read_only_shadow_values, extract_method = extract_method,full_circle_mask = full_circle_mask, vegetation_radius = vegetation_radius, exportShp = exportShp, exportCSV = exportCSV, printChart = printChart, delete_ahn_sheets = delete_ahn_sheets, delete_bgt_gmls = delete_bgt_gmls)
    #View(classifcations)
    manual_class <- get_manualValue(aws_list[c])
    AHN_selected <- AHN
    extract_methodology <- extract_method
    if(criteria_columnName == "Criteria_Value"){
      criteria_values <- "WM0 Guidelines"
    } else {
      criteria_values <- criteria_columnName
    }
 
    entry <- classifications[["summary"]][1,] 
  
    entry <- cbind(entry, manual_class, AHN_selected)#, extract_methodology, criteria_values)
    summary_classifcations <- rbind(summary_classifcations, entry)
    print("Created classifications successfully.")
    print(" ")
    print("===========================")
    if(c == length(aws_list)){
      fwrite(summary_classifcations, file = paste0("output/", AHN, "_summary_classifcations.csv"))
    }  
  }
  View(summary_classifcations)
  return (list("sc"=summary_classifcations))#, "ssa"=classifcations[["ssc"]]))
}

Cabauw_temperature_SC <- create_temperature_SC (aws.df = AWS_test.df,
                                      aws_list = c("Cabauw mast"),
                                      sensor_name = temperature_sensor_name, criteria_columnName = "Criteria_Value",
                                      AHN3 = FALSE, import_ahn = TRUE, redownload_ahn = FALSE, ahn_resolution = 0.5, ahn_radius = 500, delete_ahn_sheets = TRUE,
                                      import_bgt = TRUE, redownload_bgt = FALSE, bgt_radius = 150, delete_bgt_gmls = TRUE,
                                      solar_angles = TRUE, angle_selection_byIndexNr = c(1,20),
                                      years = c(2018), months = c(12, 1:6), days = c(21),
                                      s_hour = 0, f_hour = 23, minutes_interval = 15,
                                      calculate_shadow_angles = TRUE, read_only_shadow_values = FALSE,
                                      shadow_radius = 300, full_circle_mask = FALSE, extract_method = 'bilinear',
                                      vegetation_radius = 10,
                                      exportShp = FALSE, exportCSV = TRUE, printChart = FALSE)

