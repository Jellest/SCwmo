generate_classifications <- function(aws.df = AWS.df, aws_name, sensor_name, addition = "", criteria_columnName = "Criteria_Value", class_selection = "final_class", import_ahn = FALSE, import_bgt = FALSE, redownload_ahn = FALSE, redownload_bgt= FALSE, AHN3 = FALSE, solar_angles = FALSE, angle_selection_byIndexNr = "all", years, months, days, s_hour = 0, f_hour = 23, minutes_interval = 60, ahn_resolution = 0.5, ahn_radius = 500, bgt_radius = 150, include_shadow_angles = TRUE, calculate_shadow_angles = FALSE, shadow_radius = 300, read_only_shadow_values = FALSE, extract_method = ' bilinear', full_circle_mask = FALSE, sensor_height = 0, vegetation_radius = 10, exportShp = FALSE, exportCSV = FALSE, printChart = FALSE, delete_ahn_sheets = TRUE, delete_bgt_gmls = TRUE){
  if(AHN3 == TRUE){
    AHN <- "AHN3"
  } else {
    AHN <- "AHN2"
  }
  
  aws_name_trim <- getAWS_name_trim(aws.df = aws.df, aws_name = aws_name, addition = addition)
  single_aws <- select_single_aws(aws.df = aws.df, aws_name = aws_name, sensor_name = sensor_name)

  
  ##shades
    #import AHN
    if(import_ahn == TRUE){
      import_single_ahn(aws.df = aws.df, 
                        aws_name = aws_name, addition = addition,
                        station_coords = single_aws[["aws_rd.sp"]],#station_coords = selected_aws_temp[["aws_rd.sp"]],
                        LONLAT = LONLAT,
                        resolution = resolution, radius = radius,
                        raw_ahn = TRUE, terrain_ahn = TRUE,
                        AHN3 = AHN3,
                        delete_sheets = delete_sheets,
                        redownload = redownload_ahn)
    }
    #calculate solar angles
    if(solar_angles == TRUE){
      
    }
    #calculate solar and shadow angles
    if(calculate_shadow_angles == TRUE){
      multipleSolar_shadow_angles(aws.df = aws.df,
                                  aws_list = c(aws_name), addition = addition,
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
                                  sensor_height = sensor_height,
                                  extract_method = extract_method,
                                  full_circle_mask = full_circle_mask,
                                  printChart = printChart,
                                  exportCSV = exportCSV)
    }
    if(include_shadow_angles == TRUE){
      #calcluate classes
      
      sa_sha_csv_path <- paste0("output/", aws_name_trim,"/solar_shadow_angles/", aws_name_trim, "_", AHN, "_ah_solar_shadow_angles.csv")
      shading_table <- projected_shade_classes(aws.df = aws.df, data_path = sa_sha_csv_path, aws_name = aws_name, addition = addition, criteria_columnName = criteria_columnName, AHN3 = AHN3)
      
      # create chart
      angles_csv_path <- paste0("output/", aws_name_trim, "/solar_shadow_angles/", aws_name_trim, "_", AHN,"_ah_solar_shadow_angles.csv") 
      chart <- sun_shade_angles_chart(aws.df = aws.df, data_path = angles_csv_path, aws_name = aws_name, addition = addition, AHN3 = AHN3, extract_method = extract_method)
    } else {
      shading_table <- NA
      chart <- NA
    }
  
  ##land use
    #import BGT
    if(import_bgt == TRUE){
      import_single_bgt(aws.df = aws.df, aws_name = aws_name, sensor_name = sensor_name, addition = addition, radius = bgt_radius, delete_raw_gmls = delete_bgt_gmls, redownload = redownload_bgt)
    }
    #find objects and calculate classifications
    bgt.sp <- st_read(dsn = paste0("data/BGT/", aws_name_trim), layer = paste0("BGT_", aws_name_trim)) 
    bgt.sf <- st_transform(bgt.sp, epsg_rd)
  
    presence_objects <- find_land_use_and_classes(aws.df = aws.df, aws_name = aws_name, addition = addition, coords = single_aws[["aws_rd.sf"]], bgt_shape = bgt.sf, land_use_criteria.df = single_aws[["aws.df"]][,c(1,5)], criteria_columnName = criteria_columnName, exportCSV = exportCSV, exportShp = exportShp)
  
  
  ##vegetation
    #Calculate vegetation height
    vegetation_height <- vegetation_height(aws.df = aws.df, aws_name = aws_name, sensor_name = sensor_name, addition = addition, radius = vegetation_radius, AHN3 = AHN3, exportCSV = exportCSV)
    
    #calculalte classes
    vegetation_classes <- vegetation_classes(aws.df = aws.df, df = vegetation_height[["df"]], aws_name = aws_name, addition = addition, criteria_columnName = criteria_columnName, AHN3 = AHN3, exportCSV = exportCSV)
  
  
  
  ### create summary classification####
  #shc <- fread(paste0("output/", aws_name_trim, "/solar_shadow_angles/", aws_name_trim, "_", AHN, "_ah_solar_shadow_angles_classes.csv"), data.table = FALSE)
  #luc <- fread(paste0("output/", aws_name_trim, "/land_use/", aws_name_trim, "_landUse_classes.csv"), data.table = FALSE)
  #vhc <- fread(paste0("output/", aws_name_trim, "/vegetation_height/", aws_name_trim, "_", AHN, "_vegetation_height_classes.csv"), data.table = FALSE)
  
    if(include_shadow_angles == TRUE){
      shc_finalClass <- shading_table[1,class_selection] 
    } else {
      shc_finalClass <- NA
    }
    luc_finalClass <- presence_objects[["df"]] [1,class_selection]
  vhc_finalClass <- vegetation_classes[1,class_selection]
  
  indiv_classes <- c(shc_finalClass, luc_finalClass, vhc_finalClass)
  
  max_class <- max(indiv_classes)
  #summary.df <- data.frame(AWS = character(0), sensor_name = character(0), shades_class = numeric(0), objects_class = numeric(0), vegetation_height_class = numeric(0), final_class = numeric(0))
  

  summary.df <- data.frame(aws_name, sensor_name, indiv_classes[1], indiv_classes[2], indiv_classes[3], max_class)
  col_names <- c("AWS", "Sensor", "shades_class", "objects_class", "vegetation_height_class", "final_class")
  colnames(summary.df) <- col_names
  
  get_manualValue <- function(aws_name){
    manual_class_1 <- dplyr::filter(manual_SC_values, AWS == aws_name)[1,"Manual_TC_1"]
    manual_class_R <- dplyr::filter(manual_SC_values, AWS == aws_name)[1,"Manual_TC_R"]
    return (list("1" = manual_class_1, "R" = manual_class_R))
  }
  
  #info.df <- data.frame(manual_class_1 = numeric(), manual_class_R = character(), AHN_selected = character (), extract_method = character(), sensor_height = character(), criteria_values = character())
  manual_classes <- get_manualValue(aws_name)
  manual_class_1 <- manual_classes["1"]
  manual_class_R <- manual_classes["R"]
  
  AHN_selected <- AHN
  extract_methodology <- extract_method
  sensor_height <- sensor_height
  
  if(criteria_columnName == "Criteria_Value"){
    criteria_values <- "WM0 Guidelines"
  } else {
    criteria_values <- criteria_columnName
  }
  
  info.df <- data.frame(manual_class_1, manual_class_R, AHN_selected, extract_methodology, sensor_height, criteria_values)
  
  #info.df <- rbind(info.df, info_entry)
  
  summary.df <- cbind(summary.df, info.df)
  #View(info.df)
  column_names <- c("AWS", "Sensor", "shades_class", "objects_class", "vegetation_height_class", "final_class", "Manual_class_1", "Manual_class_R", "AHN selected", "Extract method", "Sensor height", "Criteria values")
  colnames(summary.df) <- column_names
  
  #View(summary.df)
  
  #map
  map <- map_rd(aws_name = aws_name, sensor_name = sensor_name, addition = addition, buffers = presence_objects[["buffers"]], vegetation_height_raster = vegetation_height[["height_raster"]], vegetation_radius = vegetation_radius, AHN3 = AHN3, aws.df = aws.df, class = summary.df[1,"final_class"])
  #map
  return (list("summary" = summary.df, "shading_table" = shading_table , "shading_chart"= chart, "land_use_table" = presence_objects[["df"]], "vegetation_table" = vegetation_classes, "map" = map))
}

create_temperature_SC <- function(aws.df = AWS.df, aws_list, sensor_name, addition = "", criteria_columnName = "Criteria_Value", class_selection = "final_class", import_ahn = FALSE, import_bgt = FALSE, redownload_bgt = FALSE, redownload_ahn = FALSE, AHN3 = FALSE, solar_angles = FALSE, angle_selection_byIndexNr = "all", years, months, days, s_hour = 0, f_hour = 23, minutes_interval = 60, ahn_resolution = 0.5, ahn_radius = 500, bgt_radius = 150, include_shadow_angles = FALSE, calculate_shadow_angles = FALSE, read_only_shadow_values = FALSE, extract_method = ' bilinear', shadow_radius = 300, full_circle_mask = FALSE, sensor_height = 0, vegetation_radius = 10, exportShp = FALSE, exportCSV = FALSE, printChart = FALSE, delete_ahn_sheets = TRUE, delete_bgt_gmls = TRUE, test = FALSE, summary_addition = ""){
  if(AHN3 == TRUE){
    AHN <- "AHN3"
  } else {
    AHN <- "AHN2"
  }
  if(addition != ""){
    addition = paste0(addition, "_")
  }
  if(summary_addition != ""){
    summary_addition <- paste0(summary_addition, "_")
  }
  
  summary_classifcations <- data.frame(AWS = character(0), sensor_name = character(0), shades_class = numeric(0), objects_class = numeric(0), vegetation_height_class = numeric(0), final_class = numeric(0), manual_class_1 = numeric(0), manual_class_R = character(0), AHN_selected = character (0), extract_method = character(0), sensor_height = character(0), criteria_values = character(0))
  column_names <- c("AWS", "Sensor", "shades_class", "objects_class", "vegetation_height_class", "final_class", "Manual_class_1", "Manual_class_R", "AHN selected", "Extract method", "Sensor height", "Criteria values")
  colnames(summary_classifcations) <- column_names
  
  ahn2_analysed <- c("De Bilt", "Rotterdam", "Arcen", "Vlissingen", "Wijkaanzee", "Voorschoten", "Berkhout", "Cabauwmast")
  aws_classifications <- list()
  for (c in 1:length(aws_list)){
    print(" ")
    aws <- check_aws_names(aws.df = aws.df, aws_name = aws_list[c], sensor_name = sensor_name)
    aws_name <- aws[1,"AWS"]
    sensor_name <- aws[1,"Sensor"]
    if(aws_name %in% ahn2_analysed == TRUE & test == FALSE){
      message(paste(aws_name, "is already analysed. Going to the next station."))
      next
    }
    print(paste0("Creating classifcations for ", aws_list[c], " with sensor ", sensor_name,"..."))
    single_aws_classifications <- generate_classifications(aws.df = aws.df, aws_name = aws_list[c], sensor_name = sensor_name, addition = addition, criteria_columnName = criteria_columnName, class_selection = class_selection, import_ahn = import_ahn, import_bgt = import_bgt, redownload_bgt = redownload_bgt, redownload_ahn = redownload_ahn, AHN3 = AHN3, solar_angles = solar_angles, angle_selection_byIndexNr  = angle_selection_byIndexNr, years = years, months = months, days = days, s_hour = s_hour, f_hour = f_hour, minutes_interval = minutes_interval, ahn_resolution = ahn_resolution, ahn_radius = ahn_radius, bgt_radius = bgt_radius, include_shadow_angles = include_shadow_angles, calculate_shadow_angles = calculate_shadow_angles, shadow_radius = shadow_radius, read_only_shadow_values = read_only_shadow_values, extract_method = extract_method,full_circle_mask = full_circle_mask, sensor_height = sensor_height,vegetation_radius = vegetation_radius, exportShp = exportShp, exportCSV = exportCSV, printChart = printChart, delete_ahn_sheets = delete_ahn_sheets, delete_bgt_gmls = delete_bgt_gmls)
    #View(classifcations)
    # manual_class <- get_manualValue(aws_list[c])
    # AHN_selected <- AHN
    # extract_methodology <- extract_method
    # sensor_height <- sensor_height

    aws_classifications[[aws_list[c]]] <- single_aws_classifications 
    summary_classifcations <- rbind(summary_classifcations, single_aws_classifications[["summary"]][1,])
    print(paste("Created classifications successfully for", length(aws_list), "stations."))
    print(" ")
    print("===========================")
    if(c == length(aws_list)){
      fwrite(summary_classifcations, file = paste0("output/", summary_addition, addition, AHN, "_summary_aws_classifcations.csv"))
    }  
  }
  
  ahn2_analysed <- cbind(ahn2_analysed, aws_name)
  View(summary_classifcations)

  if(length(aws_list) > 1){
      return (list("summary"=summary_classifcations,
                   "AWS" = aws_classifications)
              )
  } else if(length(aws_list) == 1){
      map <- single_aws_classifications[["map"]]
      map
      return(list("summary" = single_aws_classifications[["summary"]],
                  "shading_table" = single_aws_classifications[["shading_table"]],
                  "shading_chart" = single_aws_classifications[["shading_chart"]],
                  "land_use_table" = single_aws_classifications[["land_use_table"]],
                  "vegation_height" = single_aws_classifications[["vegetation_table"]],
                  "map" = single_aws_classifications[["map"]])
             )
  } else {
    message("error occured.")
  }
}

create_temperature_SC(aws.df = AWS.df,
                      aws_list = c("Wijk aan zee"), addition = "", summary_addition = "", 
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
                      exportShp = TRUE, exportCSV = TRUE, printChart = FALSE, test = TRUE)

