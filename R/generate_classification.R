#'generate single temperature SC
#'
#'@title single SC
#'@description generate single temperature SC
#'@return \enumerate{
#'   \item summary:
#'   \item overview_shading_table:
#'   \item complete_shading_table:
#'   \item land_use_table:
#'   \item vegetation table:
#'   \item map:
#' }
generate_classifications <- function(spatialpoint, name, sensor.name, name.supplement = "", AWS = FALSE, LONLAT = FALSE, criteria_columnName = "Criteria_Value", class_selection = "final_class", import_ahn = FALSE, import_bgt = FALSE, redownload_ahn = FALSE, redownload_bgt= FALSE, AHN3 = TRUE, solar_angles = FALSE, angle_selection_byIndexNr = "all", years, months, days, s_hour = 0, f_hour = 23, minutes.interval = 60, ahn_resolution = 0.5, ahn_radius = 500, bgt_radius = 150, include_shadow_angles = TRUE, calculate_shadow_angles = FALSE, radius = 100, read_only_shadow_values = FALSE, extract_method = ' bilinear', full_circle_mask = FALSE, sensor_height = 0, vegetation_radius = 10, exportShp = FALSE, exportCSV = FALSE, printChart = FALSE, delete_ahn_sheets = TRUE, delete_bgt_gmls = TRUE){

  name_trim <- getName_trim(name = name, name.supplement = name.supplement)
  ##shades
    #import AHN
    if(AHN3 == TRUE){
      AHN <- "AHN3"
    } else {
      AHN <- "AHN2"
    }

    if(import_ahn == TRUE){
      downloaded_ahn <- import_single_ahn(spatialpoint,
                        name = name, name.supplement = name.supplement,
                        resolution = ahn_resolution, radius = ahn_radius,
                        raw_ahn = TRUE, terrain_ahn = TRUE,
                        AHN3 = AHN3,
                        delete_sheets = delete_ahn_sheets,
                        redownload = redownload_ahn)
    }
    if(import_ahn == TRUE && redownload_ahn == TRUE){
      if(downloaded_ahn[["ahn"]] == "AHN2"){
        AHN <- "AHN2"
        AHN3 <- FALSE
      }
    }

    #calculate solar and shadow angles
    if(calculate_shadow_angles == TRUE){
      message(paste("Starting angle calculations for", name))
      #calculate solar angles
      if(solar_angles == TRUE){
        my_solar_angles <- multiple_Moments_solarAngles(spatialpoint = spatialpoint, name = name, name.supplement = name.supplement, AWS = AWS, LONLAT = LONLAT,
                                                        sensor.name = sensor.name, angle_selection_byIndexNr = angle_selection_byIndexNr,
                                                        AHN3 = AHN3,
                                                        years = years,
                                                        months = months,
                                                        days = days,
                                                        s_hour = s_hour,
                                                        f_hour = f_hour,
                                                        minutes.interval = minutes.interval,
                                                        exportCSV = exportCSV,
                                                        printChart = printChart
        )
      }

      #select only ah soar angles
      #ah_solar_angles <- dplyr::subset(my_solar_angles, elevation > 0)

      #calculate shadow anfles
      if(calculate_shadow_angles == TRUE){
        my_shadow_angles <- multipleShadowAngles(spatialpoint = spatialpoint, name = name, name.supplement = name.supplement, AWS = AWS, LONLAT = LONLAT,
                             solar_angles = my_solar_angles[["ah angles"]],
                             radius = radius,
                             AHN3 = AHN3,
                             sensor_height = sensor_height,
                             read_only_shadow_values = read_only_shadow_values,
                             extract_method = extract_method,
                             angle_selection_byIndexNr = angle_selection_byIndexNr
        )
      }

      # if(AHN3 == FALSE){
      #   ahn2_analysed <- append(ahn2_analysed, name)
      # } else if(AHN3 == TRUE){
      #   ahn3_analysed <- append(ahn3_analysed, name)
      # }

      print(paste("Completed angle calculations for", name))

    }
    if(include_shadow_angles == TRUE){
      #calcluate classes

      sa_sha_csv_path <- paste0("output/", name_trim,"/solar_shadow_angles/", name_trim, "_", AHN, "_ah_solar_shadow_angles.csv")
      complete_shading_table <- projected_shade_classes(data_path = sa_sha_csv_path, name = name, name.supplement = name.supplement, criteria_columnName = criteria_columnName, AHN3 = AHN3)
      overview_shading_table <- select(complete_shading_table[1,], name, class1_count, class2_count, class3_count, class4_count, class5_count, final_class)
      # create chart
      angles_csv_path <- paste0("output/", name_trim, "/solar_shadow_angles/", name_trim, "_", AHN,"_ah_solar_shadow_angles.csv")
      chart <- sun_shade_angles_chart(data_path = angles_csv_path, name = name, name.supplement = name.supplement, AHN3 = AHN3, extract_method = extract_method)
    } else {
      overview_shading_table <- NA
      complete_shading_table <- NA
      chart <- NA
    }

  ##land use
    #import BGT
    if(import_bgt == TRUE){
      import_single_bgt(spatialpoint = spatialpoint, name = name, sensor.name = sensor.name, name.supplement = name.supplement, radius = bgt_radius, delete_raw_gmls = delete_bgt_gmls, redownload = redownload_bgt)
    }
    #find objects and calculate classifications
    bgt.sf <- st_read(dsn = paste0("datasets/BGT/", name_trim), layer = paste0("BGT_", name_trim))
    bgt.sf <- st_transform(bgt.sf, rgdal::CRSargs(CRS("+init=epsg:28992")))
    presence_objects <- find_land_use_and_classes(name = name, name.supplement = name.supplement, spatialpoint = spatialpoint, bgt.shape = bgt.sf, land_use_criteria.df = spatialpoint[,c(1,5)], criteria_columnName = criteria_columnName, exportCSV = exportCSV, exportShp = exportShp)


  ##vegetation
    #Calculate vegetation height
    vegetation_height <- vegetation_height(spatialpoint = spatialpoint, name = name, name.supplement = name.supplement, radius = vegetation_radius, AHN3 = AHN3, exportCSV = exportCSV)

    #calculalte classes
    vegetation_classes <- vegetation_classes(df = vegetation_height[["df"]], name = name, name.supplement = name.supplement, criteria_columnName = criteria_columnName, AHN3 = AHN3, exportCSV = exportCSV)



  ### create summary classification####
  #shc <- fread(paste0("output/", name_trim, "/solar_shadow_angles/", name_trim, "_", AHN, "_ah_solar_shadow_angles_classes.csv"), data.table = FALSE)
  #luc <- fread(paste0("output/", name_trim, "/land_use/", name_trim, "_landUse_classes.csv"), data.table = FALSE)
  #vhc <- fread(paste0("output/", name_trim, "/vegetation_height/", name_trim, "_", AHN, "_vegetation_height_classes.csv"), data.table = FALSE)

    if(include_shadow_angles == TRUE){
      shc_finalClass <- complete_shading_table[1,class_selection]
    } else {
      shc_finalClass <- NA
    }
    luc_finalClass <- presence_objects[["df"]] [1,class_selection]
  vhc_finalClass <- vegetation_classes[1,class_selection]

  indiv_classes <- c(shc_finalClass, luc_finalClass, vhc_finalClass)

  max_class <- max(indiv_classes)
  #summary.df <- data.frame(AWS = character(0), sensor.name = character(0), shades_class = numeric(0), objects_class = numeric(0), vegetation_height_class = numeric(0), final_class = numeric(0))


  summary.df <- data.frame(name, sensor.name, indiv_classes[1], indiv_classes[2], indiv_classes[3], max_class)
  col_names <- c("name", "sensor", "shades_class", "objects_class", "vegetation_height_class", "final_class")
  colnames(summary.df) <- col_names

  get_manualValue <- function(name){
    manual_class_1 <- dplyr::filter(manual_SC_values, AWS == name)[1,"Manual_TC_1"]
    manual_class_R <- dplyr::filter(manual_SC_values, AWS == name)[1,"Manual_TC_R"]
    if(nchar(manual_class_R > 1)){
      manual_class_Rl <- as.numeric(substr(manual_class_R, 1, 1))
      manual_class_Rh <- as.numeric(substr(manual_class_R, 3, 3))
    } else {
      #manual_class_R <- as.numeric(manual_class_R)
      manual_class_Rl <- as.numeric(manual_class_R)
      manual_class_Rh <- as.numeric(manual_class_R)
    }
    return (list("1" = manual_class_1, "R" = manual_class_R, "Rl" = manual_class_Rl, "Rh" = manual_class_Rh))
  }

  #info.df <- data.frame(manual_class_1 = numeric(), manual_class_R = character(), AHN_selected = character (), extract_method = character(), sensor_height = character(), criteria_values = character())
  manual_classes <- get_manualValue(name)
  manual_class_1 <- manual_classes["1"]
  manual_class_R <- manual_classes["R"]
  manual_class_Rl <- manual_classes["Rl"]
  manual_class_Rh <- manual_classes["Rh"]

  if(max_class == manual_class_1){
    exact_match <- TRUE
  } else {
    exact_match <- FALSE
  }

  if(grepl(max_class, manual_class_R) == TRUE){
    partial_match <- TRUE
  } else {
    partial_match <- FALSE
  }

  AHN_selected <- AHN
  extract_methodology <- extract_method
  sensor_height <- sensor_height
  if(criteria_columnName == "Criteria_Value"){
    criteria_values <- "WM0 Guidelines"
  } else {
    criteria_values <- criteria_columnName
  }

  # if(name %in% AWS_temperature_ahn3Only_names == TRUE){
  #   has_AHN3 <- TRUE
  # } else {
  #   has_AHN3 <- FALSE
  # }

  info.df <- data.frame(manual_class_1,
                        manual_class_R,
                        manual_class_Rl,
                        manual_class_Rh,
                        exact_match,
                        partial_match,
                        AHN_selected,
                        extract_methodology,
                        sensor_height,
                        criteria_values)

  #info.df <- rbind(info.df, info_entry)

  summary.df <- cbind(summary.df, info.df)
  #View(info.df)
  column_names <- c("AWS", "Sensor", "shades_class", "objects_class", "vegetation_height_class", "final_class", "manual_class_1", "manual_class_R", "manual_class_Rl", "manual_class_Rh", "exact_match", "partial_match", "AHN_selected", "extract_method", "sensor_height", "criteria_values")
  colnames(summary.df) <- column_names

  #View(summary.df)

  #map
  map <- map_rd(spatialpoint = spatialpoint, name = name, sensor.name = sensor.name, name.supplement = name.supplement, AWS = AWS, buffers = presence_objects[["buffers"]], vegetation_height_raster = vegetation_height[["height_raster"]], vegetation_radius = vegetation_radius, AHN3 = AHN3, class = summary.df[1,"objects_class"])
  #map
  return (list("summary" = summary.df,
               "overview_shading_table" = overview_shading_table,
               "complete_shading_table" = complete_shading_table ,
               "shading_chart"= chart,
               "land_use_table" = presence_objects[["df"]],
               "vegetation_table" = vegetation_classes,
               "map" = map))
}
#'temperatureSC
#'
#'@title temperature SC
#'@description Determine air temperature SC
#'@return \enumerate{
#'   \item summary: summary table with overall and per critera SC of each analysed location
#'   \item All: same as generate classifcation
#' }
#' @export
determine_temperatureSC <- function(X = NULL, Y = NULL, name, sensor.name, AWS = FALSE, name.supplement = "", LONLAT = FALSE, criteria_columnName = "Criteria_Value", class_selection = "final_class", import_ahn = FALSE, import_bgt = FALSE, redownload_bgt = FALSE, redownload_ahn = FALSE, AHN3 = TRUE, solar_angles = FALSE, angle_selection_byIndexNr = "all", years=2018, months = c(12,1:6), days=c(21), s_hour = 0, f_hour = 23, minutes.interval = 60, ahn_resolution = 0.5, ahn_radius = 500, bgt_radius = 150, include_shadow_angles = FALSE, calculate_shadow_angles = FALSE, read_only_shadow_values = FALSE, extract_method = "bilinear", radius = 100, full_circle_mask = FALSE, sensor_height = 0, vegetation_radius = 10, exportShp = FALSE, exportCSV = FALSE, printChart = FALSE, delete_ahn_sheets = TRUE, delete_bgt_gmls = TRUE, test = FALSE, summary_name.supplement = ""){
  if(AHN3 == TRUE){
    AHN <- "AHN3"
    #name <- AWS_temperature_ahn3Only_names
  } else {
    AHN <- "AHN2"
  }

  if(name.supplement != ""){
    underscore = "_"
  } else {
    underscore = ""
  }
  if(summary_name.supplement != ""){
    summary_name.supplement <- paste0(summary_name.supplement, "_")
  }

  summary_classifcations <- data.frame(name = character(0),
                                       sensor.name = character(0),
                                       shades_class = numeric(0),
                                       objects_class = numeric(0),
                                       vegetation_height_class = numeric(0),
                                       final_class = numeric(0),
                                       manual_class_1 = numeric(0),
                                       manual_class_R = character(0),
                                       manual_class_Rl = numeric(0),
                                       manual_class_Rh = numeric(0),
                                       exact_match = logical(0),
                                       partial_match = logical(0),
                                       AHN_selected = character (0),
                                       extract_method = character(0),
                                       sensor_height = character(0),
                                       criteria_values = character(0)
                                       )
  column_names <- c("name", "sensor", "shades_class", "objects_class", "vegetation_height_class", "final_class", "manual_class_1", "manual_class_R", "manual_class_Rl", "manual_class_Rh", "exact_match", "partial_match", "AHN_selected", "extract_method", "sensor_height", "criteria_values")
  colnames(summary_classifcations) <- column_names

  classifications <- list()
  for (c in 1:length(name)){
    print(" ")
    #select / create spatialpoint
    if(AWS == TRUE){
      LONLAT = FALSE
      spatialpoint <- select_single_aws(name = name[c], sensor.name = sensor.name)
    } else {
      if(length(X) != length(Y) | length(Y) != length(name) | length(X) != length(name)){
        stop("Length of X/Y and/or names are not equal.")
      }
      spatialpoint <- create_spatialPoint(X = X[c], Y = Y[c], LONLAT = LONLAT)
      if(is.null(X) == TRUE | is.null(Y) == TRUE){
        stop("X and/or Y coordinates are missing.")
      }
    }
    print(paste0("Creating classifcations for ", name[c], " with sensor ", sensor.name,"..."))
    single_classifications <- generate_classifications(spatialpoint = spatialpoint, name = name[c], sensor.name = sensor.name, name.supplement = name.supplement, AWS = AWS, LONLAT = LONLAT, criteria_columnName = criteria_columnName, class_selection = class_selection, import_ahn = import_ahn, import_bgt = import_bgt, redownload_bgt = redownload_bgt, redownload_ahn = redownload_ahn, AHN3 = AHN3, solar_angles = solar_angles, angle_selection_byIndexNr  = angle_selection_byIndexNr, years = years, months = months, days = days, s_hour = s_hour, f_hour = f_hour, minutes.interval = minutes.interval, ahn_resolution = ahn_resolution, ahn_radius = ahn_radius, bgt_radius = bgt_radius, include_shadow_angles = include_shadow_angles, calculate_shadow_angles = calculate_shadow_angles, radius = radius, read_only_shadow_values = read_only_shadow_values, extract_method = extract_method,full_circle_mask = full_circle_mask, sensor_height = sensor_height,vegetation_radius = vegetation_radius, exportShp = exportShp, exportCSV = exportCSV, printChart = printChart, delete_ahn_sheets = delete_ahn_sheets, delete_bgt_gmls = delete_bgt_gmls)
    #View(classifcations)
    # manual_class <- get_manualValue(name[c])
    # AHN_selected <- AHN
    # extract_methodology <- extract_method
    # sensor_height <- sensor_height

    classifications[[name[c]]] <- single_classifications
    summary_classifcations <- rbind(summary_classifcations, single_classifications[["summary"]][1,])
    print(paste("Created classifications successfully for", length(name), "stations."))
    print(" ")
    print("===========================")
    if(c == length(name)){
      data.table::fwrite(summary_classifcations, file = paste0("output/", summary_name.supplement, name.supplement, underscore, AHN, "_summary_aws_classifications.csv"))
    }
  }

  #View(summary_classifcations)

  if(length(name) > 1){
      return (list("summary"=summary_classifcations,
                   "All" = classifications)
              )
  } else if(length(name) == 1){
      map <- single_classifications[["map"]]
      map
      return(list("summary" = single_classifications[["summary"]],
                  "overview_shading_table" = single_classifications[["overview_shading_table"]],
                  "complete_shading_table" = single_classifications[["complete_shading_table"]],
                  "shading_chart" = single_classifications[["shading_chart"]],
                  "land_use_table" = single_classifications[["land_use_table"]],
                  "vegetation_table" = single_classifications[["vegetation_table"]],
                  "map" = single_classifications[["map"]])
             )
  } else {
    message("error occured.")
  }
}
