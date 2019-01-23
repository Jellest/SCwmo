##multiple angles
multiple_Moments_solarAngles <- function(aws.df = AWS.df, aws_name, sensor_name, AHN3 = FALSE, years, months, days, s_hour = 0, f_hour = 23, minutes_interval = 60, angle_selection_byIndexNr = "all", exportCSV = FALSE, printChart = FALSE){
  if(AHN3 == TRUE){
    AHN <- "AHN3"
  } else {
    AHN <- "AHN2"
  }
  month_angles <- list()
  aws <- select_single_aws(aws.df = aws.df, aws_name = aws_name, sensor_name = sensor_name)
  #print(aws[["aws_wgs.sp"]]@data$LAT)
  
  aws_name_trim <- getAWS_name_trim(aws.df = aws.df, aws_name = aws_name)
  if(!dir.exists(paste0("output/", aws_name_trim))){
    dir.create(paste0("output/", aws_name_trim), showWarnings = FALSE)
  }
  if(!dir.exists(paste0("output/", aws_name_trim, "/solar_shadow_angles"))){
    dir.create(paste0("output/", aws_name_trim, "/solar_shadow_angles"))
  }
  dir.create(paste0("output/", aws_name_trim, "/solar_shadow_angles/rasters"), showWarnings = FALSE)
  dir.create(paste0("output/", aws_name_trim, "/solar_shadow_angles/rasters/", AHN), showWarnings = FALSE)
  dir.create(paste0("output/", aws_name_trim, "/solar_shadow_angles/rasters/", AHN, "/Masks"), showWarnings = FALSE)
  dir.create(paste0("output/", aws_name_trim, "/solar_shadow_angles/rasters/", AHN, "/Shadows"), showWarnings = FALSE)
  
  all_solar_angles <-  data.frame(LAT = numeric(0), LON = numeric(0), julian_day = numeric(0), azimuth = numeric(0), zenith = numeric(0), elevation = numeric(0), stringsAsFactors = FALSE)
  
  for(y in 1:length(years)){   
    for(m in 1:length(months)){ 
      if(months[m] == 1){
        month_name = "jan"
      }
      if(months[m] == 2){
        month_name = "feb"
      }
      if(months[m] == 3){
        month_name = "mar"
      }
      if(months[m] == 4){
        month_name = "apr"
      }
      if(months[m] == 5){
        month_name = "may"
      }
      if(months[m] == 6){
        month_name = "jun"
      }
      if(months[m] == 7){
        month_name = "jul"
      }
      if(months[m] == 8){
        month_name = "aug"
      }
      if(months[m] == 9){
        month_name = "sep"
      }
      if(months[m] == 10){
        month_name = "oct"
      }
      if(months[m] == 11){
        month_name = "nov"
      }
      if(months[m] == 12){
        month_name = "dec"
      }
      for(d in 1:length(days)){
        month_angles[[month_name]] <- solar_angles(X = aws[["aws_wgs.sp"]]@data$LON,
                                                   Y = aws[["aws_wgs.sp"]]@data$LAT,
                                                   year = years[y],
                                                   month = months[m],
                                                   day = days[d],
                                                   s_hour = s_hour,
                                                   f_hour = f_hour,
                                                   minutes_interval = minutes_interval,
                                                   LONLAT = TRUE)
        all_solar_angles <- rbind(all_solar_angles, month_angles[[month_name]][["all angles"]])
      }
    }
  }
  
  aws_info <- data.frame(AWS = character(nrow(all_solar_angles)), sensor_name = character(nrow(all_solar_angles)), X = numeric(nrow(all_solar_angles)), Y = numeric(nrow(all_solar_angles)))
  aws_info$AWS <- aws_name
  aws_info$sensor_name <- sensor_name
  aws_info$X <- aws[["aws_rd.sp"]]@coords[,"X"]
  aws_info$Y <- aws[["aws_rd.sp"]]@coords[,"Y"]
  all_solar_angles <- cbind(aws_info, all_solar_angles)
  #View(all_solar_angles)
  
  all_ah_solar_angles <- subset(all_solar_angles, elevation > 0)
  if(nrow(all_ah_solar_angles) == 0){
    warning(paste0("No positive elevation angles found for ", aws_name, "."))
  }
  if(angle_selection_byIndexNr != "all"){
    all_ah_solar_angles <- all_ah_solar_angles[angle_selection_byIndexNr,]
  }
  
  if(exportCSV == TRUE){
    #dir.create("output/solar_shadow_angles", getAWS_name_trim(aws_name))
    fwrite(x = all_solar_angles, file = paste0("output/", aws_name_trim, "/solar_shadow_angles/", aws_name_trim, "_solar_angles.csv"))
    fwrite(x = all_ah_solar_angles, file = paste0("output/", aws_name_trim, "/solar_shadow_angles/", aws_name_trim, "_ah_solar_angles.csv"))
    print("Exported solar angles to output folder.")
  }
  
  #all_ah_solar_angles_melt <- reshape::melt(all_ah_angles, id = "julian_day")
  #shadow_angles <- fread("data/solar_shadow_angles/DeBilt_shadow_angles.csv", data.table = FALSE)
  
  # if(printChart == TRUE){
  #   sun_shade_angles_chart(month_angles, shadow_angles = shadow_angles)
  # }
  return(list("all angles" = all_solar_angles, "all ah angles" = all_ah_solar_angles, "all month angles" = month_angles))
}

multipleShadowAngles <- function(aws.df = AWS.df, solar_angles, radius, AHN3 = FALSE, read_only_shadow_values = FALSE, extract_method = 'bilinear', full_circle_mask = FALSE, printChart = FALSE){
  ah_solar_shadow_angles <- data.frame(AWS = character(0), sensor_name = character(0), X = numeric(0), Y = numeric(0), LON = numeric(0), LAT = numeric(0), altitude = numeric(0), julian_day = numeric(0), azimuth = numeric(0), zenith = numeric(0), elevation = numeric(0), shadow_height = numeric(0), shadow_angle = numeric(0), stringsAsFactors = FALSE)
  ah_solar_shadow_angles <- ah_solar_shadow_angles[,c("LON", "LAT", "altitude", "julian_day", "azimuth", "zenith", "elevation", "shadow_height", "shadow_angle")]
  ah_shadow_rasters <- list()
  
  aws_name <- solar_angles$AWS[1]
  aws_name_trim <- getAWS_name_trim(aws.df = aws.df, aws_name = aws_name)
  
  azimuths <- solar_angles$azimuth
  start_time <- Sys.time()
  spatialpoint <- create_SpatialPoint(solar_angles[1,"X"], Y = solar_angles[1,"Y"], LONLAT = FALSE)
  if(AHN3 == TRUE){
    AHN <- "AHN3"
  } else {
    AHN <- "AHN2"
  }
  aws_path <- paste0("data/", AHN, "/", aws_name_trim, "/raw/", aws_name_trim, "_", AHN,"_raw_ahn.tif")
  #print(aws_path)
  aws_ahn <- raster(aws_path)
  
  if(full_circle_mask == TRUE){
    ahn_mask <- simple_mask_raster(aws.df = aws.df, spatialpoint = spatialpoint[["point_rd.sp"]], ahn = aws_ahn, radius = radius, aws_name = aws_name)
  }
  for (a in 1:length(azimuths)){
    print(paste("Calculating shadow angle for azimuth angle: ", azimuths[a],". ", a, " out of ", length(azimuths),"...", sep=""))
    if(full_circle_mask == FALSE){
      ahn_mask <- mask_raster(aws.df = aws.df, aws_name = aws_name, spatialpoint = spatialpoint[["point_rd.sp"]], ahn_raster = aws_ahn, AHN3 = AHN3, azimuth = azimuths[a],radius = radius)
    }
    #plot(ahn_mask)
    #View(ahn_mask)
    # print(solar_angles[a, "X"])
    # print(solar_angles[a, "Y"])
    # print(spatialpoint[["point_rd.sp"]]@coords[,"X"])
    shadow_ha <-data.frame(shadow_angles(aws.df = aws.df, aws_name = aws_name, spatialpoint = spatialpoint[["point_rd.sp"]], ahn_mask = ahn_mask, angle = azimuths[a], maxDist = radius, LONLAT = FALSE, AHN3 = AHN3, extract_method = extract_method, read_only = read_only_shadow_values)$df)
    
    #heightShadow <- shadow_ha$height[1]
    #shad_angle <- shadow_ha$elevation[1]
    #View(shadow_ha)
    #ah_shadow_rasters[a] <- shadow_angles$shadows 
    #writeRaster(ah_shadow_rasters, paste0("output/rasters/",aws_name_trims[a],"")
    #sodf <- solar_angles[a,]
    #shdf <- data.frame(shadow_height = heightShadow, shadow_angle = shad_angle)
    soshdf <- merge(solar_angles[a,], shadow_ha)
    ah_solar_shadow_angles <- rbind(ah_solar_shadow_angles, soshdf)
    fwrite(ah_solar_shadow_angles, paste0("output/", aws_name_trim, "/solar_shadow_angles/",aws_name_trim, "_", AHN, "_ah_solar_shadow_angles.csv"))
    if(a == length(azimuths)){
      #ah_solar_shadow_angles <- ah_solar_shadow_angles[,c("lat", "lon", "altitude", "julian_day", "azimuth", "zenith", "elevation", "shadow_height", "shadow_angle")]
      export_path <- paste0("output/", aws_name_trim, "/solar_shadow_angles/", aws_name_trim, "_", AHN, "_ah_solar_shadow_angles_backup.csv") 
      fwrite(ah_solar_shadow_angles, export_path)
      if(printChart == TRUE){
        sun_shade_angles_chart(data_path = export_path, aws_name = aws_name) 
      }
    }
  }
  end_time <- Sys.time()
  elapsed_time <- ceiling(end_time - start_time)
  message(paste("Finished angle calculations for", aws_name, ". Elapsed Time:", elapsed_time, "seconds."))
}

multipleSolar_shadow_angles <- function(aws.df = AWS.df, aws_list, sensor_name, solar_angles = TRUE, angle_selection_byIndexNr = "all", calculate_shadow_angles = TRUE, years, months, days, s_hour = 0, f_hour = 23, minutes_interval = 60, radius, AHN3 = FALSE, read_only_shadow_values = FALSE, extract_method = 'bilinear', full_circle_mask = FALSE, printChart = FALSE, exportCSV = FALSE){
  start_time <- Sys.time()
  for(a in 1:length(aws_list)){
    single_aws.df <- dplyr::filter(aws.df, AWS == aws_list[a] & Sensor == sensor_name)
    aws_name <- single_aws.df[1,"AWS"]
    message(paste("Starting angle calculations for", aws_name))
    if(solar_angles == TRUE){
      solar_angles <- multiple_Moments_solarAngles(aws.df = aws.df,
                                                   aws_name = aws_name,
                                                   sensor_name = sensor_name, angle_selection_byIndexNr =           angle_selection_byIndexNr,
                                                   AHN3 = AHN3,
                                                   years = years,
                                                   months = months,
                                                   days = days,
                                                   s_hour = s_hour,
                                                   f_hour = f_hour,
                                                   minutes_interval = minutes_interval,
                                                   exportCSV = exportCSV,
                                                   printChart = printChart)
    }
    if(calculate_shadow_angles == TRUE){
      solar_shadow_angles <- multipleShadowAngles(aws.df = aws.df,
                                                  solar_angles[["all ah angles"]],
                                                  radius = radius,
                                                  AHN3 = AHN3,
                                                  read_only_shadow_values = read_only_shadow_values,
                                                  extract_method = extract_method,
                                                  full_circle_mask = full_circle_mask)
    }
    print(paste("Completed angle calculations for", aws_name))
    print("")
    print("=====================")
    print("")
  }
  end_time <- Sys.time()
  elapsed_time <- end_time - start_time
  print(paste("Elapsed time:", ceiling(elapsed_time), "seconds."))
}

multipleSolar_shadow_angles(aws.df = AWS.df,
                            aws_list = c("De Bilt"),
                            sensor_name = temperature_sensor_name,
                            solar_angles = TRUE, angle_selection_byIndexNr = "all",
                            calculate_shadow_angles = FALSE,
                            read_only_shadow_values = FALSE,
                            extract_method = 'bilinear',
                            years = c(2018),
                            months = c(12, 1:6),
                            days = c(21),
                            s_hour = 0,
                            f_hour = 23,
                            minutes_interval = 15,
                            radius = 300,
                            AHN3 = FALSE,
                            full_circle_mask = FALSE,
                            printChart = FALSE,
                            exportCSV = TRUE)
