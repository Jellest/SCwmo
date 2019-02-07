##multiple angles
multiple_Moments_solarAngles <- function(aws.df = AWS.df, aws_name, sensor_name, addition = "", AHN3 = FALSE, years, months, days, s_hour = 0, f_hour = 23, minutes_interval = 60, angle_selection_byIndexNr = "all", exportCSV = FALSE, printChart = FALSE){
  if(AHN3 == TRUE){
    AHN <- "AHN3"
  } else {
    AHN <- "AHN2"
  }
  month_angles <- list()
  aws <- select_single_aws(aws.df = aws.df, aws_name = aws_name, sensor_name = sensor_name)
  #print(aws[["aws_wgs.sp"]]@data$LAT)
  
  aws_name_trim <- getAWS_name_trim(aws.df = aws.df, aws_name = aws_name, addition = addition)
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

multipleShadowAngles <- function(aws.df = AWS.df, solar_angles, radius, AHN3 = FALSE, sensor_height = 0, read_only_shadow_values = FALSE, extract_method = 'bilinear', full_circle_mask = FALSE, printChart = FALSE, addition = "", angle_selection_byIndexNr = "all"){
  ah_solar_shadow_angles <- data.frame(AWS = character(0), sensor_name = character(0), X = numeric(0), Y = numeric(0), LON = numeric(0), LAT = numeric(0), altitude = numeric(0), julian_day = numeric(0), azimuth = numeric(0), zenith = numeric(0), elevation = numeric(0), shadow_height = numeric(0), shadow_angle = numeric(0), stringsAsFactors = FALSE)
  ah_solar_shadow_angles <- ah_solar_shadow_angles[,c("LON", "LAT", "altitude", "julian_day", "azimuth", "zenith", "elevation", "shadow_height", "shadow_angle")]
  ah_shadow_rasters <- list()
  
  aws_name <- solar_angles$AWS[1]
  aws_name_trim <- getAWS_name_trim(aws.df = aws.df, aws_name = aws_name, addition = addition)
  if(addition != ""){
    orig_aws_name_trim <- getAWS_name_trim(aws.df = aws.df, aws_name = aws_name)
  } else {
    orig_aws_name_trim <- aws_name_trim
  }
  azimuths <- solar_angles$azimuth
  start_time <- Sys.time()
  spatialpoint <- create_SpatialPoint(solar_angles[1,"X"], Y = solar_angles[1,"Y"], LONLAT = FALSE)
  if(AHN3 == TRUE){
    AHN <- "AHN3"
  } else {
    AHN <- "AHN2"
  }

  aws_path <- paste0("data/", AHN, "/", orig_aws_name_trim, "/raw/", orig_aws_name_trim, "_", AHN,"_raw_ahn.tif")
  #print(aws_path)
  aws_ahn <- raster(aws_path)
  
  if(sensor_height > 0){
    terrain_raster <- raster(paste0("data/", AHN,"/", orig_aws_name_trim,"/terrain/", orig_aws_name_trim,"_", AHN, "_terrain_ahn.tif")) 
    terrain_height <- extract(terrain_raster, spatialpoint[["point_rd.sp"]], sp = TRUE)@data[1,paste0(orig_aws_name_trim, "_", AHN, "_terrain_ahn")]
    
    ahn_sensor_height <- terrain_height + sensor_height
    
    cellNr_raw <- cellFromXY(aws_ahn, spatialpoint[["point_rd.sp"]])
    print(paste0("Raw ahn original height value: ", aws_ahn[cellNr_raw]))
    #aws_ahn[cellNr_raw] <- ahn_sensor_height
    adjacentCells <- adjacent(aws_ahn, cells=c(cellNr_raw), directions=8, pairs=FALSE, include = TRUE, id = TRUE)
    #print(adjacentCells)
    for(adj in 1:length(adjacentCells)){
      aws_ahn[adjacentCells[adj]] <- ahn_sensor_height 
    }
    print(paste0("Set height at location of sensor from:", terrain_height," (terrain) to: ", ahn_sensor_height,"..."))
  }
  
  
  if(sensor_height == 0){
    terrain_raster <- raster(paste0("data/", AHN,"/", orig_aws_name_trim,"/terrain/", orig_aws_name_trim,"_", AHN, "_terrain_ahn.tif")) 
    terrain_extract <- extract(terrain_raster, spatialpoint[["point_rd.sp"]], sp = TRUE, cellnumbers = TRUE)@data[1,]
    terrain_height <- terrain_extract[1,paste0(orig_aws_name_trim, "_", AHN, "_terrain_ahn")]
    terrain_height_cellNr <- terrain_extract[1, "cells"] 
    #cellNr_terrain <- cellFromXY(terrain_raster, spatialpoint[["point_rd.sp"]])
    
    neighbours <-  matrix(1, nrow=13, ncol=13) 
    neighbours[85] <- 0
    print("Getting surrounding terrain height values...")
    terrain_height_surrounding_cellNrs <- adjacent(terrain_raster, cells=c(terrain_height_cellNr), directions=neighbours, pairs=FALSE, include = TRUE) 
    values <- c()
    for (c in 1:length(terrain_height_surrounding_cellNrs)){
      values <- append(values, terrain_raster[terrain_height_surrounding_cellNrs[c]])
    }
    
    # getVals <- function (values, terrain_raster, cellNr){
    #   new_values <- append(values, terrain_raster[cellNr])
    #   
    # return (new_values)}
    # 
    # values <- mapply(terrain_height_surrounding_cellNrs, getVals, values, terrain_raster, terrain_height_surrounding_cellNrs)
    
    minHeight <- min(values, na.rm = TRUE)
    #adjust cell and surroundng 3 m. to terrain height values.
    cellNr_raw <- cellFromXY(aws_ahn, spatialpoint[["point_rd.sp"]])
    print(paste0("Raw ahn original height value: ", aws_ahn[cellNr_raw]))
    #aws_ahn[cellNr_raw] <- ahn_sensor_height
    adjacentCells <- adjacent(aws_ahn, cells=c(cellNr_raw), directions=neighbours, pairs=FALSE, include = TRUE, id = TRUE)
    #print(adjacentCells)
    for(adj in 1:length(adjacentCells)){
      aws_ahn[adjacentCells[adj]] <- minHeight
    }
    print(paste0("Set height at location of sensor with a radius of 3 m. to terrain height: ", minHeight, "..."))
    writeRaster(x = aws_ahn, filename = paste0("output/", aws_name_trim, "/solar_shadow_angles/rasters/", AHN, "/Masks/", orig_aws_name_trim, "_", AHN, "_flat_Mask.tif"), overwrite = TRUE)
  }
  
  
  if(full_circle_mask == TRUE){
    ahn_mask <- simple_mask_raster(aws.df = aws.df, spatialpoint = spatialpoint[["point_rd.sp"]], ahn = aws_ahn, radius = radius, aws_name = aws_name)
  }
  for (a in 1:length(azimuths)){
    print(paste("Calculating shadow angle for azimuth angle: ", azimuths[a],". ", a, " out of ", length(azimuths),"...", sep=""))
    if(full_circle_mask == FALSE){
      ahn_mask <- mask_raster(aws.df = aws.df, aws_name = aws_name, addition = addition, spatialpoint = spatialpoint[["point_rd.sp"]], ahn_raster = aws_ahn, AHN3 = AHN3, azimuth = azimuths[a],radius = radius)
    }
    #print(paste0("New raw ahn height value: ", extract(ahn_mask, spatialpoint[["point_rd.sp"]], sp = TRUE)@data[1,paste0(aws_name_trim, "_", AHN, "_raw_ahn")]))
    
    #calcluate shadow_angle
    shadow_angle_raw <- shadow_angles(aws.df = aws.df, aws_name = aws_name, addition = addition, spatialpoint = spatialpoint[["point_rd.sp"]], ahn_mask = ahn_mask, angle = azimuths[a], maxDist = radius, LONLAT = FALSE, AHN3 = AHN3, extract_method = extract_method,read_only = read_only_shadow_values)$df
    
    #correct if higher than highest possible shadow angle
    print(shadow_angle_raw)
    shadow_angle <- highest_shadow_angle(aws_name = aws_name, sensor_name = temperature_sensor_name, x = azimuths[a], shadow_angle_raw = shadow_angle_raw[1,"shadow_angle_raw"])
    shadows <-cbind(shadow_angle_raw, shadow_angle)
    
    #merge with solar angles
    sol_sha.df <- merge(solar_angles[a,], shadows)
    ah_solar_shadow_angles <- rbind(ah_solar_shadow_angles, sol_sha.df)
    fwrite(ah_solar_shadow_angles, paste0("output/", aws_name_trim, "/solar_shadow_angles/",aws_name_trim, "_", AHN, "_ah_solar_shadow_angles.csv"))
    if(a == length(azimuths)){
      export_path <- paste0("output/", aws_name_trim, "/solar_shadow_angles/", aws_name_trim, "_", AHN, "_ah_solar_shadow_angles_backup.csv") 
      fwrite(ah_solar_shadow_angles, export_path)
      if(printChart == TRUE){
        sun_shade_angles_chart(aws.df = aws.df, data_path = export_path, aws_name = aws_name, addition = addition, angle_selection_byIndexNr = angle_selection_byIndexNr, AHN3 = AHN3, extract_method = extract_method) 
      }
      end_time <- Sys.time()
    }
  }
  
  elapsed_time <- ceiling(end_time - start_time)
  message(paste("Finished angle calculations for", aws_name, ". Elapsed Time:", elapsed_time, "seconds."))
}

multipleSolar_shadow_angles <- function(aws.df = AWS.df, aws_list, sensor_name, addition = "", solar_angles = TRUE, angle_selection_byIndexNr = "all", calculate_shadow_angles = TRUE, years, months, days, s_hour = 0, f_hour = 23, minutes_interval = 60, radius, AHN3 = FALSE, sensor_height = 0, read_only_shadow_values = FALSE, extract_method = 'bilinear', full_circle_mask = FALSE, printChart = FALSE, exportCSV = FALSE, test = FALSE){
  start_time <- Sys.time()
  ahn2_analysed <- c()#"De Bilt", "Arcen", "Rotterdam", "Vlissingen", "Voorschoten")#c("De Bilt", "Rotterdam", "Arcen", "Vlissingen", "Wijk aan zee", "Voorschoten", "Berkhout", "Cabauw mast", "De Kooy", "Deelen")
  ahn3_analysed <- c()#c("De Bilt", "Schiphol")#"Rotterdam", "Vlissingen", "Wijk aan zee", "Voorschoten")
  for(a in 1:length(aws_list)){
    single_aws.df <- dplyr::filter(aws.df, AWS == aws_list[a] & Sensor == sensor_name)
    aws_name <- single_aws.df[1,"AWS"]
    
    if(AHN3 == FALSE){  
      if(aws_name %in% ahn2_analysed == TRUE & test == FALSE){
        message(paste(aws_name, "is already analysed for AHN2. Going to the next station..."))
        next
      }
    } else if(AHN3 == TRUE){
      if(aws_name %in% ahn3_analysed == TRUE & test == FALSE){
        message(paste(aws_name, "is already analysed for AHN3. Going to the next station..."))
        next
      }
    }
    
    message(paste("Starting angle calculations for", aws_name))
    if(solar_angles == TRUE){
      my_solar_angles <- multiple_Moments_solarAngles(aws.df = aws.df,
                                                   aws_name = aws_name, addition = addition,
                                                   sensor_name = sensor_name, angle_selection_byIndexNr = angle_selection_byIndexNr,
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
                        multipleShadowAngles(aws.df = aws.df,
                                                  solar_angles = my_solar_angles[["all ah angles"]],
                                                  radius = radius,
                                                  AHN3 = AHN3,
                                                  sensor_height = sensor_height,
                                                  read_only_shadow_values = read_only_shadow_values,
                                                  extract_method = extract_method,
                                                  addition = addition,
                                                  angle_selection_byIndexNr = angle_selection_byIndexNr
                                             )
    }
    
    if(AHN3 == FALSE){
      ahn2_analysed <- append(ahn2_analysed, aws_name)
    } else if(AHN3 == TRUE){
      ahn3_analysed <- append(ahn3_analysed, aws_name)
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
                            aws_list = AWS_temperature_names_tmp,#c("De Bilt", "Schiphol"),
                            sensor_name = temperature_sensor_name,
                            addition = "__",
                            solar_angles = TRUE, angle_selection_byIndexNr = c(1, 13),
                            calculate_shadow_angles = TRUE,
                            sensor_height = 0,
                            read_only_shadow_values = FALSE,
                            extract_method = 'bilinear',
                            years = c(2018),
                            months = c(12, 1:6),
                            days = c(21),
                            s_hour = 0,
                            f_hour = 23,
                            minutes_interval = 15,
                            radius = 100,
                            AHN3 = FALSE,
                            full_circle_mask = FALSE,
                            printChart = TRUE,
                            exportCSV = TRUE,
                            test = FALSE)
