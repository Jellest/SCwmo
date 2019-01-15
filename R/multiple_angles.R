##multiple angles
multiple_Moments_solarAngles <- function(aws_name, sensor_name, years, months, days, exportCSV, printChart){
  month_angles <- list()
  
  aws <- select_single_aws(AWS.df, aws_name = aws_name, sensor_name = sensor_name)
  #print(aws[["aws_wgs.sp"]]@data$LAT)
  aws_name_trim <- getAWS_name_trim(aws_name)
  dir.create(paste0("output/solar_shadow_angles/", aws_name_trim), showWarnings = FALSE)
  dir.create(paste0("output/solar_shadow_angles/", aws_name_trim, "/rasters"), showWarnings = FALSE)
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
                                          minutes_interval = 15,
                                          LONLAT = TRUE)
        all_solar_angles <- rbind(all_solar_angles, month_angles[[month_name]][["all angles"]])
      }
    }
  }
  
  aws_info <- data.frame(AWS = character(length(all_solar_angles)), sensor_name = character(length(all_solar_angles)), X = numeric(length(all_solar_angles)), Y = numeric(length(all_solar_angles)))
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
  if(missing(exportCSV)){
    exportCSV = FALSE
  }

  if(exportCSV == TRUE){
    #dir.create("output/solar_shadow_angles", getAWS_name_trim(aws_name))
    fwrite(x = all_solar_angles, file = paste0("output/solar_shadow_angles/", aws_name_trim, "/", aws_name_trim, "_solar_angles.csv"))
    fwrite(x = all_ah_solar_angles, file = paste0("output/solar_shadow_angles/", aws_name_trim, "/", aws_name_trim, "_ah_solar_angles.csv"))
    print("Exported solar angles to output folder.")
  }
  
  #all_ah_solar_angles_melt <- reshape::melt(all_ah_angles, id = "julian_day")
  #shadow_angles <- fread("data/solar_shadow_angles/DeBilt_shadow_angles.csv", data.table = FALSE)
  
  if(missing(printChart)){
    printChart = FALSE
  }
  if(printChart == TRUE){
    sun_shade_angles_chart(month_angles, shadow_angles = shadow_angles)
  }
  return(list("all angles" = all_solar_angles, "all ah angles" = all_ah_solar_angles, "all month angles" = month_angles))
}

multipleShadowAngles <- function(solar_angles, radius){
  
  ah_solar_shadow_angles <- data.frame(AWS = character(0), sensor_name = character(0), X = numeric(0), Y = numeric(0), LON = numeric(0), LAT = numeric(0), altitude = numeric(0), julian_day = numeric(0), azimuth = numeric(0), zenith = numeric(0), elevation = numeric(0), shadow_height = numeric(0), shadow_angle = numeric(0), stringsAsFactors = FALSE)
  ah_solar_shadow_angles <- ah_solar_shadow_angles[,c("LON", "LAT", "altitude", "julian_day", "azimuth", "zenith", "elevation", "shadow_height", "shadow_angle")]
  ah_shadow_rasters <- list()

  azimuths <- solar_angles$azimuth
  start_time <- Sys.time()
  aws_name_trim <- getAWS_name_trim(solar_angles$AWS[1])
  spatialpoint <- create_SpatialPoint(solar_angles[1,"X"], Y = solar_angles[1,"Y"], LONLAT = FALSE)
  aws_path <- paste0("data/AHN2/", aws_name_trim, "/raw/", aws_name_trim, "_raw_ahn.tif")
  #print(aws_path)
  aws_ahn <- raster(aws_path)
  
  #ahn_mask <- simple_mask_raster(spatialpoint = spatialpoint[["point_rd.sp"]], ahn = aws_ahn, radius = radius)
  for (a in 1:length(azimuths)){
    print(paste("Calculating shadow angle for azimuth angle: ", azimuths[a],". ", a, " out of ", length(azimuths),"...", sep=""))
    ahn_mask <- mask_raster(aws_name = solar_angles$AWS[a], spatialpoint = spatialpoint[["point_rd.sp"]], ahn = aws_ahn, azimuth = azimuths[a],radius = radius)
    
    #plot(ahn_mask)
    #View(ahn_mask)
    # print(solar_angles[a, "X"])
    # print(solar_angles[a, "Y"])
    # print(spatialpoint[["point_rd.sp"]]@coords[,"X"])
    print("Calculating shadow angles...")
    shadow_angles <- shadow_angles(aws_name = solar_angles$AWS[a], spatialpoint = spatialpoint[["point_rd.sp"]], ahn_mask = ahn_mask, angle = azimuths[a], maxDist = radius, LONLAT = FALSE)
    shadow_ha <-data.frame(shadow_angles$df)
    #heightShadow <- shadow_ha$height[1]
    #shad_angle <- shadow_ha$elevation[1]
    #View(shadow_ha)
    #ah_shadow_rasters[a] <- shadow_angles$shadows 
    #writeRaster(ah_shadow_rasters, paste0("output/rasters/",aws_name_trims[a],"")
    #sodf <- solar_angles[a,]
    #shdf <- data.frame(shadow_height = heightShadow, shadow_angle = shad_angle)
    soshdf <- merge(solar_angles[a,], shadow_ha)
    ah_solar_shadow_angles <- rbind(ah_solar_shadow_angles, soshdf)
    fwrite(ah_solar_shadow_angles, paste0("output/solar_shadow_angles/", aws_name_trim,"/",aws_name_trim, "_ah_solar_shadow_angles.csv"))
    if(a == length(azimuths)){
      #ah_solar_shadow_angles <- ah_solar_shadow_angles[,c("lat", "lon", "altitude", "julian_day", "azimuth", "zenith", "elevation", "shadow_height", "shadow_angle")]
      fwrite(ah_solar_shadow_angles, paste0("output/solar_shadow_angles/", aws_name_trim, "/", aws_name_trim, "_ah_solar_shadow_angles_complete.csv"))
    }
    end_time <- Sys.time()
    elapsed_time <- ceiling(end_time - start_time)
    message(paste("Finished angle calculations finished. Elapsed Time:", elapsed_time, "seconds."))
  }
}

multipleSolar_shadow_angles <- function(aws_list, years, months, days, radius, printChart, exportCSV){
  start_time <- Sys.time()
  if(missing(exportCSV)){
    exportCSV = TRUE
  }
  if(missing(printChart)){
    printChart = FALSE
  }
  for(a in 1:length(aws_list)){
    aws_name <- aws_list$AWS[a]
    message(paste("Starting angle calcuations for", aws_name))
    solar_angles <- multiple_Moments_solarAngles(aws_name = aws_name,
                                                 sensor_name = "temp_150cm",
                                                 years = years,
                                                 months = months,
                                                 days = days,
                                                 exportCSV = exportCSV,
                                                 printChart = printChart)
    solar_shadow_angles <- multipleShadowAngles(solar_angles[["all ah angles"]],
                                                radius = radius)
    print(paste("Completed angle calculations for", aws_name))
    print("")
    print("=====================")
    print("")
  }
  end_time <- Sys.time()
  elapsed_time <- end_time - start_time
  print(paste("Elapsed time:", ceiling(elapsed_time), "seconds."))
}

multipleSolar_shadow_angles(aws_list = sAWStemperature_list.df,
                            years = 2018,
                            months = c(12, 1:6),
                            days = 21,
                            radius = 300,
                            printChart = FALSE,
                            exportCSV = TRUE)
