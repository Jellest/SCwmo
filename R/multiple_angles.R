##multiple angles
multiple_Moments_solarAnges <- function(aws_name, sensor_name, years, months, days, exportCSV, printChart){
  month_angles <- list()
  
  aws <- select_single_aws(AWS.df, aws_name = aws_name, sensor_name = sensor_name)
  print(aws[["aws_wgs.sp"]]@data$LAT)
  
  all_solar_angles <-  data.frame(lat = numeric(0), lon = numeric(0), julian_day = numeric(0), azimuth = numeric(0), zenith = numeric(0), elevation = numeric(0), stringsAsFactors = FALSE)
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
                                          year = y,
                                          month = months[m],
                                          day = d,
                                          minutes_interval = 30,
                                          LONLAT = TRUE)
        all_solar_angles <- rbind(all_solar_angles, month_angles[[month_name]][["all angles"]])
      }
    }
  }
  all_ah_angles <- subset(all_solar_angles, elevation > 0)
  if(nrow(all_ah_angles) == 0){
    warning(paste0("No elevation angles found for ", aws_name, "."))
  }
  if(missing(exportCSV)){
    exportCSV = FALSE
  }
  
  if(exportCSV == TRUE){
    #dir.create("output/solar_shadow_angles", getAWS_name_trim(aws_name))
    export_csv_path <- fwrite(x = all_solar_angles, file = paste0("output/solar_shadow_angles/", getAWS_name_trim(aws_name), "_solar_angles.csv"))
    message("Ëxported solar angles to output folder.")
  }
  
  #all_ah_solar_angles_melt <- reshape::melt(all_ah_angles, id = "julian_day")
  #shadow_angles <- fread("data/solar_shadow_angles/DeBilt_shadow_angles.csv", data.table = FALSE)
  
  if(missing(printChart)){
    printChart = FALSE
  }
  if(printChart == TRUE){
    sun_shade_angles_chart(month_angles, shadow_angles = shadow_angles)
  }
  return(list("all angles" = all_solar_angles, "all ah angles" = all_ah_angles, "all month angles" = month_angles))
}
test_sa <- multiple_Moments_solarAnges(aws_name = "De Bilt",
                                       sensor_name = "temp_150cm",
                                       years = c(2018),
                                       months = c(12,1:6),
                                       days = c(21),
                                       exportCSV = TRUE,
                                       printChart = FALSE)

multipleShadowAngles <- function(solar_angles){
  ah_solar_shadow_angles <- data.frame(lat = character(0), lon = character(0), altitude = numeric(0), julian_day = numeric(0), azimuth = numeric(0), zenith = numeric(0), elevation = numeric(0), shadow_height = numeric(0), shadow_angle = numeric(0), stringsAsFactors = FALSE)
  ah_solar_shadow_angles <- ah_solar_shadow_angles[,c("lat", "lon", "altitude", "julian_day", "azimuth", "zenith", "elevation", "shadow_height", "shadow_angle")]
  ah_shadow_rasters <- list()
  
  azimuths <- solar_angles$azimuth
  start_time <- Sys.time()
  
  for (a in seq_along(azimuths)){
    spatialpoint <- createpoint(solar_angles[a,"lon"], Y = solar_angles[a,"lat"], LONLAT = TRUE)
    aws_ahn <- raster("data/AHN2/DeBilt/raw/DeBilt_raw_ahn.tif")
    message(paste("Calculating shadow angle for azimuth angle: ", azimuths[a],". ", a, " out of ", length(azimuths),"...", sep=""))
    ahn_mask <- mask_raster(spatialpoint = , ahn = aws_ahn, azimuth = azimuths[a],distance = 100)
    shadow_angles_DeBilt <- shadow_angles(X = solar_angles[a, "LON"], Y = solar_angles[a, "LAT"], ahn_mask = ahn_mask, angle = azimuths[a], maxDist = 300)
    shadow_ha <-data.frame(shadow_angles_DeBilt$df)
    #heightShadow <- shadow_ha$height[1]
    #shad_angle <- shadow_ha$elevation[1]
    #View(shadow_ha)
    ah_shadow_rasters[a] <- shadow_angles_DeBilt$shadows 
    #sodf <- solar_angles[a,]
    #shdf <- data.frame(shadow_height = heightShadow, shadow_angle = shad_angle)
    soshdf <- merge(solar_angles[a,], shadow_ha)
    ah_solar_shadow_angles <- rbind(ah_solar_shadow_angles, soshdf)
    
    if(a == length(azimuths)){
      #ah_solar_shadow_angles <- ah_solar_shadow_angles[,c("lat", "lon", "altitude", "julian_day", "azimuth", "zenith", "elevation", "shadow_height", "shadow_angle")]
      end_time <- Sys.time()
      elapsed_time <- ceiling(end_time - start_time)
      message(paste("shadow angle calculations finished. Elapsed Time:", elapsed_time, "seconds."))
    }
  }
  return(list("shadow rasters" = ah_shadow_rasters, "ssa" = ah_solar_shadow_angles))
}

multipleSolar_shadow_angles <- function(aws_list, printChart, exportCSV){
  years <- c(2018)
  months <- c(12, 1:6)
  days <- c(21)
  for(a in 1:length(aws_list)){
    solar_angles <- multiplemonthsolarAnges(aws_name, sensor_name, years, months, days, printChart)
    solar_shadow_angles <- multipleShadowAngles(solar_angles[["all ah angles"]])
    solar_shadow_angles[["df"]][,"AWS"] <- aws_name
    solar_shadow_angles[["df"]][,"sensor_name"] <- sensor_name
    
    if(missing(exportCSV)){
      exportCSV = FALSE
    }
    
    if(exportCSV == TRUE){
      #dir.create("output/solar_shadow_angles", getAWS_name_trim(aws_name))
      export_csv_path <-   
      fwrite(x = solar_shadow_angles[["ssa"]], file = paste0("output/solar_shadow_angles/", getAWS_name_trim(aws_name), "_solar_shadow_angles.csv"))
      message("Ëxported solar and shadow angles to output folder.")
    }
  }
 
  return(solar_shadow_angles)
}


test_ssa <- multipleSolar_shadow_angles(start_month = 1, final_month = 1, printChart = FALSE, exportCSV = TRUE)