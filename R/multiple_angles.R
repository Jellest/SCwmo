##multiple anges
multiplemonthsolarAnges <- function(aws_name, sensor_name, start_month , final_month, printChart){
  month_angles <- list()
  all_solar_angles <-  data.frame(lat = numeric(0), lon = numeric(0), julian_day = numeric(0), azimuth = numeric(0), zenith = numeric(0), elevation = numeric(0), stringsAsFactors = FALSE)
  for(m in seq(start_month, final_month, 1)){
    month_angles[[m]] <- solar_angles(X = 5.17939,
                                      Y = 52.09886,
                                      day = 21,
                                      month = m,
                                      year = 2018,
                                      minutes_interval = 30,
                                      LONLAT = TRUE)
    all_solar_angles <- rbind(all_solar_angles, month_angles[[m]][["all angles"]])
  }
  all_ah_angles <- subset(all_solar_angles, elevation > 0)
  if(nrow(all_ah_angles) == 0){
    warning("No elevation angles found.")
  }
  #all_ah_solar_angles_melt <- reshape::melt(all_ah_angles, id = "julian_day")
  shadow_angles <- fread("data/solar_shadow_angles/DeBilt_shadow_angles.csv", data.table = FALSE)
  
  if(missing(printChart)){
    printChart = FALSE
  }
  if(printChart == TRUE){
    sun_shade_angles_chart(month_angles, shadow_angles = shadow_angles)
  }
  return(list("all angles" = all_solar_angles, "all ah angles" = all_ah_angles, "all month angles" = month_angles))
}

multipleShadowAngles <- function(solar_angles){
  ah_solar_shadow_angles <- data.frame(lat = character(0), lon = character(0), altitude = numeric(0), julian_day = numeric(0), azimuth = numeric(0), zenith = numeric(0), elevation = numeric(0), shadow_height = numeric(0), shadow_angle = numeric(0), stringsAsFactors = FALSE)
  ah_solar_shadow_angles <- ah_solar_shadow_angles[,c("lat", "lon", "altitude", "julian_day", "azimuth", "zenith", "elevation", "shadow_height", "shadow_angle")]
  ah_shadow_rasters <- list()
  
  azimuths <- solar_angles$azimuth
  start_time <- Sys.time()
  ahn_mask <- mask_raster(spatialpoint = deBilt_rd.sp, ahn_deBilt$raw, distance = 100)
  
  for (a in seq_along(azimuths)){
    message(paste("Calculating shadow angle for azimuth angle: ", azimuths[a],". ", a, " out of ", length(azimuths),"...", sep=""))
    shadow_angles_DeBilt <- shadow_angles(spatialpoint = deBilt_rd.sp, ahn_mask = ahn_mask, angle = azimuths[a], maxDist = 100)
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

multipleSolar_shadow_angles <- function(start_month, final_month, printChart, exportCSV){
  solar_angles <- multiplemonthsolarAnges(start_month = start_month, final_month = final_month, printChart = printChart)
  solar_shadow_angles <- multipleShadowAngles(solar_angles[["all ah angles"]])
  if(missing(exportCSV)){
    exportCSV = FALSE
  }
  if(exportCSV == TRUE){
    dir.create("data/solar_shadow_angles", "AWS")
    export_csv_path <- "data/solar_shadow_angles/AWS"
    fwrite(x = solar_shadow_angles[["ssa"]], file = paste0(export_csv_path, "test.csv"))
    message("Ëxported data to output folder.")
  }
  return(solar_shadow_angles)
}


test_ssa <- multipleSolar_shadow_angles(start_month = 1, final_month = 1, printChart = FALSE, exportCSV = TRUE)
