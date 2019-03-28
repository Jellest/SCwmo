##multiple angles
multiple_Moments_solarAngles <- function(spatialpoint, name, name.supplement = "", AWS = FALSE, LONLAT = FALSE, sensor.name, AHN3 = TRUE, years, months, days, s_hour = 0, f_hour = 23, minutes.interval = 60, angle_selection_byIndexNr = "all", exportCSV = FALSE, printChart = FALSE){
  if(AHN3 == TRUE){
    AHN <- "AHN3"
  } else {
    AHN <- "AHN2"
  }
  #View(spatialpoint)
  month_angles <- list()
  #print(aws[["aws_wgs.sp"]]@data$LAT)
  name_trim <- getName_trim(name = name, name.supplement = name.supplement)
  if(!base::dir.exists(paste0("output/", name_trim))){
    base::dir.create(paste0("output/", name_trim), showWarnings = FALSE)
  }
  if(!base::dir.exists(paste0("output/", name_trim, "/solar_shadow_angles"))){
    base::dir.create(paste0("output/", name_trim, "/solar_shadow_angles"))
  }
  base::dir.create(paste0("output/", name_trim, "/solar_shadow_angles/rasters"), showWarnings = FALSE)
  base::dir.create(paste0("output/", name_trim, "/solar_shadow_angles/rasters/", AHN), showWarnings = FALSE)
  base::dir.create(paste0("output/", name_trim, "/solar_shadow_angles/rasters/", AHN, "/Masks"), showWarnings = FALSE)
  base::dir.create(paste0("output/", name_trim, "/solar_shadow_angles/rasters/", AHN, "/Shadows"), showWarnings = FALSE)

  all_solar_angles <-  data.frame(LAT = numeric(0), LON = numeric(0), julian_day = numeric(0), azimuth = numeric(0), zenith = numeric(0), elevation = numeric(0), stringsAsFactors = FALSE)

  #create WGS spatial point if not an AWS
  spatialpoint.wgs <- sf::st_transform(spatialpoint, rgdal::CRSargs(CRS("+init=epsg:4326")))
  if(AWS == FALSE){
    #geom <- data.frame(sf::st_coordinates(spatialpoint.wgs))
    if(LONLAT == TRUE){
      geom <- data.frame(sf::st_coordinates(spatialpoint.wgs))
      spatialpoint.df <- data.frame(LON = geom$X, LAT = geom$Y)
    } else {
      geom <- data.frame(sf::st_coordinates(spatialpoint))
      spatialpoint.df <- data.frame(X = geom$X, Y = geom$Y)
    }
  } else {

  }
  #View(spatialpoint)
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
        month_angles[[month_name]] <- solar_angles(spatialpoint, AWS = AWS,
                                                   year = years[y],
                                                   month = months[m],
                                                   day = days[d],
                                                   s_hour = s_hour,
                                                   f_hour = f_hour,
                                                   minutes.interval = minutes.interval)
        all_solar_angles <- rbind(all_solar_angles, month_angles[[month_name]][["all angles"]])
      }
    }
  }
  #View(all_solar_angles)
  location_info <- data.frame(name = character(nrow(all_solar_angles)), sensor.name = character(nrow(all_solar_angles)), X = numeric(nrow(all_solar_angles)), Y = numeric(nrow(all_solar_angles)))
  location_info$name <- name
  location_info$sensor.name <- sensor.name

  if(AWS == TRUE){
    geom <- data.frame(sf::st_coordinates(spatialpoint))
    location_info$X <- geom$X
    location_info$Y <- geom$Y
  } else {
    if(LONLAT == TRUE){
      location_info$X <- spatialpoint.df$LON
      location_info$Y <- spatialpoint.df$LAT
    } else {
      location_info$X <- spatialpoint.df$X
      location_info$Y <- spatialpoint.df$Y
    }
  }
  all_solar_angles <- cbind(location_info, all_solar_angles)
  #View(all_solar_angles)

  all_ah_solar_angles <- dplyr::filter(all_solar_angles, elevation > 0)
  if(nrow(all_ah_solar_angles) == 0){
    warning(paste0("No positive elevation angles found for ", name, "."))
  }
  if(angle_selection_byIndexNr != "all"){
    all_solar_angles <- all_solar_angles[angle_selection_byIndexNr,]
    all_ah_solar_angles <- all_ah_solar_angles[angle_selection_byIndexNr,]
  }

  if(exportCSV == TRUE){
    #base::dir.create("output/solar_shadow_angles", getName_trim(name))
    data.table::fwrite(x = all_solar_angles, file = paste0("output/", name_trim, "/solar_shadow_angles/", name_trim, "_solar_angles.csv"))
    data.table::fwrite(x = all_ah_solar_angles, file = paste0("output/", name_trim, "/solar_shadow_angles/", name_trim, "_ah_solar_angles.csv"))
    print("Exported solar angles to output folder.")
  }

  #all_ah_solar_angles_melt <- reshape::melt(all_ah_angles, id = "julian_day")
  #shadow_angles <- fread("datasets/solar_shadow_angles/DeBilt_shadow_angles.csv", data.table = FALSE)

  # if(printChart == TRUE){
  #   sun_shade_angles_chart(month_angles, shadow_angles = shadow_angles)
  # }
  return(list("all angles" = all_solar_angles, "ah angles" = all_ah_solar_angles, "all month angles" = month_angles))
}

multipleShadowAngles <- function(spatialpoint, name, name.supplement = "", AWS = FALSE, LONLAT = FALSE, solar_angles, radius, AHN3 = TRUE, sensor_height = 0, minutes.interval = 60, read_only_shadow_values = FALSE, extract_method = 'bilinear', full_circle_mask = FALSE, printChart = FALSE, angle_selection_byIndexNr = "all"){
  start_time <- Sys.time()
  ah_solar_shadow_angles <- data.frame(name = character(0), sensor.name = character(0), X = numeric(0), Y = numeric(0), LON = numeric(0), LAT = numeric(0), altitude = numeric(0), julian_day = numeric(0), azimuth = numeric(0), zenith = numeric(0), elevation = numeric(0), shadow_height = numeric(0), shadow_angle = numeric(0), stringsAsFactors = FALSE)
  ah_solar_shadow_angles <- ah_solar_shadow_angles[,c("LON", "LAT", "altitude", "julian_day", "azimuth", "zenith", "elevation", "shadow_height", "shadow_angle")]
  ah_shadow_rasters <- list()

  name_trim <- getName_trim(name = name, name.supplement = name.supplement)
  if(name.supplement != ""){
    orig_name_trim <- getName_trim(name = name)
  } else {
    orig_name_trim <- name_trim
  }
  azimuths <- solar_angles$azimuth

  if(AHN3 == TRUE){
    AHN <- "AHN3"
  } else {
    AHN <- "AHN2"
  }
  #convert to sp spatialpoint
  spatialpoint.sp <- sf::as_Spatial(spatialpoint)

  name_path <- paste0("datasets/", AHN, "/", orig_name_trim, "/raw/", orig_name_trim, "_", AHN,"_raw_ahn.tif")
  #print(name_path)
  location_ahn <- raster::raster(name_path)

  if(sensor_height > 0){
    terrain_raster <- raster::raster(paste0("datasets/", AHN,"/", orig_name_trim,"/terrain/", orig_name_trim,"_", AHN, "_terrain_ahn.tif"))
    terrain_height <- raster::extract(terrain_raster, spatialpoint.sp, sp = TRUE)@data[1,paste0(orig_name_trim, "_", AHN, "_terrain_ahn")]

    ahn_sensor_height <- terrain_height + sensor_height

    cellNr_raw <- raster::cellFromXY(location_ahn, spatialpoint.sp)
    print(paste0("Raw ahn original height value: ", location_ahn[cellNr_raw]))
    #location_ahn[cellNr_raw] <- ahn_sensor_height
    adjacentCells <- raster::adjacent(location_ahn, cells=c(cellNr_raw), directions=8, pairs=FALSE, include = TRUE, id = TRUE)
    #print(adjacentCells)
    for(adj in 1:length(adjacentCells)){
      location_ahn[adjacentCells[adj]] <- ahn_sensor_height
    }
    print(paste0("Set height at location of sensor from:", terrain_height," (terrain) to: ", ahn_sensor_height,"..."))
  }


  if(sensor_height == 0){
    terrain_raster <- raster::raster(paste0("datasets/", AHN,"/", orig_name_trim,"/terrain/", orig_name_trim,"_", AHN, "_terrain_ahn.tif"))
    terrain_extract <- raster::extract(terrain_raster, spatialpoint.sp, sp = TRUE, cellnumbers = TRUE)@data[1,]
    terrain_height <- terrain_extract[1,paste0(orig_name_trim, "_", AHN, "_terrain_ahn")]
    terrain_height_cellNr <- terrain_extract[1, "cells"]
    #cellNr_terrain <- cellFromXY(terrain_raster, spatialpoint.sp)

    neighbours <-  matrix(1, nrow=13, ncol=13)
    neighbours[85] <- 0
    print("Getting surrounding terrain height values...")
    terrain_height_surrounding_cellNrs <- adjacent(terrain_raster, cells=c(terrain_height_cellNr), directions=neighbours, pairs=FALSE, include = TRUE)
    values <- c()
    for (c in 1:length(terrain_height_surrounding_cellNrs)){
      values <- base::append(values, terrain_raster[terrain_height_surrounding_cellNrs[c]])
    }

    # getVals <- function (values, terrain_raster, cellNr){
    #   new_values <- append(values, terrain_raster[cellNr])
    #
    # return (new_values)}
    #
    # values <- mapply(terrain_height_surrounding_cellNrs, getVals, values, terrain_raster, terrain_height_surrounding_cellNrs)

    minHeight <- base::min(values, na.rm = TRUE)
    #adjust cell and surroundng 3 m. to terrain height values.
    cellNr_raw <- raster::cellFromXY(location_ahn, spatialpoint.sp)
    print(paste0("Raw ahn original height value: ", location_ahn[cellNr_raw]))
    #location_ahn[cellNr_raw] <- ahn_sensor_height
    adjacentCells <- raster::adjacent(location_ahn, cells=c(cellNr_raw), directions=neighbours, pairs=FALSE, include = TRUE, id = TRUE)
    #print(adjacentCells)
    for(adj in 1:length(adjacentCells)){
      location_ahn[adjacentCells[adj]] <- minHeight
    }
    print(paste0("Set height at location of sensor with a radius of 3 m. to terrain height: ", minHeight, "..."))
    raster::writeRaster(x = location_ahn, filename = paste0("output/", name_trim, "/solar_shadow_angles/rasters/", AHN, "/Masks/", orig_name_trim, "_", AHN, "_flat_Mask.tif"), overwrite = TRUE)
  }


  if(full_circle_mask == TRUE){
    ahn_mask <- simple_mask_raster(spatialpoint = spatialpoint.sp, name = name, ahn_raster = location_ahn, AHN3 = AHN3, radius = radius)
  }
  for (a in 1:length(azimuths)){
    print(paste("Calculating shadow angle for azimuth angle: ", azimuths[a],". ", a, " out of ", length(azimuths),"...", sep=""))
    if(full_circle_mask == FALSE){
      ahn_mask <- mask_raster(spatialpoint = spatialpoint, name = name, name.supplement = name.supplement, ahn_raster = location_ahn, AHN3 = AHN3, azimuth = azimuths[a],radius = radius)
    }
    #print(paste0("New raw ahn height value: ", extract(ahn_mask, spatialpoint.sp, sp = TRUE)@data[1,paste0(name_trim, "_", AHN, "_raw_ahn")]))

    #calcluate shadow_angle
    shadow_angle_raw <- shadow_angles(spatialpoint = spatialpoint.sp, name = name, name.supplement = name.supplement, LONLAT = LONLAT, ahn_mask = ahn_mask, angle = azimuths[a], maxDist = radius, AHN3 = AHN3, read_only = read_only_shadow_values, extract_method = extract_method)

    #correct if higher than highest possible shadow angle
    shadow_angle <- highest_shadow_angle(spatialpoint = spatialpoint, AWS = AWS, x = azimuths[a], shadow_angle_raw = shadow_angle_raw[1,"shadow_angle_raw"], minutes.interval = minutes.interval)
    shadows <-cbind(shadow_angle_raw, shadow_angle)

    #merge with solar angles
    sol_sha.df <- base::merge(solar_angles[a,], shadows)
    ah_solar_shadow_angles <- rbind(ah_solar_shadow_angles, sol_sha.df)
    fwrite(ah_solar_shadow_angles, paste0("output/", name_trim, "/solar_shadow_angles/",name_trim, "_", AHN, "_ah_solar_shadow_angles.csv"))
    if(a == length(azimuths)){
      export_path <- paste0("output/", name_trim, "/solar_shadow_angles/", name_trim, "_", AHN, "_ah_solar_shadow_angles_backup.csv")
      data.table::fwrite(ah_solar_shadow_angles, export_path)
      if(printChart == TRUE){
        sun_shade_angles_chart(data_path = export_path, name = name, name.supplement = name.supplement, angle_selection_byIndexNr = angle_selection_byIndexNr, AHN3 = AHN3, extract_method = extract_method)
      }
      end_time <- Sys.time()
    }
  }

  elapsed_time <- ceiling(end_time - start_time)
  message(paste("Finished angle calculations for", name, ". Elapsed Time:", elapsed_time, "seconds."))
return(ah_solar_shadow_angles)}

multipleSolar_shadow_angles <- function(spatialpoint_list, name_list, sensor.name, name.supplement = "", AWS = FALSE, solar_angles = TRUE, angle_selection_byIndexNr = "all", calculate_shadow_angles = TRUE, years, months, days, s_hour = 0, f_hour = 23, minutes.interval = 60, radius, AHN3 = TRUE, sensor_height = 0, read_only_shadow_values = FALSE, extract_method = 'bilinear', full_circle_mask = FALSE, printChart = FALSE, exportCSV = FALSE, test = FALSE){
  start_time <- Sys.time()

  # if(AHN3 == TRUE){
  #   name_list <- AWS_temperature_ahn3Only_names
  # }
  #
  # ahn2_analysed <- c("Rotterdam","Vlissingen", "Voorschoten", "Wijk aan zee")#"De Bilt", "Arcen", "Rotterdam", "Vlissingen", "Voorschoten")#c("De Bilt", "Rotterdam", "Arcen", "Vlissingen", "Wijk aan zee", "Voorschoten", "Berkhout", "Cabauw mast", "De Kooy", "Deelen")
  # ahn3_analysed <- c()#c("De Bilt", "Schiphol")#"Rotterdam", "Vlissingen", "Wijk aan zee", "Voorschoten")
  for(a in 1:length(name_list)){
    spatialpoint <- spatialpoint_list[a,]
    mapview(spatialpoint)
    name <- name_list[a]

    # if(AHN3 == FALSE){
    #   if(name %in% ahn2_analysed == TRUE & test == FALSE){
    #     message(paste(name, "is already analysed for AHN2. Going to the next station..."))
    #     next
    #   }
    # } else if(AHN3 == TRUE){
    #   if(name %in% ahn3_analysed == TRUE & test == FALSE){
    #     message(paste(name, "is already analysed for AHN3. Going to the next station..."))
    #     next
    #   }
    # }

    message(paste("Starting angle calculations for", name))
    #determine solar angles
    if(solar_angles == TRUE){
      my_solar_angles <- multiple_Moments_solarAngles(spatialpoint = spatialpoint, name = name, name.supplement = name.supplement, AWS = AWS,
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
    #select onloy ah soar angles
    #ah_solar_angles <- dplyr::subset(my_solar_angles, elevation > 0)

    #calculate shadow angles
    if(calculate_shadow_angles == TRUE){
      my_shadow_angles <- multipleShadowAngles(spatialpoint = spatialpoint,
                                             name = name, AWS = AWS,
                                             solar_angles = my_solar_angles[["ah angles"]],
                                             radius = radius,
                                             AHN3 = AHN3,
                                             sensor_height = sensor_height,
                                             minutes.interval = minutes.interval,
                                             read_only_shadow_values = read_only_shadow_values,
                                             extract_method = extract_method,
                                             name.supplement = name.supplement,
                                             angle_selection_byIndexNr = angle_selection_byIndexNr
                                             )
    }

    # if(AHN3 == FALSE){
    #   ahn2_analysed <- append(ahn2_analysed, name)
    # } else if(AHN3 == TRUE){
    #   ahn3_analysed <- append(ahn3_analysed, name)
    # }

    print(paste("Completed angle calculations for", name))
    print("")
    print("=====================")
    print("")
  }
  end_time <- Sys.time()
  elapsed_time <- end_time - start_time
  print(paste("Elapsed time:", ceiling(elapsed_time), "seconds."))
return (my_shadow_angles)}
