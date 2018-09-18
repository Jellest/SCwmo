#intersect bgt
library(sf)
library(mapview)

tm_shape(BGT_station.sp, projection = rd_new_CRS, unit = "m", bbox = surroundingExtent)

#BGT_station_wfs.sp <- spTransform(BGT_station.sp, wgs_CRS)

coords.sf <- st_as_sf(aws_debilt_rd.sp)

buffer <- st_buffer(coords.sf, dist=100)
st_crs(buffer, "+init=epsg:28992")
units::set_units(buffer, m^2)
st_area(buffer)

createBuffer <- function(coords, distance){
  buffer <- st_buffer(coords, dist=distance)
  st_crs(buffer, "+init=epsg:28992")
return (buffer)}

createAnnulus <- function(coords, region){
  if(region == "10-30"){
    buffer_30m <- createBuffer(coords, 30)
    buffer_10m <- createBuffer(coords, 10) 
    annulus <- st_difference(buffer_30m, buffer_10m)
  } else if(region == "5-10"){
    buffer_10m <- createBuffer(coords, 10)
    buffer_5m <- createBuffer(coords, 5)
    annulus <- st_difference(buffer_10m, buffer_5m)
  }
  st_crs(annulus, "+init=epsg:28992")
return(annulus)}

clip_bgt <- function(aws_name, coords, bgt_shape, class, temperature_criteria.df){
  
  if(class == 1){
    outer_buffer_dist = 100
    inner_buffer_dist = 10
  } else if(class == 2){
    outer_buffer_dist = 30
    inner_buffer_dist = 5
  } else if(class == 3){
    outer_buffer_dist = 10
    inner_buffer_dist = 5
  } else if(class == 4){
    outer_buffer_dist = 10
    inner_buffer_dist = 3
  }
  
  #convert input data to SF
  #coordinates
  coords.sf <- st_as_sf(coords)

  #outer buffer area
  outer_buffer <- createBuffer(coords.sf, outer_buffer_dist)
  area_outer_buffer <- st_area(outer_buffer)
  units::set_units(area_outer_buffer, m^2)
  
  ##intersection
  shape_insct.sf <- st_intersection(bgt_shape, outer_buffer)
  st_crs(shape_insct.sf, "+init=epsg:28992")
  artificial_objects.sf <- subset(shape_insct.sf, object_typ == "pand" | object_typ == "wegdeel" | object_typ == "waterdeel")
  #units::set_units(shape_insct.sf, m^2)
  
  objectCountColName <- paste("objectCount_",as.character(outer_buffer_dist),"m", sep="")
  sumObjectAreasColName <- paste("sumObjectAreas_",as.character(outer_buffer_dist),"m", sep="")
  relAreaColName <- paste("relAreaBuffer_",as.character(outer_buffer_dist),"m", sep="")
  
  df <- data.frame(objectCount = character(nrow(shape_insct.sf)), sumAreas = character(nrow(shape_insct.sf)), relArea = character(nrow(shape_insct.sf)))
  colnames(df) <- c(objectCountColName, sumObjectAreasColName, relAreaColName)

  selected_aws <- which(temperature_criteria.df == aws_name)
  temperature_criteria.df[selected_aws,objectCountColName] <- nrow(artificial_objects.sf) 
  if(nrow(shape_insct.sf) == 0){
    temperature_criteria.df[selected_aws,sumObjectAreasColName] <- 0
    temperature_criteria.df[selected_aws,relAreaColName] <- 0
  } else {
    ##Calculate Area outer buffer
    area <- st_area(artificial_objects.sf)
    units::set_units(area, m^2)
    sum_area <- sum(area)
    print(sum_area)
    temperature_criteria.df[selected_aws,sumObjectAreasColName] <- sum_area
    
    relArea <- sum_area / area_outer_buffer
    temperature_criteria.df[selected_aws,relAreaColName] <- relArea 
    if(outer_buffer_dist == 100 | outer_buffer_dist  == 30){
      ##intersection annuulus
      if(outer_buffer_dist == 100){
        region <- "10-30"
      } else if(outer_buffer_dist == 30){
        region <- "5-10"
      }
    annulus <- createAnnulus(coords.sf, region)
    annulus_insct.sf <- st_intersection(bgt_shape, annulus)
    }
    
    ##create inner buffer and calculate area
    inner_buffer <- createBuffer(coords.sf, inner_buffer_dist)
    area_inner_buffer <- st_area(inner_buffer)
    units::set_units(area_outer_buffer, m^2)
  }
  # View(artificial_objects.sf)
  # View(temperature_criteria.df)
  
  
  #view data on map
  # buildings_rd.sf <- subset(shape_insct.sf, object_typ == "pand")
  # buildingds_wgs.sf <<- st_transform(buildings_rd.sf, "+init=epsg:4326")
  # 
  # water_rd.sf <- subset(shape_insct.sf, object_typ == "waterdeel")
  # water_wgs.sf <<- st_transform(water_rd.sf, "+init=epsg:4326")
  # 
  # roads_rd.sf <- subset(shape_insct.sf, object_typ == "wegdeel")
  # roads_wgs.sf <<- st_transform(roads_rd.sf, "+init=epsg:4326")
return(temperature_criteria.df)}

temperature_criteria.df <- clip_bgt("De Bilt", aws_debilt_rd.sp, BGT_station.sf, 100, temperature_criteria.df)

addFeatures(map, raods_wgs.sf)

