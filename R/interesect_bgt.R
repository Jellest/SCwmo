#intersect bgt
library(sf)
library(mapview)

clip_bgt <- function(aws_name, coords, bgt_shape, temperature_criteria.df, class){
  if(missing(class)){
    #start at class 1
    class <- 1
  }
  
  ##functions
  
  createBuffer <- function(coords, distance){
    buffer <- st_buffer(coords, dist=distance)
    st_crs(buffer, "+init=epsg:28992")
    return (buffer)}
  
  createAnnulus <- function(coords, region){
    if(region == "10_30"){
      buffer_30m <- createBuffer(coords, 30)
      buffer_10m <- createBuffer(coords, 10) 
      annulus <- st_difference(buffer_30m, buffer_10m)
    } else if(region == "5_10"){
      buffer_10m <- createBuffer(coords, 10)
      buffer_5m <- createBuffer(coords, 5)
      annulus <- st_difference(buffer_10m, buffer_5m)
    }
    st_crs(annulus, "+init=epsg:28992")
    return(annulus)}
  
  ##select buffer areas according to class
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
    
  ##column names
  meet_classNr <- paste("meetClass_", toString(class), sep="") 
  
  outer_objectCountColName <- paste("objectCount_",as.character(outer_buffer_dist),"m", sep="")
  outer_sumObjectAreasColName <- paste("sumObjectAreas_",as.character(outer_buffer_dist),"m", sep="")
  outer_relAreaColName <- paste("relAreaBuffer_",as.character(outer_buffer_dist),"m", sep="")
  
  inner_objectCountColName <- paste("objectCount_",as.character(inner_buffer_dist),"m", sep="")
  inner_sumObjectAreasColName <- paste("sumObjectAreas_",as.character(inner_buffer_dist),"m", sep="")
  inner_relAreaColName <- paste("relAreaBuffer_",as.character(inner_buffer_dist),"m", sep="")

  df <- data.frame(outer_objectCount = character(nrow(artificial_objects.sf)), outer_sumAreas = character(nrow(artificial_objects.sf)), outer_relArea = character(nrow(artificial_objects.sf)),
                   inner_objectCount = character(nrow(artificial_objects.sf)), inner_sumAreas = character(nrow(artificial_objects.sf)), inner_relArea = character(nrow(artificial_objects.sf)),
                   meetClass = logical(nrow(artificial_objects.sf)))
                  
  colnames(df) <- c(outer_objectCountColName, outer_sumObjectAreasColName, outer_relAreaColName, 
                    inner_objectCountColName, inner_sumObjectAreasColName, inner_relAreaColName,
                    meet_classNr)
  
  if(class >= 5){
    temperature_criteria.df[selected_aws_row,meet_classNr] <- TRUE
  } else {
  
    if(class == 1 | class == 2){  
      if(class == 1){
            region <- "10_30"
      } else if(class == 2){
            region <- "5_10"
      }
      annulus_objectCountColName <- paste("objectCount_",as.character(region),"m", sep="")
      annulus_sumObjectAreasColName <- paste("sumObjectAreas_",as.character(region),"m", sep="")
      annulus_relAreaColName <- paste("relAreaRegion_",as.character(region),"m", sep="")
    
      df_annulus <- data.frame(objectCount = character(nrow(artificial_objects.sf)), sumAreas = character(nrow(artificial_objects.sf)), relArea = character(nrow(artificial_objects.sf)))
      colnames(df_annulus) <- c( annulus_objectCountColName, annulus_sumObjectAreasColName, annulus_relAreaColName)
      df <- cbind(df, df_annulus)
    }
    
    
    #convert input data to SF
    #coordinates
    #coords.sf <- st_as_sf(coords)
    #outer buffer area
    outer_buffer <- createBuffer(coords, outer_buffer_dist)
    area_outer_buffer <- st_area(outer_buffer)
    units::set_units(area_outer_buffer, m^2)

    #units::set_units(shape_insct.sf, m^2)
    
    #intersection
    st_agr(bgt_shape) <- "constant"
    st_agr(outer_buffer) <- "constant"
    BGT_outerBuffer <- st_intersection(bgt_shape, outer_buffer)
    st_crs(BGT_outerBuffer, "+init=epsg:28992")
    
    #select artifical objects within outer buffer
    artificial_objects.sf <<- subset(BGT_outerBuffer, object_typ == "pand" | object_typ == "wegdeel" | object_typ == "waterdeel")
    buildings <<- subset(BGT_outerBuffer, object_typ == "pand")
    water <<- subset(BGT_outerBuffer, object_typ == "waterdeel")
    roads <<- subset(BGT_outerBuffer, object_typ == "wegdeel")
    barren <<- subset(BGT_outerBuffer, object_typ == "onbegroeidterreindeel")
    vegetation <<- subset(BGT_outerBuffer, object_typ == "begroeidterreindeel")
    
    selected_aws_row <- which(temperature_criteria.df == aws_name)
    temperature_criteria.df[selected_aws_row,outer_objectCountColName] <- nrow(artificial_objects.sf) 
    if(nrow(artificial_objects.sf) == 0){
      temperature_criteria.df[selected_aws_row,outer_sumObjectAreasColName] <- 0
      temperature_criteria.df[selected_aws_row,outer_relAreaColName] <- 0
    } else {
      ##Calculate area objects outer buffer
      area_objects_outer_buffer <- st_area(artificial_objects.sf)
      units::set_units(area_objects_outer_buffer, m^2)
      sum_area_objects_outer_buffer <- sum(area_objects_outer_buffer)
      temperature_criteria.df[selected_aws_row,outer_sumObjectAreasColName] <- sum_area_objects_outer_buffer
      
      relArea_outer_buffer <- sum_area_objects_outer_buffer / area_outer_buffer
      temperature_criteria.df[selected_aws_row,outer_relAreaColName] <- relArea_outer_buffer 
      if(class == 1 | class == 2){
        ##select annnulus region
  
        
        #create annulus
        annulus <- createAnnulus(coords, region)
        
        #intersection annulus
        st_agr(annulus) <- "constant"
        st_agr(artificial_objects.sf) <- "constant"
        annulus_insct.sf <- st_intersection(artificial_objects.sf, annulus)
  
        #count amount of objects annulus
        temperature_criteria.df[selected_aws_row,annulus_objectCountColName] <- nrow(annulus_insct.sf)
        
        #calcuate area
        area_annulus <- st_area(outer_buffer)
        units::set_units(area_annulus, m^2)
        
        area_objects_annulus <- st_area(annulus_insct.sf)
        units::set_units(area_objects_annulus, m^2)
        sum_area_objects_annulus <- sum(area_objects_annulus)
        relArea_annulus <- sum_area_objects_annulus / area_annulus
        
        temperature_criteria.df[selected_aws_row,annulus_sumObjectAreasColName] <- sum_area_objects_annulus
        temperature_criteria.df[selected_aws_row,annulus_relAreaColName] <- relArea_annulus 
      }
      
      ##create inner buffer, intersect and calculate area
      #craete inner buffer
      inner_buffer <- createBuffer(coords, inner_buffer_dist)
      
      #intersect inner buffer inner buffer
      st_agr(inner_buffer) <- "constant"
      st_agr(artificial_objects.sf) <- "constant"
      inner_buffer_insct.sf <- st_intersection(artificial_objects.sf, inner_buffer)
      
      #count amount of objects
      temperature_criteria.df[selected_aws_row,inner_objectCountColName] <- nrow(inner_buffer_insct.sf)
      
      #calculate area
      area_inner_buffer <- st_area(inner_buffer)
      units::set_units(area_outer_buffer, m^2)
      
      area_objects_inner_buffer <- st_area(inner_buffer_insct.sf)
      units::set_units(area_objects_inner_buffer, m^2)
      sum_area_objects_inner_buffer <- sum(area_objects_inner_buffer)
      
      relArea_inner_buffer <- sum_area_objects_inner_buffer / area_inner_buffer
      
      temperature_criteria.df[selected_aws_row,inner_sumObjectAreasColName] <- sum_area_objects_inner_buffer
      temperature_criteria.df[selected_aws_row,inner_relAreaColName] <- relArea_inner_buffer 
    }
    
    #view data on map
    # buildings_rd.sf <- subset(shape_insct.sf, object_typ == "pand")
    # buildingds_wgs.sf <<- st_transform(buildings_rd.sf, "+init=epsg:4326")
    # 
    # water_rd.sf <- subset(shape_insct.sf, object_typ == "waterdeel")
    # water_wgs.sf <<- st_transform(water_rd.sf, "+init=epsg:4326")
    # 
    # roads_rd.sf <- subset(shape_insct.sf, object_typ == "wegdeel")
    # roads_wgs.sf <<- st_transform(roads_rd.sf, "+init=epsg:4326")
   
    #inner_buffer_feature_wgs <<- st_transform(inner_buffer_insct.sf, "+init=epsg:4326")
    #coords_wgs.sf <- st_transform(coords, "+init=epsg:4326")
    
    #meet all class criteria
    meet_class_criteria <- temperature_criteria.df[selected_aws_row,meet_classNr]
    outer_buffer_object_count <- temperature_criteria.df[selected_aws_row, outer_objectCountColName]
    if(class == 1 | class == 2){
      if(temperature_criteria.df[selected_aws_row, outer_objectCountColName] == 0){
        print(paste("No artifical objects are found within ", toString(outer_buffer_dist), "m.", sep=""))
        temperature_criteria.df[selected_aws_row,meet_classNr] <- TRUE
      } else if(temperature_criteria.df[selected_aws_row, outer_relAreaColName] < 0.1 &
                temperature_criteria.df[selected_aws_row, annulus_relAreaColName] < 0.05 &
                temperature_criteria.df[selected_aws_row, inner_relAreaColName] < 0.01){
        temperature_criteria.df[selected_aws_row,meet_classNr] <- TRUE
        print(paste(toString(outer_buffer_object_count), " artifical objects found within ", toString(outer_buffer_dist), "m buffer ", "but NOT significant size in area within radii.", sep=""))
      } else {
        temperature_criteria.df[selected_aws_row,meet_classNr] <- FALSE
        print(paste(toString(outer_buffer_object_count), " artifical objects found within ", toString(outer_buffer_dist), "m buffer ", "but significant size in area within radii.", sep=""))
      }
    } else if(class == 3){
      if(temperature_criteria.df[selected_aws_row, outer_objectCountColName] == 0){
        temperature_criteria.df[selected_aws_row,meet_classNr] <- TRUE
        print(paste("No artifical objects are found within ", toString(outer_buffer_dist), "m.", sep=""))
      } else if(temperature_criteria.df[selected_aws_row, outer_relAreaColName] < 0.1 &
        temperature_criteria.df[selected_aws_row, inner_relAreaColName] < 0.05){
        temperature_criteria.df[selected_aws_row,meet_classNr] <- TRUE
      } else {
        temperature_criteria.df[selected_aws_row,meet_classNr] <- FALSE
        print(paste(toString(outer_buffer_object_count), " artifical objects found within ", toString(outer_buffer_dist), "m buffer ", "but significant size in area within radii.", sep=""))
      }
    } else if(class == 4){
      if(temperature_criteria.df[selected_aws_row, outer_relAreaColName] < 0.5 &
        temperature_criteria.df[selected_aws_row, inner_relAreaColName] < 0.3){
        temperature_criteria.df[selected_aws_row,meet_classNr] <- TRUE
        print(paste(toString(outer_buffer_object_count), " artifical objects found within ", toString(outer_buffer_dist), "m buffer ", "but NOT significant size in area within radii.", sep=""))
      } else {
        temperature_criteria.df[selected_aws_row,meet_classNr] <- FALSE
        print(paste(toString(outer_buffer_object_count), " artifical objects found within ", toString(outer_buffer_dist), " m buffer ", "but significant size in area within radii.", sep=""))
      }
    }
  
    if(missing(class)){
      #go through all the classes and privide output of all the relevant classes
      if(temperature_criteria.df[selected_aws_row,meet_classNr] == FALSE){
        new_classNr <- class + 1
        print(paste("Going to next class. Checking criteria for class ", toString(new_classNr), "...", sep=""))
        clip_bgt(aws_name = aws_name, coords = coords, bgt_shape = bgt_shape,class = new_classNr, temperature_criteria.df = temperature_criteria.df)
      } else{
        message("BGT clip passed class criteria ", as.character(class))
      }
    } else {
      #only provide output of selected class
      if(temperature_criteria.df[selected_aws_row,meet_classNr] == FALSE){
        print(paste("BGT clip did NOT pass class ", as.character(class)," criteria", sep=""))
      } else{
        message("BGT clip passed class criteria ", as.character(class))
      }
    }
  }
return(temperature_criteria.df)}

selectSensor_row <- function (aws_name, sensor_name, aws.df){
  selectedRow <- aws.df[which(aws.df$Sensor == sensor_name & aws.df$AWS == aws_name),]
  if(nrow(selectedRow) == 0 | nrow(selectedRow) > 1){
    selectedRow <- aws.df[which(aws.df$Sensor == "site" & aws.df$AWS == aws_name),]
  }  
  return (selectedRow)}

select_single_aws <- function(aws.df, aws_name, sensor_name){
  single_aws.df<-selectSensor_row(aws_name = aws_name, sensor_name = sensor_name, aws.df = aws.df)
  
  single_aws_rd.sp<-data.frame(single_aws.df)
  coordinates(single_aws_rd.sp) <- ~X+Y
  crs(single_aws_rd.sp)<-CRS("+init=epsg:28992")
  
  single_aws_rd.sf <- st_as_sf(single_aws_rd.sp)
  single_aws_wgs.sf <- st_transform(single_aws_rd.sf, "+init=epsg:4326")
  return(list(aws.df = single_aws.df, aws_rd.sp = single_aws_rd.sp, aws_rd.sf = single_aws_rd.sf, aws_wgs.df = single_aws_wgs.sf))}

selected_aws_site <- select_single_aws(AWS.df, "De Bilt", "site")
selected_aws_temp <- select_single_aws(AWS.df, "De Bilt", "temp_150cm")

temperature_landuse_criteria.df_site <- clip_bgt(selected_aws_site[[1]]$AWS, selected_aws_site[[3]], BGT_station.sf, selected_aws_site[[1]][c(1,5)]2Baud)
temperature_landuse_criteria.df_temp <- clip_bgt(selected_aws_temp[[1]]$AWS, selected_aws_temp[[3]], BGT_station.sf, selected_aws_temp[[1]][c(1,5)])
