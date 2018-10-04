#intersect bgt
library(sf)
library(mapview)

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

clip_bgt <- function(aws_name, coords, bgt_shape, class, temperature_criteria.df){
    #only select objects
    objects_bgt <- subset(bgt_shape, object_typ == "pand" | object_typ == "waterdeel" | object_typ == "wegdeel")
    
    #select buffer areas according to class
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
    
    #column names
    meet_classNr <- paste("meetClass_", toString(class), sep="") 
    
    outer_objectCountColName <- paste("objectCount_",as.character(outer_buffer_dist),"m", sep="")
    outer_sumObjectAreasColName <- paste("sumObjectAreas_",as.character(outer_buffer_dist),"m", sep="")
    outer_relAreaColName <- paste("relAreaBuffer_",as.character(outer_buffer_dist),"m", sep="")
    
    inner_objectCountColName <- paste("objectCount_",as.character(inner_buffer_dist),"m", sep="")
    inner_sumObjectAreasColName <- paste("sumObjectAreas_",as.character(inner_buffer_dist),"m", sep="")
    inner_relAreaColName <- paste("relAreaBuffer_",as.character(inner_buffer_dist),"m", sep="")
  
    df <- data.frame(outer_objectCount = character(nrow(objects_bgt)), outer_sumAreas = character(nrow(objects_bgt)), outer_relArea = character(nrow(objects_bgt)),
                     inner_objectCount = character(nrow(objects_bgt)), inner_sumAreas = character(nrow(objects_bgt)), inner_relArea = character(nrow(objects_bgt)),
                     meetClass = logical(nrow(objects_bgt)))
                    
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
      
        df_annulus <- data.frame(objectCount = character(nrow(objects_bgt)), sumAreas = character(nrow(objects_bgt)), relArea = character(nrow(objects_bgt)))
        colnames(df_annulus) <- c( annulus_objectCountColName, annulus_sumObjectAreasColName, annulus_relAreaColName)
        df <- cbind(df, df_annulus)
      }
      
      
      #convert input data to SF
      #coordinates
      #coords.sf <- st_as_sf(coords)
      coords.sf <- coords
      #outer buffer area
      outer_buffer <- createBuffer(coords.sf, outer_buffer_dist)
      area_outer_buffer <- st_area(outer_buffer)
      units::set_units(area_outer_buffer, m^2)
      
      ##intersection
      st_agr(objects_bgt) = "constant"
      st_agr(outer_buffer) = "constant"
      
      BGT <<- st_intersection(bgt_shape, outer_buffer)
      st_crs(BGT, "+init=epsg:28992")
      artificial_objects.sf <<- subset(BGT, object_typ == "pand" | object_typ == "wegdeel" | object_typ == "waterdeel")
      buildings <<- subset(BGT, object_typ == "pand")
      water <<- subset(BGT, object_typ == "waterdeel")
      roads <<- subset(BGT, object_typ == "wegdeel")
      barren <<- subset(BGT, object_typ == "onbegroeidterreindeel")
      vegetation <<- subset(BGT, object_typ == "begroeidterreindeel")
      #units::set_units(shape_insct.sf, m^2)
      
      selected_aws_row <- which(temperature_criteria.df == aws_name)
      temperature_criteria.df[selected_aws_row,outer_objectCountColName] <- nrow(artificial_objects.sf) 
      if(nrow(shape_insct.sf) == 0){
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
          annulus <- createAnnulus(coords.sf, region)
          
          #intersection annulus
          st_agr(annulus) = "constant"
          annulus_insct.sf <- st_intersection(objects_bgt, annulus)
    
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
        inner_buffer <- createBuffer(coords.sf, inner_buffer_dist)
        
        #intersect inner buffer inner buffer
        st_agr(inner_buffer) = "constant"
        inner_buffer_insct.sf <- st_intersection(objects_bgt, inner_buffer)
        
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
     
      inner_buffer_feature_wgs <<- st_transform(inner_buffer_insct.sf, "+init=epsg:4326")
      coords_wgs.sf <<- st_transform(coords.sf, "+init=epsg:4326")
      
      #meet all class criteria
      meet_class_criteria <- temperature_criteria.df[selected_aws_row,meet_classNr]
      if(class == 1 | class == 2){
        if(temperature_criteria.df[selected_aws_row, outer_objectCountColName] == 0){
          temperature_criteria.df[selected_aws_row,meet_classNr] <- TRUE
        } else if(temperature_criteria.df[selected_aws_row, outer_relAreaColName] < 0.1 &
                  temperature_criteria.df[selected_aws_row, annulus_relAreaColName] < 0.05 &
                  temperature_criteria.df[selected_aws_row, inner_relAreaColName] < 0.01){
          temperature_criteria.df[selected_aws_row,meet_classNr] <- TRUE
        } else {
          temperature_criteria.df[selected_aws_row,meet_classNr] <- FALSE
        }
      } else if(class == 3){
        if(temperature_criteria.df[selected_aws_row, outer_objectCountColName] == 0){
          temperature_criteria.df[selected_aws_row,meet_classNr] <- TRUE
        } else if(temperature_criteria.df[selected_aws_row, outer_relAreaColName] < 0.1 &
                  temperature_criteria.df[selected_aws_row, inner_relAreaColName] < 0.05){
          temperature_criteria.df[selected_aws_row,meet_classNr] <- TRUE
        } else {
          temperature_criteria.df[selected_aws_row,meet_classNr] <- FALSE
        }
      } else if(class == 4){
        if(temperature_criteria.df[selected_aws_row, outer_relAreaColName] < 0.5 &
           temperature_criteria.df[selected_aws_row, inner_relAreaColName] < 0.3){
          temperature_criteria.df[selected_aws_row,meet_classNr] <- TRUE
        } else {
          temperature_criteria.df[selected_aws_row,meet_classNr] <- FALSE
        }
      }
    
      # if(temperature_criteria.df[selected_aws_row,meet_classNr] == FALSE){
      #   new_classNr <- class + 1
      #   clip_bgt("De Bilt", aws_debilt_rd.sp, BGT_station.sf,class = new_classNr, temperature_criteria.df)
      # } else{
      #   message("BGT clip passed class criteria ", as.character(class))
      # }
    }
  
return(temperature_criteria.df)}



selectSensor_row <- function (aws_name, sensor_name, aws){
  selectedRow <- aws[which(Sensor == sensor_name & AWS == aws_name)]
  if(nrow(selectedRow) == 0 | nrow(selectedRow) > 1){
    selectedRow <- aws[which(Sensor == "site")]
  }  
  return (selectedRow)}

temperature_landuse_criteria.df <- AWS.df[c(1,5)]
temperature_landuse_criteria.df <- selectSensor_row(aws_name = "De Bilt", sensor_name = "temp_150cm", aws = temperature_landuse_criteria.df)
#temperature_landuse_criteria.df <- temperature_landuse_criteria.df[-(1:10), ]
temperature_landuse_criteria.df <- clip_bgt("De Bilt", deBilt_rd.sf, BGT_station.sf,class = 1, temperature_landuse_criteria.df)


addFeatures(map, coords_wgs.sf, group="AWS")
addFeatures(map, inner_buffer_feature_wgs, group="buildings")


