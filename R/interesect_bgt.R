#intersect bgt
library(sf)
library(mapview)

presence_objects <- function(aws_name, coords, bgt_shape, temperature_criteria.df, class, go_to_next_class, cv_colName, exportCSV, exportShp){
  #load possible missing values
  if(missing(class)){
    #start at class 1
    class <- 1
  }
  
  if(missing(go_to_next_class)){
    go_to_next_class <- FALSE
  }
  
  if(missing(cv_colName)){
    cv_colName <- "Criteria_Value"
  }
  if(missing(exportCSV)){
    exportCSV = TRUE
  }
  
  if(missing(exportShp)){
    exportShp <- TRUE
  }
  aws_name_trim <- getAWS_name_trim(aws_name)
  ##load criteria
  tluc <- dplyr::filter(guideline_criteria, Variable == "temperature" & Criteria_Type == "land use" & Class == class)
  selected_aws_row <- which(temperature_criteria.df == aws_name)
  ##functions
  createBuffer <- function(coords, distance){
    buffer <- st_buffer(coords, dist=distance)
    st_crs(buffer, epsg_rd)
    return (buffer)}
  
  createAnnulus <- function(coords, region){
    split_values <- strsplit(region, split="_")
    
    outer_ring <- createBuffer(coords, as.numeric(split_values[[1]][2]))
    inner_ring <- createBuffer(coords, as.numeric(split_values[[1]][1]))
    annulus <- st_difference(outer_ring, inner_ring)
    
    st_crs(annulus, epsg_rd)
    return(annulus)}
  
  ##select criteria for buffers and relative area according to class
  outer_buffer_dist <- as.numeric(filter(tluc, Name == "outer buffer distance")[1,cv_colName])
  inner_buffer_dist <- as.numeric(filter(tluc, Name == "inner buffer distance")[1,cv_colName])
  outer_buffer_relArea_criteria <- as.numeric(filter(tluc, Name == "relative area outer buffer")[1,cv_colName])
  annulus_relArea_criteria <- as.numeric(filter(tluc, Name == "relative area annulus")[1,cv_colName])
  inner_buffer_relArea_criteria <- as.numeric(filter(tluc, Name == "relative area inner buffer")[1,cv_colName])
  
  meet_classNr <- paste("meetClass_", toString(class), sep="")
  if(class >= 5){
    temperature_criteria.df[selected_aws_row,meet_classNr] <- TRUE
    annulus_insct.sf <- NULL
    outerBuffer_intsct.sf <- NULL
    inner_buffer_insct.sf <- NULL
  } else {
    #create outer buffer
    outer_buffer <- createBuffer(coords, outer_buffer_dist)
    area_outer_buffer <- st_area(outer_buffer)
    units::set_units(area_outer_buffer, m^2)
    #print(st_crs(bgt_shape))
    #print(st_crs(outer_buffer))
    
    #intersection outer buffer
    st_agr(bgt_shape) <- "constant"
    st_agr(outer_buffer) <- "constant"
    outerBuffer_intsct.sf <- st_intersection(bgt_shape, outer_buffer)
    st_crs(outerBuffer_intsct.sf, epsg_rd)
    
    #select artifical objects within outer buffer
    artificial_objects.sf <- subset(outerBuffer_intsct.sf, object_typ == "pand" | object_typ == "wegdeel" | object_typ == "waterdeel")
    buildings <- subset(outerBuffer_intsct.sf, object_typ == "pand")
    water <- subset(outerBuffer_intsct.sf, object_typ == "waterdeel")
    roads <- subset(outerBuffer_intsct.sf, object_typ == "wegdeel")
    barren <- subset(outerBuffer_intsct.sf, object_typ == "onbegroeidterreindeel")
    vegetation <- subset(outerBuffer_intsct.sf, object_typ == "begroeidterreindeel")
    
    
    ##column names
    meet_noObjects_outerBuffer_criteria <- paste("meet_noObjects_",as.character(outer_buffer_dist),"m_c",toString(class),"_criteria", sep="")
    
    outer_objectCountColName <- paste("objectCount_",as.character(outer_buffer_dist),"m", sep="")
    outer_sumObjectAreasColName <- paste("sumObjectAreas_",as.character(outer_buffer_dist),"m", sep="")
    outer_relAreaColName <- paste("relAreaBuffer_",as.character(outer_buffer_dist),"m", sep="")
    meet_outerBuffer_criteria <- paste("meet_",as.character(outer_buffer_dist),"m_c",toString(class),"_criteria", sep="")
    
    inner_objectCountColName <- paste("objectCount_",as.character(inner_buffer_dist),"m", sep="")
    inner_sumObjectAreasColName <- paste("sumObjectAreas_",as.character(inner_buffer_dist),"m", sep="")
    inner_relAreaColName <- paste("relAreaBuffer_",as.character(inner_buffer_dist),"m", sep="")
    meet_innerBuffer_criteria <- paste("meet_",as.character(inner_buffer_dist),"m_c",toString(class),"_criteria", sep="")
    
    df <- data.frame(outer_objectCount = numeric(nrow(artificial_objects.sf)),
                     meet_noObjectsOuterBufer_criteria =  logical(nrow(artificial_objects.sf)),
                     outer_sumAreas = character(nrow(artificial_objects.sf)),
                     outer_relArea = numeric(nrow(artificial_objects.sf)),
                     meet_ObjectsouterBuffer_criteria = logical(nrow(artificial_objects.sf)),
                     inner_objectCount = numeric(nrow(artificial_objects.sf)),
                     inner_sumAreas = character(nrow(artificial_objects.sf)),
                     inner_relArea = numeric(nrow(artificial_objects.sf)),
                     meet_ObjectsinnerBuffer_criteria = logical(nrow(artificial_objects.sf)),
                     meetClass = logical(nrow(artificial_objects.sf)))
    
    colnames(df) <- c(outer_objectCountColName,
                      meet_noObjects_outerBuffer_criteria,
                      outer_sumObjectAreasColName,
                      outer_relAreaColName,
                      meet_outerBuffer_criteria,
                      inner_objectCountColName,
                      inner_sumObjectAreasColName,
                      inner_relAreaColName,
                      meet_innerBuffer_criteria,
                      meet_classNr)
    
    
    
  
    if(class == 1 | class == 2){
      region <- filter(tluc, Name == "annulus distance")[1,cv_colName]
      annulus_objectCountColName <- paste("objectCount_",as.character(region),"m", sep="")
      annulus_sumObjectAreasColName <- paste("sumObjectAreas_",as.character(region),"m", sep="")
      annulus_relAreaColName <- paste("relAreaRegion_",as.character(region),"m", sep="")
      meet_annulus_criteria <- paste("meet_",as.character(region),"annulus_c",toString(class),"_criteria", sep="")
      
      df_annulus <- data.frame(objectCount = character(nrow(artificial_objects.sf)), sumAreas = character(nrow(artificial_objects.sf)), relArea = character(nrow(artificial_objects.sf)), meet_annulus_Criteria = logical(nrow(artificial_objects.sf)))
      colnames(df_annulus) <- c( annulus_objectCountColName, annulus_sumObjectAreasColName, annulus_relAreaColName, meet_annulus_criteria)
      df <- cbind(df, df_annulus)
    }
    #convert input data to SF
    #coordinates
    #coords.sf <- st_as_sf(coords)
    #outer buffer area

    
    #units::set_units(shape_insct.sf, m^2)
    

    temperature_criteria.df[selected_aws_row,outer_objectCountColName] <- nrow(artificial_objects.sf)
    if(nrow(artificial_objects.sf) == 0){
      temperature_criteria.df[selected_aws_row,outer_sumObjectAreasColName] <- 0
      temperature_criteria.df[selected_aws_row,outer_relAreaColName] <- 0
      temperature_criteria.df[selected_aws_row,inner_sumObjectAreasColName] <- 0
      temperature_criteria.df[selected_aws_row,inner_relAreaColName] <- 0
      inner_buffer_insct.sf <- NULL
      if(class == 1 | class == 2){
        temperature_criteria.df[selected_aws_row,annulus_sumObjectAreasColName] <- 0
        temperature_criteria.df[selected_aws_row,annulus_relAreaColName] <- 0
      
      }
      annulus_insct.sf <- NULL
    } else {
      ##Calculate area objects outer buffer
      area_objects_outer_buffer <- st_area(artificial_objects.sf)
      units::set_units(area_objects_outer_buffer, m^2)
      sum_area_objects_outer_buffer <- sum(area_objects_outer_buffer)
      temperature_criteria.df[selected_aws_row,outer_sumObjectAreasColName] <- sum_area_objects_outer_buffer
      
      relArea_outer_buffer <- as.numeric(sum_area_objects_outer_buffer) / as.numeric(area_outer_buffer)
      temperature_criteria.df[selected_aws_row,outer_relAreaColName] <- relArea_outer_buffer
      if(class == 1 | class == 2){
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
      } else {
        annulus_insct.sf <- NULL
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
    
    #meet individual criteria
    #inner and outer buffer relative areas
    if(temperature_criteria.df[selected_aws_row,outer_relAreaColName] < outer_buffer_relArea_criteria){
      temperature_criteria.df[selected_aws_row,meet_outerBuffer_criteria] <- TRUE
    } else {
      temperature_criteria.df[selected_aws_row,meet_outerBuffer_criteria] <- FALSE
    }
    if(temperature_criteria.df[selected_aws_row, inner_relAreaColName] < inner_buffer_relArea_criteria){
      temperature_criteria.df[selected_aws_row,meet_innerBuffer_criteria] <- TRUE
    } else {
      temperature_criteria.df[selected_aws_row,meet_innerBuffer_criteria] <- FALSE
    }
    #no objects within outer buffer
    if(class != 4){ #Class 1, 2 or 3
      if(temperature_criteria.df[selected_aws_row, outer_objectCountColName] == 0){
        temperature_criteria.df[selected_aws_row, meet_noObjects_outerBuffer_criteria] <- TRUE
      } else {
        temperature_criteria.df[selected_aws_row, meet_noObjects_outerBuffer_criteria] <- FALSE
      }
    }
    
    #annulus relative area
    if(class == 1 | class == 2){
      if(temperature_criteria.df[selected_aws_row, annulus_relAreaColName] < annulus_relArea_criteria){
        temperature_criteria.df[selected_aws_row, meet_annulus_criteria] <- TRUE
      } else {
        temperature_criteria.df[selected_aws_row, meet_annulus_criteria] <- FALSE
      }
    }
    #meet all class criteria column
    outer_buffer_object_count <- temperature_criteria.df[selected_aws_row, outer_objectCountColName]
    if(class == 1 | class == 2){
      if(temperature_criteria.df[selected_aws_row, outer_objectCountColName] == 0){
        print(paste("No artifical objects are found within ", toString(outer_buffer_dist), "m.", "Class ", class, " is met.", sep=""))
        temperature_criteria.df[selected_aws_row,meet_classNr] <- TRUE
        if(class == 1){
          temperature_criteria.df[selected_aws_row,"meet_class2"] <- NA
          temperature_criteria.df[selected_aws_row,"meet_class3"] <- NA
          temperature_criteria.df[selected_aws_row,"meet_class4"] <- NA
          temperature_criteria.df[selected_aws_row,"meet_class5"] <- NA
        } else if(class == 2){
          temperature_criteria.df[selected_aws_row,"meet_class3"] <- NA
          temperature_criteria.df[selected_aws_row,"meet_class4"] <- NA
          temperature_criteria.df[selected_aws_row,"meet_class5"] <- NA
        }
      } else if(temperature_criteria.df[selected_aws_row, meet_outerBuffer_criteria] == TRUE &
                temperature_criteria.df[selected_aws_row, meet_annulus_criteria] == TRUE &
                temperature_criteria.df[selected_aws_row, meet_innerBuffer_criteria] == TRUE){
        temperature_criteria.df[selected_aws_row,meet_classNr] <- TRUE
        if(class == 1){
          temperature_criteria.df[selected_aws_row,"meet_class2"] <- NA
          temperature_criteria.df[selected_aws_row,"meet_class3"] <- NA
          temperature_criteria.df[selected_aws_row,"meet_class4"] <- NA
          temperature_criteria.df[selected_aws_row,"meet_class5"] <- NA
        } else if(class == 2){
          temperature_criteria.df[selected_aws_row,"meet_class3"] <- NA
          temperature_criteria.df[selected_aws_row,"meet_class4"] <- NA
          temperature_criteria.df[selected_aws_row,"meet_class5"] <- NA
        }
        print(paste(toString(outer_buffer_object_count), " artifical objects found within ", toString(outer_buffer_dist), "m buffer ", "but NOT significant size in area within radii. Class ", class, " is met.", sep=""))
      } else {
        temperature_criteria.df[selected_aws_row,meet_classNr] <- FALSE
        print(paste(toString(outer_buffer_object_count), " artifical objects found within ", toString(outer_buffer_dist), "m buffer ", "but significant size in area within radii. Class ", class, " is not met.", sep=""))
      }
    } else if(class == 3){
      if(temperature_criteria.df[selected_aws_row, outer_objectCountColName] == 0){
        temperature_criteria.df[selected_aws_row,meet_classNr] <- TRUE
        print(paste("No artifical objects are found within ", toString(outer_buffer_dist), "m.", "Class ", class, " is met.", sep=""))
        temperature_criteria.df[selected_aws_row,"meet_class4"] <- NA
        temperature_criteria.df[selected_aws_row,"meet_class5"] <- NA
      } else if(temperature_criteria.df[selected_aws_row, meet_outerBuffer_criteria] == TRUE &
                temperature_criteria.df[selected_aws_row, meet_innerBuffer_criteria] == TRUE){
        temperature_criteria.df[selected_aws_row,meet_classNr] <- TRUE
        temperature_criteria.df[selected_aws_row,"meet_class4"] <- NA
        temperature_criteria.df[selected_aws_row,"meet_class5"] <- NA
      } else {
        temperature_criteria.df[selected_aws_row,meet_classNr] <- FALSE
        print(paste(toString(outer_buffer_object_count), " artifical objects found within ", toString(outer_buffer_dist), "m buffer ", "but significant size in area within radii. Class ", class, " is not met.", sep=""))
      }
    } else if(class == 4){
      if(temperature_criteria.df[selected_aws_row, meet_outerBuffer_criteria] == TRUE &
        temperature_criteria.df[selected_aws_row, meet_innerBuffer_criteria] == TRUE){
        temperature_criteria.df[selected_aws_row,meet_classNr] <- TRUE
        temperature_criteria.df[selected_aws_row,"meet_class5"] <- NA
        print(paste(toString(outer_buffer_object_count), " artifical objects found within ", toString(outer_buffer_dist), "m buffer ", "but NOT significant size in area within radii. Class ", class, " is met.", sep=""))
      } else {
        temperature_criteria.df[selected_aws_row,meet_classNr] <- FALSE
        print(paste(toString(outer_buffer_object_count), " artifical objects found within ", toString(outer_buffer_dist), " m buffer ", "but significant size in area within radii. Class ", class, " is not met.", sep=""))
        temperature_criteria.df[selected_aws_row,"meet_class5"] <- TRUE
      }
    }
  }
  if(temperature_criteria.df[selected_aws_row,meet_classNr] == FALSE){
    if(missing(class) & missing(class)){
        #go through all the classes and privide output of all the relevant classes, starting at class 1
        if(temperature_criteria.df[selected_aws_row,meet_classNr] == FALSE){
          new_classNr <- class + 1
          print(paste("Going to next class. Checking criteria for class ", toString(new_classNr), "...", sep=""))
          go_to_next_class = TRUE
          presence_objects(aws_name = aws_name, coords = coords, bgt_shape = bgt_shape,class = new_classNr, temperature_criteria.df = temperature_criteria.df, go_to_next_class = TRUE, exportCSV = exportCSV, exportShp = exportShp)
        } else{
          message("BGT clip passed class criteria ", class)
        }
      } else if(missing(class) == FALSE & missing(go_to_next_class) == FALSE){
        if(temperature_criteria.df[selected_aws_row,meet_classNr] == FALSE){
          new_classNr <- class + 1
          print(paste("Going to next class. Checking criteria for class ", toString(new_classNr), "...", sep=""))
          go_to_next_class = TRUE
          presence_objects(aws_name = aws_name, coords = coords, bgt_shape = bgt_shape,class = new_classNr, temperature_criteria.df = temperature_criteria.df, go_to_next_class = TRUE, exportCSV = exportCSV, exportShp = exportShp)
        } else{
          message("BGT clip passed class criteria ", class)
        }
      } else if(missing(class) == FALSE & missing(go_to_next_class)){
        #only provide output of selected class
        if(temperature_criteria.df[selected_aws_row,meet_classNr] == FALSE){
          print(paste("BGT clip did NOT pass class ", class," criteria", sep=""))
        } else{
          message("BGT clip passed class criteria ", class)
        }
      }
  } else {
    temperature_criteria.df[selected_aws_row,"final_classNr"] <- class
    message("BGT clip passed criteria for class ", class, ".")
  }
  
  #export
  if(exportCSV == TRUE | exportShp == TRUE){
    if(!dir.exists("output/objects")){
      dir.create("output/objects")
    }
    if(!dir.exists(paste0("output/objects/",aws_name_trim))){
      dir.create(paste0("output/objects/", aws_name_trim))
    }
    output_path <- paste0("output/objects/", aws_name_trim, "/")
  }
  if(exportCSV == TRUE){
    fwrite(temperature_criteria.df, paste0(output_path, "objects_classes.csv"))
  }
  if(exportShp == TRUE){
    st_write(obj = outerBuffer_intsct.sf, dsn = paste0(output_path, aws_name_trim, "_", outer_buffer_dist, "m_buffer.shp"), driver = "ESRI Shapefile", overwrite_layer = TRUE)
    st_write(obj = inner_buffer_insct.sf, dsn = paste0(output_path, aws_name_trim, "_", inner_buffer_dist, "m_buffer.shp"), driver = "ESRI Shapefile", overwrite_layer = TRUE)
    if(class == 1 | class == 2){
      st_write(obj = annulus_insct.sf, dsn = paste0(output_path, aws_name_trim, "_", region, "m_annulus.shp"), driver = "ESRI Shapefile", overwrite_layer = TRUE)
    }
  }
  return(list("df" = temperature_criteria.df, "outer_buf" = outerBuffer_intsct.sf, "annulus" = annulus_insct.sf,"inner_buf" = inner_buffer_insct.sf))
}

selected_aws_site <- select_single_aws(AWS.df, "De Bilt", "site")
selected_aws_temp <- select_single_aws(AWS.df, "De Bilt", "temp_150cm")

BGT_station_adjust.sp <- readOGR(dsn = "data/BGT/deBilt", "BGT_DeBilt_adjustments", stringsAsFactors=FALSE)
crs(BGT_station_adjust.sp) <- crs(epsg_rd)
BGT_station_adjust.sf <- st_as_sf(BGT_station_adjust.sp)

temperature_landuse_criteria.df_site <- presence_objects(aws_name = "De Bilt", coords = selected_aws_site[[3]], bgt_shape = BGT_DeBilt[[1]], temperature_criteria.df = selected_aws_site[[1]][,c(1,5)])
temperature_landuse_criteria.df_temp <- presence_objects(aws_name = selected_aws_temp[[1]]$AWS, coords = selected_aws_temp[[3]], bgt_shape = BGT_DeBilt[[1]], temperature_criteria.df = selected_aws_temp[[1]][,c(1,5)])
mapview(BGT_station_adjust.sf)
View(temperature_landuse_criteria.df_site[["df"]])
