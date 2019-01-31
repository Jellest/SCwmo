#intersect bgt
library(sf)
library(mapview)

find_land_use_and_classes <- function(aws.df = AWS.df, aws_name, addition = "", coords, bgt_shape, land_use_criteria.df, class, go_to_next_class, criteria_columnName = "Criteria_Value", exportCSV = FALSE, exportShp = FALSE){
  #load possible missing values
  if(missing(class)){
    #start at class 1
    class <- 1
  }
  
  if(missing(go_to_next_class)){
    go_to_next_class <- FALSE
  }
  
  aws_name_trim <- getAWS_name_trim(aws.df = aws.df, aws_name = aws_name, addition = addition)
  ##load criteria
  tluc <- dplyr::filter(guideline_criteria, Variable == "temperature" & Criteria_Type == "land use" & Class == class)
  tluc <- check_criteria(df = tluc, criteria_columnName = criteria_columnName)
  #selected_aws_row <- which(land_use_criteria.df == aws_name)
  ##functions
  createAnnulus <- function(coords, region){
    split_values <- strsplit(region, split="_")
    
    outer_ring <- createBuffer(coords, as.numeric(split_values[[1]][2]))
    inner_ring <- createBuffer(coords, as.numeric(split_values[[1]][1]))
    st_agr(outer_ring) <- "constant"
    st_agr(inner_ring) <- "constant"
    annulus <- st_difference(outer_ring, inner_ring)
    
    st_crs(annulus, epsg_rd)
    return(annulus)}
  
  determine_buffers <- function(class){
    
    if(class == 1){
      buffers <- c(100, 30, 10)  
    } else if(class == 2 | class == 3){
      buffers <- c(30, 10, 5)  
    } else if(class == 4){
      buffers <- c(10, 3)  
    } else if(class ==5){
      buffers <- c(30, 10, 5, 3)   
    }
    buffers <- c(100, 30, 10, 5, 3)
  return (buffers)
  }
  
  ##select criteria for buffers and relative area according to class
  outer_buffer_dist <- as.numeric(dplyr::filter(tluc, Name == "outer buffer distance")[1,criteria_columnName])
  inner_buffer_dist <- as.numeric(dplyr::filter(tluc, Name == "inner buffer distance")[1,criteria_columnName])
  outer_buffer_relArea_criteria <- as.numeric(dplyr::filter(tluc, Name == "relative area outer buffer")[1,criteria_columnName])
  annulus_relArea_criteria <- as.numeric(dplyr::filter(tluc, Name == "relative area annulus")[1,criteria_columnName])
  inner_buffer_relArea_criteria <- as.numeric(dplyr::filter(tluc, Name == "relative area inner buffer")[1,criteria_columnName])
  
  meet_classNr <- paste("meetClass_", toString(class), sep="")
  if(class >= 5){
    land_use_criteria.df[1,meet_classNr] <- TRUE
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
    artificial_objects.sf <- subset(outerBuffer_intsct.sf, object_typ == "pand" | object_typ == "wegdeel" | object_typ == "waterdeel" | object_typ == "overigbouwwerk" | object_typ == "scheiding")
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
      region <- dplyr::filter(tluc, Name == "annulus distance")[1,criteria_columnName]
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

    land_use_criteria.df[1,outer_objectCountColName] <- nrow(artificial_objects.sf)
    if(nrow(artificial_objects.sf) == 0){
      land_use_criteria.df[1,outer_sumObjectAreasColName] <- 0
      land_use_criteria.df[1,outer_relAreaColName] <- 0
      land_use_criteria.df[1,inner_sumObjectAreasColName] <- 0
      land_use_criteria.df[1,inner_relAreaColName] <- 0
      inner_buffer_insct.sf <- NULL
      if(class == 1 | class == 2){
        land_use_criteria.df[1,annulus_sumObjectAreasColName] <- 0
        land_use_criteria.df[1,annulus_relAreaColName] <- 0
      
      }
      annulus_insct.sf <- NULL
    } else {
      ##Calculate area objects outer buffer
      area_objects_outer_buffer <- st_area(artificial_objects.sf)
      units::set_units(area_objects_outer_buffer, m^2)
      sum_area_objects_outer_buffer <- sum(area_objects_outer_buffer)
      land_use_criteria.df[1,outer_sumObjectAreasColName] <- sum_area_objects_outer_buffer
      
      relArea_outer_buffer <- as.numeric(sum_area_objects_outer_buffer) / as.numeric(area_outer_buffer)
      land_use_criteria.df[1,outer_relAreaColName] <- relArea_outer_buffer
      if(class == 1 | class == 2){
        #create annulus
        annulus <- createAnnulus(coords, region)
        
        #intersection annulus
        st_agr(annulus) <- "constant"
        st_agr(artificial_objects.sf) <- "constant"
        annulus_insct.sf <- st_intersection(artificial_objects.sf, annulus)
        
        #count amount of objects annulus
        land_use_criteria.df[1,annulus_objectCountColName] <- nrow(annulus_insct.sf)
        
        #calcuate area
        area_annulus <- st_area(outer_buffer)
        units::set_units(area_annulus, m^2)
        
        area_objects_annulus <- st_area(annulus_insct.sf)
        units::set_units(area_objects_annulus, m^2)
        sum_area_objects_annulus <- sum(area_objects_annulus)
        relArea_annulus <- sum_area_objects_annulus / area_annulus
        
        land_use_criteria.df[1,annulus_sumObjectAreasColName] <- sum_area_objects_annulus
        land_use_criteria.df[1,annulus_relAreaColName] <- relArea_annulus
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
      land_use_criteria.df[1,inner_objectCountColName] <- nrow(inner_buffer_insct.sf)
      
      #calculate area
      area_inner_buffer <- st_area(inner_buffer)
      units::set_units(area_outer_buffer, m^2)
      
      area_objects_inner_buffer <- st_area(inner_buffer_insct.sf)
      units::set_units(area_objects_inner_buffer, m^2)
      sum_area_objects_inner_buffer <- sum(area_objects_inner_buffer)
      
      relArea_inner_buffer <- sum_area_objects_inner_buffer / area_inner_buffer
      
      land_use_criteria.df[1,inner_sumObjectAreasColName] <- sum_area_objects_inner_buffer
      land_use_criteria.df[1,inner_relAreaColName] <- relArea_inner_buffer
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
    
    if(land_use_criteria.df[1,outer_relAreaColName] < outer_buffer_relArea_criteria){
      land_use_criteria.df[1,meet_outerBuffer_criteria] <- TRUE
    } else {
      land_use_criteria.df[1,meet_outerBuffer_criteria] <- FALSE
    }
    if(land_use_criteria.df[1, inner_relAreaColName] < inner_buffer_relArea_criteria){
      land_use_criteria.df[1,meet_innerBuffer_criteria] <- TRUE
    } else {
      land_use_criteria.df[1,meet_innerBuffer_criteria] <- FALSE
    }
    #no objects within outer buffer
    if(class != 4){ #Class 1, 2 or 3
      if(land_use_criteria.df[1, outer_objectCountColName] == 0){
        land_use_criteria.df[1, meet_noObjects_outerBuffer_criteria] <- TRUE
      } else {
        land_use_criteria.df[1, meet_noObjects_outerBuffer_criteria] <- FALSE
      }
    }
    
    #annulus relative area
    if(class == 1 | class == 2){
      if(land_use_criteria.df[1, annulus_relAreaColName] < annulus_relArea_criteria){
        land_use_criteria.df[1, meet_annulus_criteria] <- TRUE
      } else {
        land_use_criteria.df[1, meet_annulus_criteria] <- FALSE
      }
    }
    #meet all class criteria column
    outer_buffer_object_count <- land_use_criteria.df[1, outer_objectCountColName]
    if(class == 1 | class == 2){
      if(land_use_criteria.df[1, outer_objectCountColName] == 0){
        print(paste("No artifical objects are found within ", toString(outer_buffer_dist), "m.", "Class ", class, " is met.", sep=""))
        land_use_criteria.df[1,meet_classNr] <- TRUE
        if(class == 1){
          land_use_criteria.df[1,"meet_class2"] <- "NA"
          land_use_criteria.df[1,"meet_class3"] <- "NA"
          land_use_criteria.df[1,"meet_class4"] <- "NA"
          land_use_criteria.df[1,"meet_class5"] <- "NA"
        } else if(class == 2){
          land_use_criteria.df[1,"meet_class3"] <- "NA"
          land_use_criteria.df[1,"meet_class4"] <- "NA"
          land_use_criteria.df[1,"meet_class5"] <- "NA"
        }
      } else if(land_use_criteria.df[1, meet_outerBuffer_criteria] == TRUE &
                land_use_criteria.df[1, meet_annulus_criteria] == TRUE &
                land_use_criteria.df[1, meet_innerBuffer_criteria] == TRUE){
        land_use_criteria.df[1,meet_classNr] <- TRUE
        if(class == 1){
          land_use_criteria.df[1,"meet_class2"] <- "NA"
          land_use_criteria.df[1,"meet_class3"] <- "NA"
          land_use_criteria.df[1,"meet_class4"] <- "NA"
          land_use_criteria.df[1,"meet_class5"] <- "NA"
        } else if(class == 2){
          land_use_criteria.df[1,"meet_class3"] <- "NA"
          land_use_criteria.df[1,"meet_class4"] <- "NA"
          land_use_criteria.df[1,"meet_class5"] <- "NA"
        }
        print(paste(toString(outer_buffer_object_count), " artifical objects found within ", toString(outer_buffer_dist), "m buffer ", "but NOT significant size in area within radii. Class ", class, " is met.", sep=""))
      } else {
        land_use_criteria.df[1,meet_classNr] <- FALSE
        print(paste(toString(outer_buffer_object_count), " artifical objects found within ", toString(outer_buffer_dist), "m buffer ", "but significant size in area within radii. Class ", class, " is not met.", sep=""))
      }
    } else if(class == 3){
      if(land_use_criteria.df[1, outer_objectCountColName] == 0){
        land_use_criteria.df[1,meet_classNr] <- TRUE
        print(paste("No artifical objects are found within ", toString(outer_buffer_dist), "m.", "Class ", class, " is met.", sep=""))
        land_use_criteria.df[1,"meet_class4"] <- "NA"
        land_use_criteria.df[1,"meet_class5"] <- "NA"
      } else if(land_use_criteria.df[1, meet_outerBuffer_criteria] == TRUE &
                land_use_criteria.df[1, meet_innerBuffer_criteria] == TRUE){
        land_use_criteria.df[1,meet_classNr] <- TRUE
        land_use_criteria.df[1,"meet_class4"] <- "NA"
        land_use_criteria.df[1,"meet_class5"] <- "NA"
      } else {
        land_use_criteria.df[1,meet_classNr] <- FALSE
        print(paste(toString(outer_buffer_object_count), " artifical objects found within ", toString(outer_buffer_dist), "m buffer ", "but significant size in area within radii. Class ", class, " is not met.", sep=""))
      }
    } else if(class == 4){
      if(land_use_criteria.df[1, meet_outerBuffer_criteria] == TRUE &
        land_use_criteria.df[1, meet_innerBuffer_criteria] == TRUE){
        land_use_criteria.df[1,meet_classNr] <- TRUE
        land_use_criteria.df[1,"meet_class5"] <- "NA"
        print(paste(toString(outer_buffer_object_count), " artifical objects found within ", toString(outer_buffer_dist), "m buffer ", "but NOT significant size in area within radii. Class ", class, " is met.", sep=""))
      } else {
        land_use_criteria.df[1,meet_classNr] <- FALSE
        print(paste(toString(outer_buffer_object_count), " artifical objects found within ", toString(outer_buffer_dist), " m buffer ", "but significant size in area within radii. Class ", class, " is not met.", sep=""))
        land_use_criteria.df[1,"meet_class5"] <- TRUE
      }
    }
  }
  #export
  if(exportCSV == TRUE | exportShp == TRUE){
    if(!dir.exists(paste0("output/", aws_name_trim))){
      dir.create(paste0("output/", aws_name_trim), showWarnings = FALSE)
    }
    if(!dir.exists(paste0("output/", aws_name_trim, "/land_use"))){
      dir.create(paste0("output/", aws_name_trim, "/land_use"), showWarnings = FALSE)
    }
    output_path <- paste0("output/", aws_name_trim, "/land_use/")
  }
  if(exportCSV == TRUE){
    if(land_use_criteria.df[1,meet_classNr] == TRUE){
      land_use_criteria.df[1,"final_class"] <- class
    }
    fwrite(land_use_criteria.df, paste0(output_path, aws_name_trim, "_landUse_classes.csv"))
  }
  if(exportShp == TRUE){
    check_shpExists(paste0(output_path, aws_name_trim, "_", outer_buffer_dist, "m_buffer.shp"))
    check_shpExists(paste0(output_path, aws_name_trim, "_", inner_buffer_dist, "m_buffer.shp"))
    
    st_write(obj = outerBuffer_intsct.sf, dsn = paste0(output_path, aws_name_trim, "_", outer_buffer_dist, "m_buffer.shp"), driver = "ESRI Shapefile")
    st_write(obj = inner_buffer_insct.sf, dsn = paste0(output_path, aws_name_trim, "_", inner_buffer_dist, "m_buffer.shp"), driver = "ESRI Shapefile")
    if(class == 1 | class == 2){
      check_shpExists(paste0(output_path, aws_name_trim, "_", region, "m_annulus.shp"))
      st_write(obj = annulus_insct.sf, dsn = paste0(output_path, aws_name_trim, "_", region, "m_annulus.shp"), driver = "ESRI Shapefile")
    }
  }
  
  #finalize class
  if(land_use_criteria.df[1,meet_classNr] == FALSE){
    if(missing(class) & missing(class)){
        #go through all the classes and privide output of all the relevant classes, starting at class 1
        if(land_use_criteria.df[1,meet_classNr] == FALSE){
          new_classNr <- class + 1
          print(paste("Going to next class. Checking criteria for class ", toString(new_classNr), "...", sep=""))
          go_to_next_class = TRUE
          find_land_use_and_classes(aws.df = aws.df, aws_name = aws_name, addition = addition, coords = coords, bgt_shape = bgt_shape,class = new_classNr, land_use_criteria.df = land_use_criteria.df, go_to_next_class = TRUE, exportCSV = exportCSV, exportShp = exportShp)
        } else{
          buffers <- determine_buffers(class)
          message("Land use passed class criteria ", class)
          return (list("buffers" = buffers))
        }
      } else if(missing(class) == FALSE & missing(go_to_next_class) == FALSE){
        if(land_use_criteria.df[1,meet_classNr] == FALSE){
          new_classNr <- class + 1
          print(paste("Going to next class. Checking criteria for class ", toString(new_classNr), "...", sep=""))
          go_to_next_class = TRUE
          find_land_use_and_classes(aws.df = aws.df, aws_name = aws_name, addition = addition,coords = coords, bgt_shape = bgt_shape,class = new_classNr, land_use_criteria.df = land_use_criteria.df, go_to_next_class = TRUE, exportCSV = exportCSV, exportShp = exportShp)
        } else{
          buffers <- determine_buffers(class)
          message("Land use passed class criteria ", class)
          return(list("df" = land_use_criteria.df, "buffers" = buffers, "outer_buf" = outerBuffer_intsct.sf, "annulus" = annulus_insct.sf,"inner_buf" = inner_buffer_insct.sf))
        }
      } else if(missing(class) == FALSE & missing(go_to_next_class)){
        #only provide output of selected class
        if(land_use_criteria.df[1,meet_classNr] == FALSE){
          buffers <- determine_buffers(class)
          print(paste("Land use did NOT pass class ", class," criteria", sep=""))
          return(list("df" = land_use_criteria.df, "buffers" = buffers, "outer_buf" = outerBuffer_intsct.sf, "annulus" = annulus_insct.sf,"inner_buf" = inner_buffer_insct.sf))
        } else {
          buffers <- determine_buffers(class)
          message("Land use passed class criteria ", class)
          return(list("df" = land_use_criteria.df, "buffers" = buffers, "outer_buf" = outerBuffer_intsct.sf, "annulus" = annulus_insct.sf,"inner_buf" = inner_buffer_insct.sf))
          
        }
      }
  } else {
    land_use_criteria.df[1,"final_class"] <- class
    buffers <- determine_buffers(class)
    message("Land use passed criteria for class ", class, ".")
    return(list("df" = land_use_criteria.df, "buffers" = buffers, "outer_buf" = outerBuffer_intsct.sf, "annulus" = annulus_insct.sf,"inner_buf" = inner_buffer_insct.sf))
  }
}


## test scripts
# selected_aws_site <- select_single_aws(AWS.df, "De Bilt", "site")
# selected_aws_temp <- select_single_aws(AWS.df, "De Bilt", "temp_150cm")
# 
# BGT_station_adjust.sp <- readOGR(dsn = "data/BGT/deBilt", "BGT_DeBilt_adjustments", stringsAsFactors=FALSE)
# crs(BGT_station_adjust.sp) <- crs(epsg_rd)
# BGT_station_adjust.sf <- st_as_sf(BGT_station_adjust.sp)
# 
# temperature_landuse_criteria.df_site <- find_land_use_and_classes(aws_name = "De Bilt", coords = selected_aws_site[[3]], bgt_shape = BGT_DeBilt[[1]], land_use_criteria.df = selected_aws_site[[1]][,c(1,5)])
# temperature_landuse_criteria.df_temp <- find_land_use_and_classes(aws_name = selected_aws_temp[[1]]$AWS, coords = selected_aws_temp[[3]], bgt_shape = BGT_DeBilt[[1]], land_use_criteria.df = selected_aws_temp[[1]][,c(1,5)])
# mapview(BGT_station_adjust.sf)
# View(temperature_landuse_criteria.df_site[["df"]])
