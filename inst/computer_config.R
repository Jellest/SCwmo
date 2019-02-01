# computer configurations
start <- function(settings){
  hd <- function(settings){
    # make sure home directory is directry that includes  R folder and the data foder.
    home_directories <- c("/nobackup/users/stuurman","C:/Users/Jelle/Dropbox/KNMI/R_SPQJ")
    home_directory <<- home_directories[settings]
    setwd(home_directory)
    print(paste("setup_settings:", settings, sep="  "))
    print(paste("home directory:", home_directory, sep=" "))
  }
  
  libs <- function(){
    library(raster)
    library(rgeos)
    library(rgdal)
    library(gdalUtils)
    #library(gdata)
    library(sp)
    library(sf)
    library(lubridate)
    library(insol)
    library(ggplot2)
    library(leaflet)
    library(mapview)
    library(devtools)
    library(data.table)
    library(dplyr)
    library(horizon)
    library(xtable)
    library(htmlwidgets)
    library(htmltools)
  }
  hd(settings)
  libs()
  
  #global variables
  AWS.df<<-fread("data/coordinates/AWS_coordinates.csv", data.table = FALSE)
  manual_SC_values <<- fread("data/classifcations/manual_classification_values.csv", data.table = FALSE) 
  guideline_criteria <<- fread("wmoSC/data-raw/guideline_criteria.csv", data.table = FALSE)
  epsg_rd <<- "+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.4171,50.3319,465.5524,-0.398957,0.343988,-1.8774,4.0725 +units=m +no_defs"
  temperature_sensor_name <<- "temp_150cm"
  
  AWS_temperature_list.df <<-dplyr::filter(AWS.df, Sensor == temperature_sensor_name)
  AWS_temperature_names <<- as.character(unlist(select(AWS_temperature_list.df, AWS)))
  AWS_temperature_ahn3Only_names <<- setdiff(AWS_temperature_names, c("Maastricht", "Ell", "Arcen", "Hupsel", "Twenthe", "Nieuw Beerta", "Eelde", "Deelen"))
  
  sAWS_names <<- c("De Bilt", "Arcen", "Rotterdam","Vlissingen", "Voorschoten", "Wijk aan zee")
  sAWS_list.df <<- dplyr::filter(AWS.df, AWS %in% sAWS_names)
  
  sAWSahn3_names <<- c("De Bilt", "Rotterdam", "Vlissingen", "Voorschoten", "Wijk aan zee")
  sAWSahn3_list.df <<- dplyr::filter(AWS.df, AWS %in% sAWSahn3_names & Sensor == temperature_sensor_name)
  
  #global  functions
  create_SpatialPoint <<- function(X, Y, LONLAT){
    if(missing(LONLAT)){
      LONLAT = FALSE
    }
    point.sp<-data.frame(X = X, Y = Y, stringsAsFactors=FALSE)
    coordinates(point.sp) <- ~X+Y
    if(LONLAT == FALSE){
      crs(point.sp)<-CRS(epsg_rd)
      point_rd.sp <- point.sp
      point_wgs.sp <- spTransform(point.sp, "+init=epsg:4326") 
    } else {
      crs(point.sp)<-CRS("+init=epsg:4326")
      point_rd.sp <- spTransform(point.sp, epsg_rd)
      point_wgs.sp <- point.sp
    }
    
    return (list("point_rd.sp" = point_rd.sp, "point_wgs.sp" = point_wgs.sp))
  }
  
  check_criteria <<- function(df, criteria_columnName){
    if(missing(criteria_columnName)){
      criteria_columnName <- "Criteria_Value"
    }
    for(a in 1:nrow(df)){
      str <- df[a,criteria_columnName]
      tryCatch({  
        if(grepl(",", str) == TRUE){
          str_adj <- gsub(",", ".", str, fixed = TRUE)
          nr <- as.numeric(str_adj)
          df[a,criteria_columnName] <- nr
        }
      }, error=function(e){
            message(e)
            stop("No correct Criteria value column name is selected. Change it or leave it out for default WMO Crteria gudeline values.")
      })
    }
    return (df)
  }
  check_aws_names <<- function(aws.df = AWS.df, aws_name, sensor_name){
    check_existence <- aws.df[which(aws.df$AWS == aws_name & aws.df$Sensor == sensor_name),]
    if(nrow(check_existence) == 0 | nrow(check_existence) > 1){
      check_existence <- aws.df[which(aws.df$AWS == aws_name & aws.df$Sensor == "site"),]
      if(nrow(check_existence) == 0){
        check_existence <- aws.df[which(aws.df$AWS == aws_name),]
        if(nrow(check_existence) == 0){
          stop("No AWS found with this name and/or sensor name.")
        } else if(nrow(check_existence) == 1){
           my_sensor_name <- check_existence[1,"Sensor"]
           message(paste0("Single entry AWS name was for for", aws_name, "using ", my_sensor_name," as sensor name."))
           return (check_existence) 
        } else{
          print(check_existence)
          stop(paste0("More than one entry has been found for ", aws_name, ". Please select an AWS name (and sensor name) that returns one entry."))
        }
      } else {
          message(paste(sensor_name, "sensor is not found. 'site' is selected as sensor name."))
          sensor_name <- "site"
          return(check_existence)
      }
    } else if(nrow(check_existence) == 1){
      return(check_existence)
    }
  }
  
  select_single_aws <<- function(aws.df = AWS.df, aws_name, sensor_name){
    if(missing(sensor_name)){
      sensor_name <- "site"
      first_sensor_name <- "site"
    } else {
      first_sensor_name <- sensor_name
    }
    single_aws.df<- selectSensor_row(aws.df = aws.df, aws_name = aws_name, sensor_name = sensor_name)
    #select site if other sensor name is selected.
    if(is.null(single_aws.df) == TRUE){
      sensor_name = "site"
      single_aws.df<- selectSensor_row(aws.df = aws.df, aws_name = aws_name, sensor_name = sensor_name)
      message(paste0(first_sensor_name, " sensor is not found. 'site' is selected as sensor name."))
    }
    
    if(nrow(single_aws.df) == 1){
      print(paste0("Getting AWS coordinates of ", aws_name, " with '", sensor_name, "' as sensor name."))
      single_aws_rd.sp<-data.frame(single_aws.df)
      coordinates(single_aws_rd.sp) <- ~X+Y
      crs(single_aws_rd.sp)<-CRS(epsg_rd)
      
      single_aws_rd.sf <- sf::st_as_sf(single_aws_rd.sp)
      single_aws_rd_copy.sf <- single_aws_rd.sf
      single_aws_wgs.sf <- sf::st_transform(single_aws_rd_copy.sf, "+init=epsg:4326")
      single_aws_wgs.sp <- sf::as_Spatial(single_aws_wgs.sf)
      return(list("aws.df" = single_aws.df,
                  "aws_rd.sp" = single_aws_rd.sp,
                  "aws_rd.sf" = single_aws_rd.sf,
                  "aws_wgs.sp" = single_aws_wgs.sp,
                  "aws_wgs.sf" = single_aws_wgs.sf
      )
      )
    } else {
      warning("No single entry AWS found with this AWS name.")
    }
  }
  
  selectSensor_row <<- function (aws.df = AWS.df, aws_name, sensor_name){
    check_existence <- aws.df[which(aws.df$AWS == aws_name & aws.df$Sensor == sensor_name),] 
    if(nrow(check_existence) == 0){
      stop("No AWS found with this name and/or sensor name.")
      return (NULL)
    } else {
      if(missing(sensor_name)){
        sensor_name = "site"
      }
      selectedRow <- aws.df[which(aws.df$Sensor == sensor_name & aws.df$AWS == aws_name),]
      
      if(nrow(selectedRow) == 0 | nrow(selectedRow) > 1){
        selectedRow <- aws.df[which(aws.df$Sensor == "site" & aws.df$AWS == aws_name),]
      }
      return (selectedRow)
    }
  }
  
  getAWS_name_trim <<- function (aws.df = AWS.df, aws_name, addition = ""){
    aws_name_untrimmed <- aws.df$AWS[which(aws.df$AWS == aws_name)][1]
    if(is.na(aws_name_untrimmed) == TRUE) {
      aws_name_trim <- stop(paste("No AWS station found with the following name:", aws_name))
    } else{
      aws_name_trim <- gsub(" ", "", aws_name_untrimmed)
    }
    if(addition != ""){
      aws_name_trim <- paste0(aws_name_trim, "_", addition)
    }
    return (aws_name_trim)
  }
  
  createBuffer <<- function(coords, distance){
    buffer <- st_buffer(coords, dist=distance)
    st_crs(buffer, epsg_rd)
    return (buffer)}
  
  check_shpExists <<- function (shape_path){
    first_part_path <-  substr(shape_path, 1, nchar(shape_path)-4) 
    if(file.exists(shape_path) == TRUE){
      print(first_part_path)
      file.remove(shape_path)
      file.remove(paste0(first_part_path, ".shx"))
      file.remove(paste0(first_part_path, ".prj"))
      file.remove(paste0(first_part_path, ".dbf"))
    }
  }
}

#AWS_test.df<<-fread("data/coordinates/AWS_coordinates.csv", data.table = FALSE)

#installl Perl
#installXLSXsupport()
# install.packages(c("horizon", "raster", "rgeos", "gdalUtils", "gdata", "sp", "sf", "lubridate", "insol", "ggplot2", "leaflet", "mapview", "devtools", "data.table", "dplyr", "htmlwidgets", "htmltools"), dependencies = TRUE )