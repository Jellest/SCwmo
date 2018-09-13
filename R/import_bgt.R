library(raster)
library(rgeos)
library(rgdal)
library(gdalUtils)
library(gdata)
library(sf)

#aws station
station <- "De Bilt"
getAWS_name <- function (aws){
  aws_name_untrimmed <- AWS.df$AWS[which(AWS.df$AWS == aws)][1]
  if(is.na(aws_name_untrimmed) == TRUE) {
    aws_name <- stop(paste("No aws station found with the following name:", aws))
  } else{
    aws_name <- gsub(" ", "", aws_name_untrimmed)
  }
return (aws_name)}

rd_new_CRS <- CRS("+init=epsg:28992")
wgs_CRS <- CRS("+init=epsg:4326")
aws_name <- getAWS_name("De Bilt")

aws_debilt_wgs.sp <- data.frame("lat"= subset(AWS.df, AWS == station & Sensor == "site")$LAT, "lon"=subset(AWS.df, AWS == station & Sensor == "site")$LON) 
coordinates(aws_debilt_wgs.sp)<-~lon+lat
crs(aws_debilt_wgs.sp)<- CRS("+init=epsg:4326")

aws_debilt_rd.sp <- spTransform(aws_debilt_wgs.sp, CRS= CRS("+init=epsg:28992"))

#create 200 meter buffer arouond sensor
surroundingBuffer <- buffer(aws_debilt_rd.sp,width=200)

#get BBOX extent of buffer area
surroundingExtent <- extent(surroundingBuffer)

#create WFS string
createWFS_string <- function(WFSbase_url, epsg, bbox){
  bgt_wfs <- paste("WFS:", WFSbase_url, "&SRSNAME=", epsg, "&BBOX=", bbox, sep="")
return (bgt_wfs)}

bgt_wfs <- createWFS_string(WFSbase_url = "https://geodata.nationaalgeoregister.nl/beta/bgt/wfs?request=getcapabilities" 
                , epsg = "EPSG:28992"
                , bbox = paste(toString(surroundingExtent@xmin), toString(surroundingExtent@ymin), toString(surroundingExtent@xmax), toString(surroundingExtent@ymax), sep=",")
                )
#xls_location <- paste(home_directory, wmoSC,data-raw, " bgt_names_features.xlsx", sep= folder_structure)
#bgt_objects.xlsx <- read.xls(xls_location[setup_settings], sheet="bgt_objects")

bgt_objects.csv <- read.table(paste("wmoSC", "data-raw", "bgt_objects.csv", sep=folder_structure), header = TRUE, sep=",",blank.lines.skip = TRUE)
bgt_objects_shortname_list <- as.vector(data.frame(lapply(bgt_objects.csv[1], as.character), stringsAsFactors=FALSE)[,1])
bgt_objects_name_list <- as.vector(data.frame(lapply(bgt_objects.csv[2], as.character), stringsAsFactors=FALSE)[,1])

#bgt_features_perObject.xlsx <- read.xls(xls_location, sheet="bgt_features_perObject")
bgt_features_perObject.csv <- read.table(paste("wmoSC", "data-raw", "bgt_features_perObject.csv", sep= folder_structure), header = TRUE, sep=",", blank.lines.skip = TRUE)

bgt_begroeidterrein_features <- data.frame(lapply(bgt_features_perObject.csv[1], as.character), stringsAsFactors=FALSE)
bgt_onbegroeidterrein_features <- data.frame(lapply(bgt_features_perObject.csv[2], as.character), stringsAsFactors=FALSE)
bgt_waterdeel_features <- data.frame(lapply(bgt_features_perObject.csv[3], as.character), stringsAsFactors=FALSE)
bgt_wegdeel_features <- data.frame(lapply(bgt_features_perObject.csv[4], as.character), stringsAsFactors=FALSE)
bgt_scheiding_features <- data.frame(lapply(bgt_features_perObject.csv[5], as.character), stringsAsFactors=FALSE)
bgt_pand_features <- data.frame(lapply(bgt_features_perObject.csv[6], as.character), stringsAsFactors=FALSE)
bgt_overigbouwwerk_features <- data.frame(lapply(bgt_features_perObject.csv[7], as.character), stringsAsFactors=FALSE)
bgt_spoor_features <- data.frame(lapply(bgt_features_perObject.csv[8], as.character), stringsAsFactors=FALSE)

bgt_functioneelgebied_features <- data.frame(lapply(bgt_features_perObject.csv[9], as.character), stringsAsFactors=FALSE)

bgt_features_list <- list(bgt_begroeidterrein_features, bgt_onbegroeidterrein_features, bgt_waterdeel_features, bgt_wegdeel_features, bgt_scheiding_features, bgt_pand_features, bgt_overigbouwwerk_features, bgt_spoor_features, bgt_functioneelgebied_features)

# omit <- function(features){
#   #features[!apply(data == "", 1, all),]
#   featureset <- na.omit(features)
#   print(featureset)
#   #features[complete.cases(features),]
#   #return(features)
# }
# 
# lapply(bgt_features_list, omit)
# 
# na.omit(bgt_waterdeel_features)
bgt_begroeidterrein_features[complete.cases(bgt_begroeidterrein_features),]

all_bgt_objects_200m <- data.frame(aws = character(0), feature_type = character(0), has_features = logical(0), features_count = numeric(0), stringsAsFactors = FALSE) 

bgt_colNames <- c("bgt-functi", "bgt-fysiek", "bgt-type", "bronhouder", "gml_id", "naam", "object_type", "plus-fun_1", "plus-fun_2", "plus-funct", "plus-fys_2", "plus-fysie", "plus-sta_1", "plus-sta_2", "plus-statu", "plus-typ_1", "plus-type", "relatieveh", "status")
bgt.df <- setNames(data.frame(matrix(ncol = 19, nrow = 0), stringsAsFactors = FALSE), bgt_colNames)
bgt.df <- data.frame(lapply(bgt.df, function(x) if(is.logical(x)) {return(as.character(x))} else { return(x)}), stringsAsFactors=FALSE)

#bgt_shape <- assign(paste("BGT", station, sep="_"), st_sfc(st_polygon(list())))

#crs(BGT_deBilt) <- crs("+init=epsg:28992")


#adjust columns
adjustColumns <- function(raw_data_location, object_name_short){
  print("adding columns...")
  shp <- readOGR(dsn = raw_data_location, layer = object_name_short, stringsAsFactors=FALSE)
  print(paste("before", length(names(shp))))
  ## add and remove the different columns to proceed later with correct rbind
  print(paste("in: ", object_name_short))
  if(object_name_short == "begroeidterreindeel"){
    shp$plus.funct <- NA
    shp$plus.fun_1 <- NA
    shp$plus.fun_2 <- NA
    shp$bgt.functi <- NA
    shp$plus.typ_1 <- NA
    shp$plus.type <- NA
    shp$bgt.type <- NA
    shp$naam <- NA
    shp$object_typ <- "begroeidterreindeel" 
    
    drops<- c('status_lee', 'status_cod', 'inonderzoe', 'eindregist', 'terminatio', 'lokaalid', 'inonderz_1', 'optalud_le', 'fysiekvoor', 'creationda', 'terminat_1', 'plus.fys_1', 'optalud', 'lv.publica', 'tijdstipre', 'kruinlijn_')
    shp <- shp[,!(names(shp) %in% drops)]
  
    } else if(object_name_short == "onbegroeidterreindeel"){
    shp$plus.funct <- NA
    shp$plus.fun_1 <- NA
    shp$plus.fun_2 <- NA
    shp$bgt.functi <- NA
    shp$plus.typ_1 <- NA
    shp$plus.type <- NA
    shp$bgt.type <- NA
    shp$naam <- NA
    shp$object_typ <- "onbegroeidterreindeel" 
    
    drops<- c('status_lee', 'status_cod', 'inonderzoe', 'eindregist', 'terminatio', 'lokaalid', 'inonderz_1', 'optalud_le', 'fysiekvoor', 'creationda', 'terminat_1', 'plus.fys_1', 'optalud', 'lv.publica', 'tijdstipre', 'kruinlijn_')
    shp <- shp[,!(names(shp) %in% drops)]
    
  } else if(object_name_short == "waterdeel"){
    shp$plus.fysie <- NA
    shp$plus.funct <- NA
    shp$plus.fun_1 <- NA
    shp$plus.fun_2 <- NA
    shp$bgt.functi <- NA
    shp$bgt.fysiek <- NA
    shp$plus.fys_2 <- NA
    shp$naam <- NA
    shp$object_typ <- "waterdeel" 
    
    drops<- c('type_codes', 'status_lee', 'status_cod', 'inonderzoe', 'eindregist', 'terminatio', 'lokaalid', 'inonderz_1', 'creationda', 'terminat_1', 'lv.publica', 'tijdstipre', 'plus.type_')
    shp <- shp[,!(names(shp) %in% drops)]
    
  } else if(object_name_short == "wegdeel"){
    shp$plus.typ_1 <- NA
    shp$plus.type <- NA
    shp$bgt.type <- NA
    shp$naam <- NA
    shp$object_typ <- "wegdeel" 
    
    drops<- c('status_lee', 'status_cod', 'inonderzoe', 'eindregist', 'terminatio', 'lokaalid', 'inonderz_1', 'plus.fys_1', 'lv.publica', 'kruinlijn_', 'tijdstipre', 'optalud', 'creationda', 'optalud_le', 'fysiekvoor', 'terminat_1', 'functie_co')
    shp <- shp[,!(names(shp) %in% drops)]
    
  } else if(object_name_short == "scheiding"){
    shp$plus.fysie <- NA
    shp$plus.funct <- NA
    shp$plus.fun_1 <- NA
    shp$plus.fun_2 <- NA
    shp$bgt.functi <- NA
    shp$bgt.fysiek <- NA
    shp$plus.fys_2 <- NA
    shp$naam <- NA
    shp$object_typ <- "scheiding" 
    
    drops<- c('type_codes', 'status_lee', 'status_cod', 'plus.type_', 'inonderzoe', 'eindregist', 'terminatio', 'lokaalid', 'inonderz_1', 'creationda', 'terminat_1', 'lv.publica', 'tijdstipre')
    shp <- shp[,!(names(shp) %in% drops)]
    
  } else if(object_name_short == "pand"){
    shp$plus.fysie <- NA
    shp$plus.funct <- NA
    shp$plus.fun_1 <- NA
    shp$plus.fun_2 <- NA
    shp$bgt.functi <- NA
    shp$bgt.fysiek <- NA
    shp$plus.fys_2 <- NA
    shp$plus.typ_1 <- NA
    shp$plus.type <- NA
    shp$bgt.type <- NA
    shp$naam <- NA
    shp$object_typ <- "pand" 
    
    drops<- c('identifica', 'status_lee', 'status_cod', 'inonderzoe', 'eindregist', 'terminatio', 'lokaalid', 'inonderz_1', 'creationda', 'terminat_1', 'lv.publica', 'tijdstipre')
    shp <- shp[,!(names(shp) %in% drops)]
    
  } else if(object_name_short == "overigbouwwerk"){
    shp$plus.fysie <- NA
    shp$plus.funct <- NA
    shp$plus.fun_1 <- NA
    shp$plus.fun_2 <- NA
    shp$bgt.functi <- NA
    shp$bgt.fysiek <- NA
    shp$plus.fys_2 <- NA
    shp$plus.typ_1 <- NA
    shp$plus.type <- NA
    shp$bgt.type <- NA
    shp$naam <- NA
    shp$object_typ <- "overigbouwwerk" 
    
    drops<- c('type_codes', 'plus.type_', 'identifica', 'status_lee', 'status_cod', 'inonderzoe', 'eindregist', 'terminatio', 'lokaalid', 'inonderz_1', 'creationda', 'terminat_1', 'lv.publica', 'tijdstipre')
    shp <- shp[,!(names(shp) %in% drops)]
    
  } else if(object_name_short == "spoor"){
    shp$plus.fysie <- NA
    shp$bgt.fysiek <- NA
    shp$plus.fys_2 <- NA
    shp$plus.typ_1 <- NA
    shp$plus.type <- NA
    shp$bgt.type <- NA
    shp$naam <- NA
    shp$object_typ <- "spoor" 
    
    drops<- c('status_lee', 'status_cod', 'inonderzoe', 'eindregist', 'terminatio', 'lokaalid', 'inonderz_1', 'functie_co', 'creationda', 'terminat_1', 'lv.publica', 'tijdstipre')
    shp <- shp[,!(names(shp) %in% drops)]
    
  } else if(object_name_short == "functioneelgebied"){
    shp$plus.fysie <- NA
    shp$plus.funct <- NA
    shp$plus.fun_1 <- NA
    shp$plus.fun_2 <- NA
    shp$bgt.functi <- NA
    shp$bgt.fysiek <- NA
    shp$plus.fys_2 <- NA
    shp$plus.typ_1 <- NA
    shp$plus.type <- NA
    shp$object_typ <- "functioneelgebied" 
    
    drops<- c('type_codes', 'status_lee', 'status_cod', 'plus.type_', 'naam', 'inonderzoe', 'eindregist', 'terminatio', 'lokaalid', 'inonderz_1', 'creationda', 'terminat_1', 'naam_leeg', 'lv.publica', 'plus.typ_1', 'plus.type', 'tijdstipre')
    shp <- shp[,!(names(shp) %in% drops)]
    
  }
  shp[ , order(names(shp))]
  print(paste("after", length(names(shp))))
  print(names(shp))
  writeOGR(obj = shp, dsn = raw_data_location, layer = object_name_short, driver = "ESRI Shapefile", overwrite_layer = TRUE)
  return (shp)
}

bgt_counter <- 0
read_bgt<-function(aws_name, wfs, bgt_object_name, object_name_short){
  bgt_directory <- paste("data", "BGT", sep=folder_structure)
  dir.create(paste(bgt_directory, aws_name, sep=folder_structure), showWarnings = FALSE)
  working_directory <- paste(bgt_directory, aws_name, sep=folder_structure)
  dir.create(paste(working_directory, "raw", sep=folder_structure), showWarnings = FALSE)
  dir.create(paste(working_directory, "selections", sep=folder_structure), showWarnings = FALSE)
  
  object_name <- paste("object", object_name_short, sep="_")
  object <- assign(object_name, SpatialPolygonsDataFrame(SpatialPolygons(list()), data=data.frame()))
  
  rowname <- paste(aws_name,object_name_short,sep="_")
  print(" ")
  print(rowname)
  
  
  raw_data_location <- paste(working_directory, "raw", sep=folder_structure)
  print(paste("raw data location: ", raw_data_location, sep=""))
  raw_shape_file_path <- paste(working_directory, "raw", "", sep=folder_structure)
  shape_file <- paste( raw_shape_file_path, object_name_short, ".shp", sep="")
  print(paste("shapefile: ", shape_file, sep=""))
  
  object <- tryCatch({
    ogr2ogr(src_datasource_name = wfs    , dst_datasource_name = shape_file, layer = bgt_object_name, overwrite = TRUE)
    
    object_name <- readOGR(dsn = raw_data_location, layer = object_name_short, stringsAsFactors=FALSE)
    #object_name@data$object_type<- c(object_name_short)
    crs(object_name) <- crs("+init=epsg:28992")

    #adjust columns
    object_name <- adjustColumns(raw_data_location, object_name_short)
    print(paste("field count object name: ", length(names(object_name@data))))

    crs(object_name) <- crs("+init=epsg:28992")
    
    bgt_counter <<- bgt_counter + 1
    
    if(bgt_counter == 1){
      #print("in 1")
      #print(length(object_name))
      BGT_station.sp <<- object_name
      #print(length(BGT_shape))
    } else {
      #print("in >1")
      print(paste("length new object:",length(object_name), sep=" "))
      print(paste("length BGT:",length(BGT_station.sp), sep=" "))
      
      BGT_station.sp <<- rbind(BGT_station.sp, object_name, makeUniqueIDs = TRUE)
      print(paste("new length BGT:",length(BGT_station.sp), sep=" "))
    }
    object_count <- length(object_name)
    entry_t <- data.frame(aws_name, object_name_short, TRUE, object_count, stringsAsFactors=FALSE)
    names(entry_t) <- c("aws", "object_type", "has_features", "feature_count")
    rownames(entry_t) <- rowname
    all_bgt_objects_200m <<- rbind(all_bgt_objects_200m, entry_t, stringsAsFactors=FALSE)
    
  }, error=function(e){
    bgt_counter <<- bgt_counter + 1
    object <- NA
    #remove empty shp
    shp <- paste(raw_shape_file_path, object_name_short, ".shp", sep="")
    dbf <- paste(raw_shape_file_path, object_name_short, ".dbf", sep="")
    prj <- paste(raw_shape_file_path, object_name_short, ".prj", sep="")
    shx <- paste(raw_shape_file_path, object_name_short, ".shx", sep="")
    #file.remove(shp, dbf, prj, shx)
    
    print(e)
    #enter that object has no features
    entry_f <- data.frame(aws_name, object_name_short, FALSE, 0, stringsAsFactors=FALSE)
    names(entry_f) <- c("aws", "object_type", "has_features", "feature_count")
    rownames(entry_f) <- rowname
    all_bgt_objects_200m <<- rbind(all_bgt_objects_200m, entry_f, stringsAsFactors=FALSE)
    message(paste("No features found for", object_name_short, "in", aws_name, sep=" "))
    object_name <- NA
  }
  )
  
  if(bgt_counter == length(bgt_objects_name_list)){
    shp_name <- paste("BGT", station, sep="_")
    BGT_station.sp$AREA <-sapply(slot(BGT_station.sp, 'polygons'), function(i) slot(i, 'area')) 
    writeOGR(obj = BGT_station.sp, dsn = working_directory, layer = shp_name, driver = "ESRI Shapefile", overwrite_layer = TRUE)
    BGT_station.sf <<- st_as_sf(BGT_station.sp)
  }
}

#read BGt and create one BGT shp for the aws.
bgt_counter <- 0
mapply(read_bgt,aws_name = aws_name, wfs = bgt_wfs, bgt_object_name = bgt_objects_name_list, object_name_short = bgt_objects_shortname_list)
 

#assign(paste("BGT", aws_name, sep="_"), SpatialPolygonsDataFrame(SpatialPolygons(list(BGT_station.sp)), data=BGT_station.sp), envir = .GlobalEnv)
bgt_counter <- 0


bgt_shp_files <- list.files(paste("data", "BGT", aws_name, "raw", sep=folder_structure), pattern = ".shp")

names(bgt_list) <- bgt_objects_shortname_list
pand_shape <- bgt_list[pand]
crs(pand_shape)<- CRS("+init=epsg:28992")
crs(pand_shape)
crs(bgt_shape)

desk <- "C:/users/Jelle/Desktop"
mshp<- "C:/users/Jelle/Desktop/test.shp"
ogr2ogr(src_datasource_name = desk    , dst_datasource_name = mshp, layer = 'pand', overwrite = TRUE)

