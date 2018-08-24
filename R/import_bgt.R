library(raster)
library(rgeos)
library(rgdal)
library(gdalUtils)
library(gdata)

dataset <- "BGT"

#De Bilt
station <- "deBilt"
debilt<-data.frame("lat"=140802.698, "lon"=456881.8)
coordinates(debilt)<-~lat+lon

storage_settings <- 2

#create 200 meter buffer arouond sensor
surroundingBuffer <- buffer(debilt,width=200) #since RDcoords are in meters width is also in m

#get BBOX extent of buffer area
surroundingExtent <- extent(surroundingBuffer)

#create WFS string
bgt_wfsBaseUrl <- "https://geodata.nationaalgeoregister.nl/beta/bgt/wfs?request=getcapabilities"
my_EPSG <- "EPSG:28992"
my_bbox <- paste(toString(surroundingExtent@xmin), toString(surroundingExtent@ymin), toString(surroundingExtent@xmax), toString(surroundingExtent@ymax), sep=",")
bgt_wfs <- paste("WFS:", bgt_wfsBaseUrl, "&SRSNAME=", my_EPSG, "&BBOX=", my_bbox, sep="")

#list of features
ogrListLayers(bgt_wfs)
  
bgt_objects.xlsx <- read.xls("C:\\Users\\KNMI\\Dropbox\\KNMI\\R_SPQJ\\wmoSC\\data-raw\\bgt_names_features.xlsx", sheet="bgt_objects")
bgt_objects_shortname_list <- data.frame(lapply(bgt_objects.xlsx[1], as.character), stringsAsFactors=FALSE)
bgt_objects_name_list <- data.frame(lapply(bgt_objects.xlsx[2], as.character), stringsAsFactors=FALSE)

bgt_features_perObject_list <- list()
bgt_features_perObject.xlsx <- read.xls("C:\\Users\\KNMI\\Dropbox\\KNMI\\R_SPQJ\\wmoSC\\data-raw\\bgt_names_features.xlsx", sheet="bgt_features_perObject")


bgt_begroeidterrein_features <- data.frame(lapply(bgt_features_perObject.xlsx[1], as.character), stringsAsFactors=FALSE)
bgt_onbegroeidterrein_features <- data.frame(lapply(bgt_features_perObject.xlsx[2], as.character), stringsAsFactors=FALSE)
bgt_waterdeel_features <- data.frame(lapply(bgt_features_perObject.xlsx[3], as.character), stringsAsFactors=FALSE)
bgt_wegdeel_features <- data.frame(lapply(bgt_features_perObject.xlsx[4], as.character), stringsAsFactors=FALSE)
bgt_scheiding_features <- data.frame(lapply(bgt_features_perObject.xlsx[5], as.character), stringsAsFactors=FALSE)
bgt_pand_features <- data.frame(lapply(bgt_features_perObject.xlsx[6], as.character), stringsAsFactors=FALSE)
bgt_overigbouwwerk_features <- data.frame(lapply(bgt_features_perObject.xlsx[7], as.character), stringsAsFactors=FALSE)
bgt_spoor_features <- data.frame(lapply(bgt_features_perObject.xlsx[8], as.character), stringsAsFactors=FALSE)
bgt_functioneelgebied_features <- data.frame(lapply(bgt_features_perObject.xlsx[9], as.character), stringsAsFactors=FALSE)

bgt_features_list <- list(bgt_begroeid_features, bgt_onbegroeidterrein_features, bgt_waterdeel_features, bgt_wegdeel_features, bgt_scheiding_features, bgt_pand_features, bgt_overigbouwwerk_features, bgt_spoor_features, bgt_functioneelgebied_features)

omit <- function(features){
  #features[!apply(data == "", 1, all),]
  featureset <- na.omit(features)
  print(featureset)
  #features[complete.cases(features),]
  #return(features)
}

lapply(bgt_features_list, omit)

na.omit(bgt_waterdeel_features)

all_bgt_objects_200m <- data.frame(station = character(0), feature_type = character(0), has_features = logical(0), features_count = numeric(0), stringsAsFactors = FALSE) 


read_bgt<-function(wfs, bgt_object_name, object_name_short, storage_location){
  object_name <- paste("object", object_name_short, sep="_")
  object <- assign(object_name, SpatialPolygonsDataFrame(SpatialPolygons(list()), data=data.frame()))
  rowname <- paste(station,object_name_short,sep="_")
  print(rowname)
  shape_file_1 <- paste(storage_location, "/raw/", object_name_short, ".shp", sep="")
  shape_file_2 <- paste(storage_location, "\\raw\\", object_name_short, ".shp", sep="")
  shape_files <- c(shape_file_1, shape_file_2)
  shape_file <- shape_files[2]
  
  print(shape_file_2)
  
  object <- tryCatch({
    ogr2ogr(src_datasource_name = wfs    , dst_datasource_name = shape_file_2, layer = bgt_object_name, overwrite = TRUE)
    raw_data_location <- paste(storage_location, "\\raw", sep="")
    object_name <- readOGR(dsn = raw_data_location, layer = object_name_short, stringsAsFactors=FALSE)
    object_name@data$object_type<- c(object_name_short)
    object_count <- length(object_name)
    entry_t <- data.frame(station, object_name_short, TRUE, object_count, stringsAsFactors=FALSE)
    names(entry_t) <- c("station", "object_type", "has_features", "feature_count")
    rownames(entry_t) <- rowname
    all_bgt_objects_200m <<- rbind(all_bgt_objects_200m, entry_t, stringsAsFactors=FALSE)
    },
  error=function(e){
    print(e)
    entry_f <- data.frame(station, object_name_short, FALSE, 0, stringsAsFactors=FALSE)
    names(entry_f) <- c("station", "object_type", "has_features", "feature_count")
    rownames(entry_f) <- rowname
    all_bgt_objects_200m <<- rbind(all_bgt_objects_200m, entry_f, stringsAsFactors=FALSE)
    message(paste("No features found for", object_name_short, "in", station, sep=" "))
    object_name <- NA
    }
  )
  return (object_name)
}

#bgt_list_station <- paste("bgt_feature_list_", station, sep="") 
#bgt_list <- assign(bgt_list_name, list())

bgt_list <- list()
bgt_list <- mapply(read_bgt,wfs = bgt_wfs, bgt_object_name = bgt_objects_name_list, object_name_short = bgt_objects_shortname_list, storage_location = data_location)

names(bgt_list) <- bgt_objects_shortname_list

shape_location_test <- paste(data_location, "\\raw\\", "pand", ".shp", sep="") 
new_name <- paste(data_location, "\\selections\\", "pand_select", ".shp", sep="")
pand_shape <- bgt_list[["pand"]] 

# ogr2ogr(src_datasource_name = shape_location,
#         dst_datasource_name = new_name,
#         where = gml_id = "pand.10632890",
#         overwrite = TRUE)

ogr2ogr(shape_location_test,
        new_name,
        layer = "pand",
        where = "gml_id = 'pand.10632890'")

ogrinfo(shape_location_test,
        layer = "pand",
        where = "gml_id = 'pand.10632890'")





##single features test script

# single_bgt_features <- data.frame(station = character(), feature_type = character(), has_features = logical(), features_count = numeric(), stringsAsFactors = FALSE)
# my_rowname <- paste(station, "pand", sep="_")
# 
# data_location <- data_locations[storage_settings + 1]
# ogrListLayers(bgt_wfs)
bgt_single <- read_bgt(wfs = bgt_wfs, bgt_object_name = "bgt:pand", object_name_short = "pand", storage_location = data_location)
# single_count <- length(bgt_single)
# entry_test <- data.frame(station, "pand", TRUE, single_count, stringsAsFactors=FALSE)
# names(entry_test) <- c("station", "feature_type", "has_features", "features_count")
# rownames(entry_test) <- my_rowname
# single_bgt_features <- rbind(single_bgt_features, entry_test)