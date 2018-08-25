library(raster)
library(rgeos)
library(rgdal)
library(gdalUtils)
library(gdata)

dataset <- "BGT"

#De Bilt
aws <- "deBilt"
debilt<-data.frame("lat"=140802.698, "lon"=456881.8)
coordinates(debilt)<-~lat+lon

#create 200 meter buffer arouond sensor
surroundingBuffer <- buffer(debilt,width=200) #since RDcoords are in meters width is also in m

#get BBOX extent of buffer area
surroundingExtent <- extent(surroundingBuffer)

#create WFS string
bgt_wfsBaseUrl <- "https://geodata.nationaalgeoregister.nl/beta/bgt/wfs?request=getcapabilities"
my_EPSG <- "EPSG:28992"
my_bbox <- paste(toString(surroundingExtent@xmin), toString(surroundingExtent@ymin), toString(surroundingExtent@xmax), toString(surroundingExtent@ymax), sep=",")
bgt_wfs <- paste("WFS:", bgt_wfsBaseUrl, "&SRSNAME=", my_EPSG, "&BBOX=", my_bbox, sep="")

xls_location <- c( "/nobackup/users/stuurman/wmoSC/data-raw/bgt_names_features.xlsx", "C:\\Users\\KNMI\\Dropbox\\KNMI\\R_SPQJ\\wmoSC\\data-raw\\bgt_names_features.xlsx")
#bgt_objects.xlsx <- read.xls(xls_location[storage_settings], sheet="bgt_objects")

bgt_objects.csv <- read.table(paste("wmoSC", "data-raw", "bgt_objects.csv", sep=folder_structure ), header = TRUE, sep=",")
bgt_objects_shortname_list <- as.vector(data.frame(lapply(bgt_objects.csv[1], as.character), stringsAsFactors=FALSE)[,1])
bgt_objects_name_list <- as.vector(data.frame(lapply(bgt_objects.csv[2], as.character), stringsAsFactors=FALSE)[,1])

#bgt_features_perObject.xlsx <- read.xls(xls_location[storage_settings], sheet="bgt_features_perObject")
bgt_features_perObject.csv <- read.table(paste("wmoSC", "data-raw", "bgt_features_perObject.csv", sep= folder_structure), header = TRUE, sep=",")


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

omit <- function(features){
  #features[!apply(data == "", 1, all),]
  featureset <- na.omit(features)
  print(featureset)
  #features[complete.cases(features),]
  #return(features)
}

lapply(bgt_features_list, omit)

na.omit(bgt_waterdeel_features)

all_bgt_objects_200m <- data.frame(aws = character(0), feature_type = character(0), has_features = logical(0), features_count = numeric(0), stringsAsFactors = FALSE) 

read_bgt<-function(aws, wfs, bgt_object_name, object_name_short){
  bgt_directory <- paste("data", "BGT", sep=folder_structure)
  dir.create(paste(bgt_directory, station, sep=folder_structure), showWarnings = FALSE)
  working_directory <- paste(bgt_directory, station, sep=folder_structure)
  dir.create(paste(working_directory, "raw", sep=folder_structure), showWarnings = FALSE)
  dir.create(paste(working_directory, "selections", sep=folder_structure), showWarnings = FALSE)
  
  object_name <- paste("object", object_name_short, sep="_")
  object <- assign(object_name, SpatialPolygonsDataFrame(SpatialPolygons(list()), data=data.frame()))
  rowname <- paste(aws,object_name_short,sep="_")
  print(rowname)
  
  
  raw_shape_file_directory <- paste(working_directory, "raw", "", sep=folder_structure)
  shape_file <- paste( raw_shape_file_directory, object_name_short, ".shp", sep="")
  print(paste("shapefile: ", shape_file, sep=""))
  
  object <- tryCatch({
    ogr2ogr(src_datasource_name = wfs    , dst_datasource_name = shape_file, layer = bgt_object_name, overwrite = TRUE)
    
    raw_data_location_1 <- paste(working_directory, "/raw/", sep="")
    raw_data_location_2 <- paste(working_directory, "\\raw", sep="")
    
    raw_data_locations <- c(raw_data_location_1, raw_data_location_2)
    raw_data_location <- raw_data_locations[storage_settings]
    print(paste("raw data location: ", raw_data_location, sep=""))
    
    object_name <- readOGR(dsn = raw_data_location, layer = object_name_short, stringsAsFactors=FALSE)
    object_name@data$object_type<- c(object_name_short)
    object_count <- length(object_name)
    entry_t <- data.frame(aws, object_name_short, TRUE, object_count, stringsAsFactors=FALSE)
    names(entry_t) <- c("aws", "object_type", "has_features", "feature_count")
    rownames(entry_t) <- rowname
    all_bgt_objects_200m <<- rbind(all_bgt_objects_200m, entry_t, stringsAsFactors=FALSE)
  },
  error=function(e){
    object <- NA
    #remove shp
    shp <- paste(raw_shape_file_directory, object_name_short, ".shp", sep="")
    dbf <- paste(raw_shape_file_directory, object_name_short, ".dbf", sep="")
    prj <- paste(raw_shape_file_directory, object_name_short, ".prj", sep="")
    shx <- paste(raw_shape_file_directory, object_name_short, ".shx", sep="")
    file.remove(shp, dbf, prj, shx)
    
    print(e)
    #enter that object has no features
    entry_f <- data.frame(aws, object_name_short, FALSE, 0, stringsAsFactors=FALSE)
    names(entry_f) <- c("aws", "object_type", "has_features", "feature_count")
    rownames(entry_f) <- rowname
    all_bgt_objects_200m <<- rbind(all_bgt_objects_200m, entry_f, stringsAsFactors=FALSE)
    message(paste("No features found for", object_name_short, "in", aws, sep=" "))
    object_name <- NA
  }
  )
  return (object_name)
}
#create list of shape files
bgt_list <- list()
bgt_list <- mapply(read_bgt,aws = aws, wfs = bgt_wfs, bgt_object_name = bgt_objects_name_list, object_name_short = bgt_objects_shortname_list)

names(bgt_list) <- bgt_objects_shortname_list
pand_shape <- bgt_list[["pand"]]