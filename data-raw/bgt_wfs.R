library(raster)
library(rgeos)
library(rgdal)
library(gdalUtils)

dataset <- "BGT"

#De Bilt
station <- "DeBilt"
debilt<-data.frame("lat"=140802.698, "lon"=456881.8)
coordinates(debilt)<-~lat+lon

#create 200 meter buffer arouond sensor
surroundingBuffer <- buffer(debilt,width=200) #since RDcoords are in meters width is also in m

data_location <- paste("/nobackup/users/stuurman/data/", dataset, "/", station, "/", sep="")

#get BBOX extent of buffer area
surroundingExtent <- extent(surroundingBuffer)

#create WFS string
bgt_wfsBaseUrl <- "https://geodata.nationaalgeoregister.nl/beta/bgt/wfs?request=getcapabilities"
my_EPSG <- "EPSG:28992"
my_bbox <- paste(toString(surroundingExtent@xmin), toString(surroundingExtent@ymin), toString(surroundingExtent@xmax), toString(surroundingExtent@ymax), sep=",")
bgt_wfs <- paste("WFS:", bgt_wfsBaseUrl, "&SRSNAME=", my_EPSG, "&BBOX=", my_bbox, sep="")

#list of features
ogrListLayers(bgt_wfs)
bgtFeatureNamesList <- c("bgt:begroeidterreindeel", "bgt:functioneelgebied", "bgt:onbegroeidterreindeel", "bgt:ondersteunendwaterdeel", "bgt:waterdeel", "bgt:ondersteunendwegdeel", "bgt:wegdeel", "bgt:pand", "bgt:spoor", "bgt:mast")
featureNamesList <- c("begroeidterreindeel", "functioneelgebied", "onbegroeidterreindeel","ondersteunendwaterdeel", "waterdeel", "ondersteunendwegdeel", "wegdeel", "pand", "spoor", "mast")
all_bgt_features <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("station", "feature_type", "has_features", "feature_count"))
                         

read_bgt<-function(all_features, wfs, bgt_feature, feature_name_short, storage_location){
  feature_name <- paste("feature",feature_name_short, sep="_")
  feature <- assign(feature_name, SpatialPolygonsDataFrame(SpatialPolygons(list()), data=data.frame()))
  rowname <- paste(station,feature_name_short,sep="_")
  print(feature_name)
  shape_file  <- paste(storage_location, feature_name_short,".shp",sep="")
  print(shape_file)
  feature <- tryCatch({
    ogr2ogr(src_datasource_name = wfs    , dst_datasource_name = shape_file, layer = bgt_feature, overwrite = TRUE)
    feature_name <- readOGR(dsn = storage_location, layer = feature_name_short, stringsAsFactors=FALSE)
    
    feature_count <- length(feature_name@data)

    entry_t <- data.frame(station, feature_name_short, TRUE, feature_count, stringsAsFactors=FALSE)
    names(entry_t) <- c("station", "feature_type", "has_features", "features_count")
    rownames(entry_t) <- rowname
    all_features <- rbind(all_features, entry_t, stringsAsFactors=FALSE)
    
    #return(feature)
    },
  error=function(e){
    e
    entry_f <- data.frame(station, feature_name_short, FALSE, 0, stringsAsFactors=FALSE)
    names(entry_f) <- c("station", "feature_type", "has_features", "features_count")
    rownames(entry_f) <- rowname
    all_features <- rbind(all_features, entry_f, stringsAsFactors=FALSE)
    message(paste("No features found for", feature_name_short, "in", station, sep=" "))
  }
  )
  return (feature_name)
}


bgt_list<- mapply(read_bgt, all_bgt_features, wfs = bgt_wfs, bgt_feature = bgtFeatureNamesList, feature_name_short = featureNamesList, storage_location = data_location)


##single features test script

# single_bgt_features <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("station", "feature_type", "has_features", "features_count"))
# my_rowname <- paste(station, "pand", sep="_")
# 
# bgt_single <- read_bgt(single_bgt_features, wfs = bgt_wfs, bgt_feature = "bgt:pand", feature_name_short = "pand", storage_location = data_location)
# 
# single_count <- length(bgt_single@data)
# entry_test <- data.frame(station, "pand", TRUE, single_count, stringsAsFactors=FALSE)
# names(entry_test) <- c("station", "feature_type", "has_features", "features_count")
# rownames(entry_test) <- my_rowname
# single_bgt_features <- rbind(single_bgt_features, entry_test)

