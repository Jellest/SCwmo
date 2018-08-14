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

data_location <- paste("/nobackup/users/stuurman/data/", dataset, station, sep="")

#get BBOX extent of buffer area
surroundingExtent <- extent(surroundingBuffer)

#create WFS string
bgt_wfsBaseUrl <- "https://geodata.nationaalgeoregister.nl/beta/bgt/wfs?request=getcapabilities"
my_EPSG <- "EPSG:28992"
my_bbox <- paste(toString(surroundingExtent@xmin), toString(surroundingExtent@ymin), toString(surroundingExtent@xmax), toString(surroundingExtent@ymax), sep=",")
bgt_wfs <- paste("WFS:", bgt_wfsBaseUrl, "&SRSNAME=", my_EPSG, "&BBOX=", my_bbox, sep="")

#ogrinfo(bgt_wfs, so=TRUE)

#list of features
ogrListLayers(bgt_wfs)
bgtFeatureNamesList <- c("bgt:begroeidterreindeel", "bgt:functioneelgebied", "bgt:onbegroeidterreindeel", "bgt:ondersteunendwaterdeel", "bgt:waterdeel", "bgt:ondersteunendwegdeel", "bgt:wegdeel", "bgt:pand", "bgt:spoor")
featureNamesList <- c("begroeidterreindeel", "functioneelgebied", "onbegroeidterreindeel","ondersteunendwaterdeel", "waterdeel", "ondersteunendwegdeel", "wegdeel", "pand", "spoor")  

read_bgt<-function(bgt_feature,feature_name_short, storage_location){
  feature_name = paste("feature",bgt_feature, sep="_")
  feature = assign(feature_name, SpatialPolygonsDataFrame(SpatialPolygons(list()), data=data.frame()))

  print(feature_name)
  shape_file  = paste(storage_location, feature_name_short,".shp",sep="")

  ogr2ogr(src_datasource_name = bgt_wfs, dst_datasource_name = shape_file, layer = feature_name_short)
  
  bgtShpList <- tryCatch(readOGR(dsn = shape_file, layer = feature_name_short, storage_location, stringsAsFactors=FALSE),error=function(e) e)
  
  return(bgtShpList)
}

bgt_list<-mapply(read_bgt,bgt_feature = bgtFeatureNamesList, feature_name_short = featureNamesList, storage_location = data_location)
