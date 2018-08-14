library(raster)
library(rgeos)
library(rgdal)
library(gdalUtils)

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


#ogrinfo(bgt_wfs, so=TRUE)

#list of features
ogrListLayers(bgt_wfs)
bgtFeatureNamesList <- c("bgt:begroeidterreindeel", "bgt:functioneelgebied", "bgt:onbegroeidterreindeel", "bgt:ondersteunendwaterdeel", "bgt:waterdeel", "bgt:ondersteunendwegdeel", "bgt:wegdeel", "bgt:pand", "bgt:spoor")
featureNamesList <- c("begroeidterreindeel", "functioneelgebied", "onbegroeidterreindeel","ondersteunendwaterdeel", "waterdeel", "ondersteunendwegdeel", "wegdeel", "pand", "spoor")  

#define features to be loaded
bgtShpList <- list()

for (i in seq_along(featureNamesList)){
  feature_name = paste("feature",featureNamesList[i], sep="_")
  #assign(paste("Feature", bgtFeatureNamesList[i], sep = "_"), SpatialPolygonsDataFrame(SpatialPolygons(list()), data=data.frame()))
  feature = assign(feature_name, SpatialPolygonsDataFrame(SpatialPolygons(list()), data=data.frame()))
  #bgtShpList[feature_name]<- feature
}
for(j in seq_along(bgtFeatureNamesList)){

  #layer_name = paste("bgt",bgtFeatureNamesList[j],sep= ":")
  #print(layer_name)
  print(featureNamesList[j])
  print(bgtFeatureNamesList[j])
  
  shape_file  = paste(featureNamesList[j],".shp",sep="")
  ogr2ogr(bgt_wfs, shape_file, bgtFeatureNamesList[j])
  bgtShpList[j] <- readOGR(shape_file, featureNamesList[j], stringsAsFactors=FALSE)
}

#load pand layer and make shapefile
ogr2ogr(bgt_wfs, "bgt.shp", "bgt:functioneelgebied")
bgtTest <- readOGR("bgt.shp", "bgt", stringsAsFactors=FALSE)
plot(bgtTest)
