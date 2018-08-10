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
featurNamesList <- c("begroeid", "functioneel", "onbegroeid","ondersteunendWater", "water", "ondersteunendWeg", "weg", "pand", "spoor")  

#define features to be loaded

for(j in seq_along(featurNamesList)){
assign(paste("Feature_", featurNamesList[j], sep = ""), j)
}

for(i in seq_along(bgtFeaturNamesList)){
  featureName <- bgtFeatureNamesList[i]
  #create shapefile from features
  ogr2ogr(bgt_wfs, "bgt.shp", featureName)
}



colNames(datos) <- paste("X", i, sep="") then? Then you can access them with datos$X1, datos$X2

