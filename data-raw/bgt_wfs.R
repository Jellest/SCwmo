##load BGT

#load wfs
library(gdalUtils)
bgt_wfs <- "WFS:https://geodata.nationaalgeoregister.nl/beta/bgt/wfs?request=getcapabilities"

#ogrinfo(bgt_wfs, so=TRUE)

#load pand layer and make shapefile
ogr2ogr(bgt_wfs, "bgt.shp", "bgt:pand")
library(rgdal)
on

#plot BGT
bgt <- readOGR("bgt.shp", "bgt", stringsAsFactors=FALSE)
plot(bgt, max.plot = 19)
