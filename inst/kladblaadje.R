
#load wfs
library(gdalUtils)
dsn <- "WFS:https://geodata.nationaalgeoregister.nl/beta/bgt/wfs?request=getcapabilities"

ogrinfo(dsn, so=TRUE)

ogr2ogr(dsn, "bgt.shp", "bgt:pand")

library(rgdal)
bgt <- readOGR("bgt.shp", "bgt", stringsAsFactors=FALSE)

plot(bgt, max.plot = 19)

library(rwfs)
fileName <- tempfile()
download.file("https://geodata.nationaalgeoregister.nl/beta/bgt/wfs?request=getcapabilities", fileName)
request <- rwfs::GMLFile$new(fileName)
client <- rwfs::WFSCachingClient$new(request)
layer <- client$getLayer("bgt:pand")
print(layer@data)
plot(layer, max.plot = 19)
unlink(fileName)
