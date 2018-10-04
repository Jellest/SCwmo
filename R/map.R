library(leaflet)
library(mapview)

lyrs <- c('brtachtergrondkaart', 'brtachtergrondkaartgrijs', '2017_ortho25') 
lyr <- lyrs[3]
basemaps <- c(paste("http://geodata.nationaalgeoregister.nl/wmts/?SERVICE=WMTS&REQUEST=GetTile&VERSION=1.0.0&LAYER=", lyrs[1], "&TILEMATRIXSET=EPSG:3857&TILEMATRIX=EPSG:3857:{z}&TILEROW={y}&TILECOL={x}&FORMAT=image/png", sep=""), paste("http://geodata.nationaalgeoregister.nl/wmts/?SERVICE=WMTS&REQUEST=GetTile&VERSION=1.0.0&LAYER=", lyrs[2], "&TILEMATRIXSET=EPSG:3857&TILEMATRIX=EPSG:3857:{z}&TILEROW={y}&TILECOL={x}&FORMAT=image/png", sep=""), paste("http://geodata.nationaalgeoregister.nl/wmts/?SERVICE=WMTS&REQUEST=GetTile&VERSION=1.0.0&LAYER=", lyrs[3], "&TILEMATRIXSET=EPSG:3857&TILEMATRIX=EPSG:3857:{z}&TILEROW={y}&TILECOL={x}&FORMAT=image/png", sep=""), "https://geodata.nationaalgeoregister.nl/tiles/service/wmts?request=GetCapabilities&service=WMTS&REQUEST=GetTile&VERSION=1.0.0&LAYER=ahn2_05m_ruw&TILEMATRIXSET=EPSG:28992&TILEMATRIX=EPSG:28992:{z}&TILEROW={y}&TILECOL={x}&FORMAT=image/png")
map <- leaflet() %>%
  setView(5.177565, 52.099006, zoom = 15) %>%
  addTiles(basemaps[1], options = leafletOptions(minZoom = 7, maxZoom = 19),group = "BRT") %>%
  addTiles(basemaps[2], options = leafletOptions(minZoom = 7, maxZoom = 19),group = "BRT Gray") %>%
  addTiles(basemaps[3], options = leafletOptions(minZoom = 7, maxZoom = 19), group = "Satellite") %>%
  # addTiles(basemap_rd, group = "AHN2") %>%
  # addWMSTiles(
  #   "https://geodata.nationaalgeoregister.nl/ahn2/wms?"
  #   , layers = "ahn2_05m_ruw"
  #   , options = WMSTileOptions(format = "image/png", transparent = TRUE, crs=leafletCRS("L.CRS.EPSG3857"))
  #   , group = "AHN2") %>%
  addMouseCoordinates  %>%
  addLayersControl(
    baseGroups = c("BRT", "BRT Gray", "Satellite"),
    #overlayGroups = c("buildings", "water", "roads"),
    options = layersControlOptions(collapsed = TRUE)) %>%
  addMeasure(
    position = "bottomleft",
    primaryLengthUnit = "meters",
    primaryAreaUnit = "sqmeters",
    activeColor = "#3D535D",
    completedColor = "#7D4479")


# map %>% addFeatures(buildings_wgs.sf, weight = 0.7, color="black", fillColor = "red", opacity = 1, fillOpacity = 0.6) %>%
#   addFeatures(roads_wgs.sf, weight = 0.7, color="black", fillColor = "grey", opacity = 1, fillOpacity = 0.6) %>%
#   addFeatures(water_wgs.sf, weight = 0.7, color="black", fillColor = "blue", opacity = 1, fillOpacity = 0.6)
map %>% addRasterImage(ahn_mask, opacity = 1)

ahn2_raw_wgs <- st_transform(ahn2_deBilt_raw, "+init=epsg:4326")

AWS<-data.frame(deBilt.df)
coordinates(AWS) <- ~LON+LAT
crs(AWS)<-CRS("+init=epsg:4326")


buildings_wgs.sf <- st_transform(buildings.sf, "+init=epsg:4326")
roads_wgs.sf <- st_transform(raods.sf, "+init=epsg:4326")
water_wgs.sf <- st_transform(water.sf, "+init=epsg:4326")

map %>%
  addFeatures(buildings_wgs.sf, color="red", weight=0.5, fillColor="red") %>%
  addFeatures(roads_wgs.sf, color="grey", weight=0.5, fillColor="grey") %>%
  addFeatures(water_wgs.sf, color="blue", weight=0.5, fillColor="blue") %>%
  addFeatures(deBilt_wgs.sp, color="green", weight=0.5, fillColor="green") %>%
  setView(5.17939, 52.09886, zoom = 17)

ahn_map <- shadow_angles_DeBilt2[[1]]$height
shadow_map <- shadow_angles_DeBilt2[[1]]$elevation_angle 

ahnraster_map <- mapview(ahn_map, layer.name="height (m)", maxpixels =  1440000, map.types = c("Esri.WorldImagery")) + mapview(deBilt_wgs.sf, col.region ="yellow", layer.name="Temperature sensor", zcol=c("AWS"))
ahnraster_map@map %>% setView(5.17939, 52.09886, zoom = 16)
mapshot(ahnraster_map, file = "ahn2.png", url = NULL, remove_url = TRUE, remove_controls = c("zoomControl", "layersControl"))

elevraster_map <- mapview(shadow_map, layer.name="elevation angle", maxpixels =  1440000, map.types = c("Esri.WorldImagery")) + mapview(deBilt_wgs.sf, col.region ="yellow", layer.name="Temperature sensor", zcol=c("AWS"))
elevraster_map@map %>% setView(5.17939, 52.09886, zoom = 16)
mapshot(elevraster_map, file = "shadows.png", url = NULL, remove_url = TRUE, remove_controls = c("zoomControl", "layersControl"))


bgt_map <- mapview(buildings, col.region="#f0495f", lwd=0.5, legend = TRUE, zcol=c("object")) + mapview(roads, col.region="#444444", lwd=0.5, legend = TRUE) + mapview(water, col.region="#25e0ed", lwd=0.5, legend = TRUE, zcol=c("object")) + mapview(vegetation, col.region="#5dcb1e", lwd=0.5, legend = TRUE,zcol=c("object")) + mapview(barren, col.region="#f1f4c7", lwd=0.5, legend = TRUE, zcol=c("object")) + mapview(deBilt_aws, col.region ="black", legend = TRUE, zcol=c("AWS"))

bgt_map <- mapview(BGT, col.region=c("#f1f4c7", "#f0495f", "#444444", "#5dcb1e", "#25e0ed"), map.types = c("Esri.WorldImagery"), zcol=c("object")) + mapview(deBilt_wgs.sf, col.region ="yellow", layer.name="Temperature sensor", zcol=c("AWS"))
bgt_map@map %>% setView(5.17939, 52.09886, zoom = 18)
mapshot(bgt_map, file = "BGT.png", url = NULL, remove_url = TRUE, remove_controls = c("zoomControl", "layersControl"))

satellite <- mapview(deBilt_wgs.sf, col.region ="yellow", map.types = c("Esri.WorldImagery"), layer.name="Temperature sensor", zcol=c("AWS"))
satellite@map %>% setView(5.17939, 52.09886, zoom = 17)
mapshot(satellite, file = "satellite2.png", url = NULL, remove_url = TRUE, remove_controls = c("zoomControl", "layersControl"))
