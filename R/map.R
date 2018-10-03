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

map %>% setView(5.177565, 52.099006, zoom = 15)
# map %>% addFeatures(buildings_wgs.sf, weight = 0.7, color="black", fillColor = "red", opacity = 1, fillOpacity = 0.6) %>%
#   addFeatures(roads_wgs.sf, weight = 0.7, color="black", fillColor = "grey", opacity = 1, fillOpacity = 0.6) %>%
#   addFeatures(water_wgs.sf, weight = 0.7, color="black", fillColor = "blue", opacity = 1, fillOpacity = 0.6)
map %>% addRasterImage(ahn_mask, opacity = 1)

ahn2_raw_wgs <- st_transform(ahn2_deBilt_raw, "+init=epsg:4326")
View(buildings.sf)

buildings_wgs.sf <- st_transform(buildings.sf, "+init=epsg:4326")
roads_wgs.sf <- st_transform(raods.sf, "+init=epsg:4326")
water_wgs.sf <- st_transform(water.sf, "+init=epsg:4326") 
