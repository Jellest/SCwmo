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
    overlayGroups = c("AWS", " buildings"),
    options = layersControlOptions(collapsed = TRUE)) %>%
  addMeasure(
    position = "bottomleft",
    primaryLengthUnit = "meters",
    primaryAreaUnit = "sqmeters",
    activeColor = "#3D535D",
    completedColor = "#7D4479")

map %>% setView(5.177565, 52.099006, zoom = 15)


View(buildings.sf)
