library(leaflet)
library(mapview)
#RDNew_CRS <- function(){
resolutions <- c(3440.640, 1720.320, 860.160, 430.080, 215.040, 107.520, 53.760, 26.880, 13.440, 6.720, 3.360, 1.680, 0.840, 0.420)
RD_New <- leafletCRS(crsClass = 'L.Proj.CRS', code = 'EPSG:28992',
                        proj4def = '+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.417,50.3319,465.552,-0.398957,0.343988,-1.8774,4.0725 +units=m +no_defs',
                        resolutions = resolutions
                        , bounds = c(-285401.92, 22598.08, 595401.9199999999, 903401.9199999999)
                        #, origin = c(-305401.92, 22598.08)#c(7.1389803, 48.1833305)
                        )
#return(RD_New)}
minZoom <- 7
maxZoom <- 18

lyrs <- c('brtachtergrondkaart', 'brtachtergrondkaartgrijs', '2017_ortho25') 
lyr <- lyrs[3]
basemaps <- c(paste("http://geodata.nationaalgeoregister.nl/wmts/?SERVICE=WMTS&REQUEST=GetTile&VERSION=1.0.0&LAYER=", lyrs[1], "&TILEMATRIXSET=EPSG:3857&TILEMATRIX=EPSG:3857:{z}&TILEROW={y}&TILECOL={x}&FORMAT=image/png", sep=""), paste("http://geodata.nationaalgeoregister.nl/wmts/?SERVICE=WMTS&REQUEST=GetTile&VERSION=1.0.0&LAYER=", lyrs[2], "&TILEMATRIXSET=EPSG:3857&TILEMATRIX=EPSG:3857:{z}&TILEROW={y}&TILECOL={x}&FORMAT=image/png", sep=""), paste("http://geodata.nationaalgeoregister.nl/wmts/?SERVICE=WMTS&REQUEST=GetTile&VERSION=1.0.0&LAYER=", lyrs[3], "&TILEMATRIXSET=EPSG:3857&TILEMATRIX=EPSG:3857:{z}&TILEROW={y}&TILECOL={x}&FORMAT=image/png", sep=""), "https://geodata.nationaalgeoregister.nl/tiles/service/wmts?request=GetCapabilities&service=WMTS&REQUEST=GetTile&VERSION=1.0.0&LAYER=ahn2_05m_ruw&TILEMATRIXSET=EPSG:28992&TILEMATRIX=EPSG:28992:{z}&TILEROW={y}&TILECOL={x}&FORMAT=image/png")
map <- leaflet(options = leafletOptions(
  minZoom = minZoom
  , maxZoom = maxZoom
  # , crs = RD_New
  )) %>%
  setView(5.177565, 52.099006, zoom = 15) %>%
  addTiles(basemaps[1], group = "BRT") %>%
  addTiles(basemaps[2], group = "BRT Gray") %>%
  addTiles(basemaps[3], group = "Satellite") %>%
  # addTiles(basemap_rd, group = "AHN2") %>%
  # addWMSTiles(
  #   "https://geodata.nationaalgeoregister.nl/ahn2/wms?"
  #   , layers = "ahn2_05m_ruw"
  #   , options = WMSTileOptions(format = "image/png", transparent = TRUE, crs=leafletCRS("L.CRS.EPSG3857"))
  #   , group = "AHN2") %>%
  addMouseCoordinates  %>%
  addLayersControl(
    baseGroups = c("BRT", "BRT Gray", "Satellite"),
    # overlayGroups = c("AHN2"),
    options = layersControlOptions(collapsed = TRUE)) %>%
  addMeasure(
    position = "bottomleft",
    primaryLengthUnit = "meters",
    primaryAreaUnit = "sqmeters",
    activeColor = "#3D535D",
    completedColor = "#7D4479")
map

map %>% setView(5.177565, 52.099006, zoom = 15)

