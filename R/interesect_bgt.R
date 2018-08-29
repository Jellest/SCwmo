#intersect bgt
library(sf)

intersect_bgt <- function(coords, shp, buffer_dist){
  buffer <- buffer(coords,width=buffer_dist)
  shape_insct<- intersect(shp, buffer)
return (shape_insct)}

shape_insct <- intersect_bgt(debilt, pand_shape, 200)


plot(shape_insct)

m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")
m  # Print the map

minZoom = 0
maxZoom = 13
resolutions <- 0.42*(2^(maxZoom:minZoom))

epsg28992 <- leafletCRS(crsClass = 'L.Proj.CRS', code = 'EPSG:28992',
                        proj4def = '+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.417,50.3319,465.552,-0.398957,0.343988,-1.8774,4.0725 +units=m +no_defs',
                        resolutions = resolutions,
                        bounds=   c(-285401.92, 22598.08, 595401.9199999999, 903401.9199999999))

urls<-(c('https://geodata.nationaalgeoregister.nl/tiles/service/wmts/grijs/EPSG:3857/{z}/{x}/{y}.png', 'https://geodata.nationaalgeoregister.nl/luchtfoto/rgb/wmts/1.0.0/Actueel_ortho25/EPSG:28992/{z}/{x}/{y}.jpeg', 'http://geodata.nationaalgeoregister.nl/tms/1.0.0/brtachtergrondkaart/{z}/{x}/{y}.png'))
leaflet(options = leafletOptions(
  crs = epsg28992, 
  minZoom = minZoom, maxZoom = maxZoom)) %>%
  
  addTiles() %>%
  setView(456881.8, 140802.698, zoom = 12) %>%  
  addWMSTiles(baseUrl = urls[1],
              options = WMSTileOptions(format = "image/png", transparent = TRUE),
              layers= 'Actueel_ortho25',
              attribution = ""
  
  
)
