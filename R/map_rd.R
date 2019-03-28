#'RD map
#'
#'@title RD map
#'@description create interactive RD map
#'@param name of AWS or location
#'@param spatialpoint single sf point in RD new coordinates
#'@param AHN3 Default TRUE. Set to FALSE if AHN2 needs to be used.
#'@author Jelle Stuurman
#'@return Rd map as leaflet object
map_rd <- function(spatialpoint, name, sensor.name, name.supplement = "", AWS = FALSE, buffers, vegetation_height_raster, vegetation_radius, AHN3 = TRUE, class = "", zoom = 17){
#   tag.map.title <- tags$style(HTML("
#   .leaflet-control.map-title {
#     transform: translate(-50%,20%);
#     position: fixed !important;
#     left: 50%;
#     text-align: center;
#     padding-left: 10px;
#     padding-right: 10px;
#     background: rgba(255,255,255,0.75);
#     font-weight: bold;
#     font-size: 28px;
#   }
# "))
#
  #View(spatialpoint)
  if(AWS == FALSE){
    spatialpoint.wgs <- sf::st_transform(spatialpoint, rgdal::CRSargs(CRS("+init=epsg:4326")))
    #View(spatialpoint.wgs)
    geom <- data.frame(sf::st_coordinates(spatialpoint.wgs))
    spatialpoint.df <- data.frame(LON = geom$X, LAT = geom$Y)
  } else {
    spatialpoint.df <- data.frame(spatialpoint)
  }
  name_trim <- getName_trim(name = name, name.supplement = name.supplement)

  #print(class(vegetation_height_raster))
  if(AHN3 == TRUE){
    AHN <- "AHN3"
  } else {
    AHN <- "AHN2"
  }

  if(missing(buffers)){
    circles <- FALSE
  } else {
    circles <- TRUE
    if(100 %in% buffers == FALSE){
      buffers <- base::append(100, buffers)
    }
  }

  if(missing(vegetation_height_raster)){
    vegetation <- FALSE
  } else {
    vegetation <- TRUE
    vegetation_height_raster <- raster::raster(paste0("output/", name_trim, "/vegetation_height/", name_trim, "_", AHN, "_", vegetation_radius, "m_height_difference.tif"))
  }


  #resolutions <- c(3440.640, 1720.320, 860.160, 430.080, 215.040, 107.520, 53.760, 26.880, 13.440, 6.720, 3.360, 1.680, 0.840, 0.420)
  # epsg28992 <- leafletCRS(crsClass= 'L.Proj.CRS', code = 'EPSG:28992',
  #                         proj4def = '+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.417,50.3319,465.552,-0.398957,0.343988,-1.8774,4.0725 +units=m +no_defs',
  #                         resolutions = resolutions,
  #                         bounds = c(-285401.92, 22598.08, 595401.9199999999, 903401.9199999999)
  #                         #, origin = c(-305401.92, 22598.08))  #c(7.1389803, 48.1833305)
  #                         )
  overlays <- c("Location", "AHN2", "AHN3")
  satellite_image <- "https://geodata.nationaalgeoregister.nl/luchtfoto/rgb/wmts/Actueel_ortho25/EPSG:3857/{z}/{x}/{y}.jpeg"
  map <- leaflet::leaflet(options = leafletOptions(
    #crs = epsg28992,
    minZoom = 4, maxZoom = 20
    )) %>%
    setView(spatialpoint.df[1,"LON"], spatialpoint.df[1,"LAT"], zoom = zoom) %>%
    addTiles(satellite_image, attribution = "KNMI, Kadaster, PDOK, AHN, 2018") %>%
    addScaleBar(position = "bottomright", options = scaleBarOptions(maxWidth = 100, metric = TRUE, imperial = FALSE, updateWhenIdle = TRUE)) %>%
    # addMeasure(
    #   position = "bottomleft",
    #   primaryLengthUnit = "meters",
    #   primaryAreaUnit = "sqmeters",
    #   activeColor = "#3D535D",
    #   completedColor = "#7D4479") %>%
    addMouseCoordinates() %>%
    addWMSTiles(
      "https://geodata.nationaalgeoregister.nl/ahn2/wms?"
      , layers = "ahn2_05m_ruw"
      , options = WMSTileOptions(format = "image/png", transparent = FALSE)
      , group = "AHN2"
    )%>%
    addWMSTiles(
      "https://geodata.nationaalgeoregister.nl/ahn3/wms?"
      , layers = "ahn3_05m_dsm"
      , options = WMSTileOptions(format = "image/png", transparent = FALSE)
      , group = "AHN3"
    )
    #addControl(title, position = "bottomleft", className="map-title")

    # addWMSTiles(
    #   "https://geodata.nationaalgeoregister.nl/beta/bgt/wms?"
    #   , layers = "BGT WMS"
    #   , options = WMSTileOptions(format = "image/png", transparent = FALSE)
    #   , group = "BGT"
    # ) %>%
    #addMarkers(data = )

    # %>% addLegend("bottomright", pal = pal, values = ~gdp_md_est,
    #            title = "Est. GDP (2010)",
    #            labFormat = labelFormat(prefix = "$"),
    #            opacity = 1
    # )

  ## Add Land use (BGT)
  if(circles == TRUE){
    overlays <- base::append(overlays, c("Land use (BGT)", "Circle areas"))
    bgt.sf <- sf::st_read(dsn = paste0("datasets/BGT/", name_trim, "/BGT_", name_trim, ".shp"))

    bgt.sf <- sf::st_transform(bgt.sf, rgdal::CRSargs(CRS("+init=epsg:28992")))
    bgt_wgs <- sf::st_transform(bgt.sf, "+init=epsg:4326")

    fillColours <- as.character(bgt_wgs$hexColour)
    #print(fillColours)
    bgt_objects <- as.character(bgt_wgs$object)
    bgt_objects <- base::unique(bgt_objects)
    bgt_colours <- base::unique(fillColours)
    #print(bgt_objects)
    #print(bgt_colours)
    id <- 1:nrow(bgt_wgs)
    bgt_counter <- 1
    for(i in id){
      map <- leaflet::addPolygons(map, data = bgt_wgs[id[bgt_counter],], smoothFactor = 0.5,
                         opacity = 1.0, color = 'black', weight = 0.4,
                         fillColor = fillColours[id[bgt_counter]], fillOpacity = .8,
                         group = "Land use (BGT)"
                         )
      bgt_counter <- bgt_counter + 1
    }
  }
  map <- leaflet::addCircleMarkers(map, data = spatialpoint.df[1,], lng = spatialpoint.df[1,"LON"], lat = spatialpoint.df[1,"LAT"],
                           radius = 5,  stroke = TRUE, color = "black", weight = 2.0,
                           fillColor = "#e9ff00", fillOpacity = 1.0,
                           group = "Location")

  #add height difference raster
  if(vegetation == TRUE){
    overlays <- base::append(overlays, "Vegetation height")
    pal <- colorNumeric(c("#e5f5f9", "#99d8c9", "#2ca25f"), values(vegetation_height_raster), na.color = "transparent")
    map <- addRasterImage(map, vegetation_height_raster, colors = pal, opacity = 0.8, group = "Vegetation height")
  }
  if(circles == TRUE){
    #add circles
    for (b in 1:length(buffers)){
      map <- leaflet::addCircles(map, data = spatialpoint.df,
                        lng = ~LON, lat = ~LAT,
                        radius = buffers[b],
                        opacity = 1.0, color = 'black', weight = 2.0,
                        fill = TRUE, fillOpacity = 0,
                        label = paste(buffers[b], "m"),
                        group = "Circle areas")
    }
  }

  if(circles == TRUE) {
    name_label <- paste0(name, " (land use class ", class,")")
  } else {
    name_label <- paste0(name)
  }

  #add legend and control
  map <- leaflet::addLegend(map, position = "topright", values = name, color = "#e9ff00", labels = name_label,
              title = "Air temperature sensor", group = "Location")
  #if(circles == TRUE){
    map <- leaflet::addLegend(map, position = "topright",
                    values = bgt_objects, colors = bgt_colours, labels = bgt_objects,
                    title = "Land use", group = "Land use (BGT)")
 #}
  if(vegetation == TRUE){
    map <- leaflet::addLegend(map, position = "topright", values = values(vegetation_height_raster), pal = pal,
                title = "Detected heigt (m)", group = "Vegetation height")
  }

  map <- leaflet::addLayersControl(map,
                                  position = "topleft",
                                  baseGroups = c("Satellite"),
                                  overlayGroups = overlays,
                                  options = leaflet::layersControlOptions(collapsed = FALSE)) %>%
  setView(spatialpoint.df[1,"LON"], spatialpoint.df[1,"LAT"], zoom = zoom)
  map <- leaflet::hideGroup(map, "AHN2")
  map <- leaflet::hideGroup(map, "AHN3")
  return(map)
}
