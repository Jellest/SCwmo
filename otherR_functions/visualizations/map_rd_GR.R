library(leaflet)
library(mapview)
map_rd <- function(aws_name, sensor_name, addition = "", buffers, vegetation_height_raster, vegetation_radius, AHN3 = FALSE, aws.df = AWS.df, class = "", zoom = 8){
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
  
  
  single_aws <- select_single_aws(aws_name = aws_name, sensor_name = sensor_name, aws.df = aws.df)[["aws.df"]]
  aws_name_trim <- getAWS_name_trim(aws.df = aws.df, aws_name = aws_name, addition = addition)
  
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
      buffers <- append(100, buffers)
    }
  }
  
  if(missing(vegetation_height_raster)){
    vegetation <- FALSE
  } else {
    vegetation <- TRUE
    vegetation_height_raster <- raster(paste0("output/", aws_name_trim, "/vegetation_height/", aws_name_trim, "_", AHN, "_", vegetation_radius, "m_height_difference.tif"))
  }
  
  
  #resolutions <- c(3440.640, 1720.320, 860.160, 430.080, 215.040, 107.520, 53.760, 26.880, 13.440, 6.720, 3.360, 1.680, 0.840, 0.420)
  # epsg28992 <- leafletCRS(crsClass= 'L.Proj.CRS', code = 'EPSG:28992',
  #                         proj4def = '+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.417,50.3319,465.552,-0.398957,0.343988,-1.8774,4.0725 +units=m +no_defs',
  #                         resolutions = resolutions,
  #                         bounds = c(-285401.92, 22598.08, 595401.9199999999, 903401.9199999999)
  #                         #, origin = c(-305401.92, 22598.08))  #c(7.1389803, 48.1833305)
  #                         )                 
  overlays <- c("AWS", "AHN2", "AHN3")
  luchtfoto <- "https://geodata.nationaalgeoregister.nl/luchtfoto/rgb/wmts/2018_ortho25/EPSG:3857/{z}/{x}/{y}.jpeg"
  map <- leaflet(options = leafletOptions(
    #crs = epsg28992, 
    minZoom = 4, maxZoom = 20
    )) %>%
    setView(single_aws[1,"LON"], single_aws[1,"LAT"], zoom = zoom) %>%
    addTiles(luchtfoto, attribution = "KNMI, PDOK, 2018") %>%
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
    #%>%
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
    overlays <- append(overlays, c("Land use (BGT)", "Circle areas"))
    my_bgt <- st_read(dsn = paste0("data/BGT/", aws_name_trim, "/BGT_", aws_name_trim, ".shp"))
    #my_bgt <- st_read(dsn = "output/DeBilt/land_use/DeBilt_100m_buffer_data.shp") 
    
    bgt <- st_transform(my_bgt, epsg_rd)
    my_bgt_wgs <- st_transform(bgt, "+init=epsg:4326")
    
    my_bgt_wgs <- subset(my_bgt_wgs, object == "vegetation" | object == "road")
    
    fillColours <- as.character(my_bgt_wgs$hexColour)
    bgt_objects <- as.character(my_bgt_wgs$object)
    bgt_objects <- unique(bgt_objects)
    bgt_colours <- unique(fillColours)
    
    id <- 1:nrow(my_bgt_wgs)
    bgt_counter <- 1
    for(i in id){
      map <- addPolygons(map, data = my_bgt_wgs[id[bgt_counter],], smoothFactor = 0.5,
                         opacity = 1.0, color = 'black', weight = 0.4,
                         fillColor = fillColours[id[bgt_counter]], fillOpacity = 0.7, 
                         group = "Land use (BGT)"
                         )
      bgt_counter <- bgt_counter + 1
    }
  }
  #add height difference raster
  if(vegetation == TRUE){
    overlays <- append(overlays, "Vegetation height")
    pal <- colorNumeric(c("#e5f5f9", "#99d8c9", "#2ca25f"), values(vegetation_height_raster), na.color = "transparent")
    map <- addRasterImage(map, vegetation_height_raster, colors = pal, opacity = 0.8, group = "Vegetation height")
  }
  if(circles == TRUE){
    #add circles
    for (b in 1:length(buffers)){
      map <- addCircles(map, data = single_aws,
                        lng = ~LON, lat = ~LAT,
                        radius = buffers[b], 
                        opacity = 1.0, color = 'black', weight = 2.0,
                        fill = TRUE, fillOpacity = 0,
                        label = paste(buffers[b], "m"),
                        group = "Circle areas")
    }
  }
  map <- addCircleMarkers(map, data = single_aws[1,], lng = single_aws[1,"LON"], lat = single_aws[1,"LAT"],
                          radius = 5,  stroke = TRUE, color = "black", weight = 2.0,
                          fillColor = "#e9ff00", fillOpacity = 1.0,
                          group = "AWS") 
  
  
  
  if(circles == TRUE) {
    aws_label <- paste0(aws_name)#, " (land use class ", class,")")
  } else {
    aws_label <- paste0(aws_name)
  }
  #add legend and control
  map <- addLegend(map, position = "topright", values = aws_name, color = "#e9ff00", labels = aws_label,
              title = "Air temperature sensor", group = "AWS")
  if(circles == TRUE){
    map <- addLegend(map, position = "topright",
                    values = bgt_objects, colors = bgt_colours, labels = bgt_objects,
                    title = "Land use", group = "Land use (BGT)")
  }
  if(vegetation == TRUE){
    map <- addLegend(map, position = "topright", values = values(vegetation_height_raster), pal = pal,
                title = "Detected heigt (m)", group = "Vegetation height")
  }
  
  map <- addLayersControl(map,
    position = "topleft",
    baseGroups = c("Satellite"),
    overlayGroups = overlays,
    options = layersControlOptions(collapsed = FALSE)) %>%
  setView(single_aws[1,"LON"], single_aws[1,"LAT"], zoom = zoom)
  map <- hideGroup(map, "AHN2")
  map <- hideGroup(map, "AHN3")
  return(map)
}

vegetation_map <- map_rd(aws_name = "De Bilt",
       sensor_name = temperature_sensor_name,
       zoom = 13,
       #buffers = c(30, 10),
       #vegetation_height_raster = FALSE,
       vegetation_radius = 10,
       AHN3 = TRUE)

vegetation_map

mapshot(vegetation_map, file = "greaterArea_DeBilt.png", url = NULL, remove_url = TRUE, remove_controls = c("zoomControl", "layersControl", "homeButton"))


mapshot(vegetation_map, file = "bgt_intersect_map.png", url = NULL, remove_url = TRUE, remove_controls = c("zoomControl", "layersControl", "homeButton"))


ras <- raster(paste0("output/DeBilt/vegetation_height/DeBilt_AHN3_10m_height_difference.tif"))
breaks <- c(0,0.1,0.4,0.6)
pal <- colorNumeric(c("#e5f5f9", "#99d8c9", "#2ca25f"), values(ras), na.color = "transparent")

plot(ras, col = pal, ) 

