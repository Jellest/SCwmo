library(leaflet)
library(mapview)
bgt_map <- function(aws_name, sensor_name, addition = "", bgt, aws.df = AWS.df){
  single_aws <- select_single_aws(aws_name = aws_name, sensor_name = sensor_name, aws.df = aws.df)
  aws_name_trim <- getAWS_name_trim(aws.df = aws.df, aws_name = aws_name, addition = addition)

  bgt <- st_read(dsn = bgt, layer = "BGT_DeBilt")
  
  overlays <- c("AWS", "BGT")
  luchtfoto <- "https://geodata.nationaalgeoregister.nl/luchtfoto/rgb/wmts/2018_ortho25/EPSG:3857/{z}/{x}/{y}.jpeg"
  map <- leaflet(options = leafletOptions(
    #crs = epsg28992, 
    minZoom = 4, maxZoom = 20
  )) %>%
    setView(single_aws[["aws.df"]][1,"LON"], single_aws[["aws.df"]][1,"LAT"], zoom = 8) %>%
    addTiles(luchtfoto, attribution = "KNMI, Kadaster, PDOK, 2018") %>%
    addScaleBar(position = "bottomright",
                options = scaleBarOptions(
                  maxWidth = 100,
                  metric = TRUE,
                  imperial = FALSE,
                  updateWhenIdle = TRUE)) %>%
    # addMeasure(
    #   position = "bottomleft",
    #   primaryLengthUnit = "meters",
    #   primaryAreaUnit = "sqmeters",
    #   activeColor = "#3D535D",
    #   completedColor = "#7D4479") %>%
    addCircleMarkers(data = single_aws[["aws.df"]][1,], lng = single_aws[["aws.df"]][1,"LON"], lat = single_aws[["aws.df"]][1,"LAT"],
                     radius = 5,  stroke = TRUE, color = "black", weight = 2.0,
                     fillColor = "#e9ff00", fillOpacity = 1.0,
                     group = "AWS")
  
  #add BGT
  bgt <- st_transform(bgt, epsg_rd)
  my_bgt_wgs <- st_transform(bgt, "+init=epsg:4326")
  
  fillColours <- as.character(my_bgt_wgs$hexColour)
  bgt_objects <- as.character(my_bgt_wgs$object)
  bgt_objects <- unique(bgt_objects)
  bgt_colours <- unique(fillColours)
  id <- 1:nrow(my_bgt_wgs)
  bgt_counter <- 1
  for(i in id){
    map <- addPolygons(map, data = my_bgt_wgs[id[bgt_counter],], smoothFactor = 0.5,
                       opacity = 1.0, color = 'black', weight = 0.4,
                       fillColor = fillColours[id[bgt_counter]], fillOpacity = .7, 
                       group = "Land use (BGT)"
    )
    bgt_counter <- bgt_counter + 1
  }

  #add legend and control
  map <- addLegend(map, position = "topright", values = aws_name, color = "#e9ff00", labels = aws_name,
                   title = "temperature sensor", group = "AWS")
  
  map <- addLegend(map, position = "topright",
                   values = bgt_objects, colors = bgt_colours, labels = bgt_objects,
                   title = "Land use", group = "BGT")
  
  
  map <- addLayersControl(map,
                          position = "topleft",
                          baseGroups = c("Satellite"),
                          overlayGroups = c("AWS", "BGT"),
                          options = layersControlOptions(collapsed = FALSE)) %>%
  setView(single_aws[["aws.df"]][1,"LON"], single_aws[["aws.df"]][1,"LAT"], zoom = 17)
  map
  return(map)
}

my_bgt_map <- bgt_map(aws_name = "De Bilt",
                          sensor_name = temperature_sensor_name,
                          bgt = "data/BGT/DeBilt/BGT_DeBilt.shp")

my_bgt_map
mapshot(my_bgt_map, file = "DeBilt_BGT.png", url = NULL, remove_url = TRUE, remove_controls = c("zoomControl", "layersControl", "homeButton"), selfcontained = FALSE)
