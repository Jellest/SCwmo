library(leaflet)
library(mapview)
raster_map <- function(aws_name, sensor_name, addition = "", raster, radius = 0, aws.df = AWS.df, name, zoom){
  single_aws <- select_single_aws(aws_name = aws_name, sensor_name = sensor_name, aws.df = aws.df)
  aws_name_trim <- getAWS_name_trim(aws.df = aws.df, aws_name = aws_name, addition = addition)
  
  if(radius !=0){
    aws_mask<-raster::buffer(single_aws[["aws_rd.sp"]], width=radius)
    
    raster <- raster::mask(raster, aws_mask)
  }
  
  overlays <- c("AWS", name)
  luchtfoto <- "https://geodata.nationaalgeoregister.nl/luchtfoto/rgb/wmts/2018_ortho25/EPSG:3857/{z}/{x}/{y}.jpeg"
  map <- leaflet(options = leafletOptions(
    #crs = epsg28992, 
    minZoom = 4, maxZoom = 20
  )) %>%
    setView(single_aws[["aws.df"]][1,"LON"], single_aws[["aws.df"]][1,"LAT"], zoom = zoom) %>%
    addTiles(luchtfoto, attribution = "KNMI, PDOK, AHN, 2018") %>%
    addScaleBar(position = "bottomright",
                options = scaleBarOptions(
                  maxWidth = 100,
                  metric = TRUE,
                  imperial = FALSE,
                  updateWhenIdle = TRUE))
    # addMeasure(
    #   position = "bottomleft",
    #   primaryLengthUnit = "meters",
    #   primaryAreaUnit = "sqmeters",
    #   activeColor = "#3D535D",
    #   completedColor = "#7D4479") %>%
    
  
  #add raster ##ffffff", "#9ecae1", "#0021ff
  #deebf7", "#08519c"
  pal <- colorNumeric(c("#deebf7", "#08519c"), values(raster), na.color = "transparent")
  map <- addRasterImage(map, raster, colors = pal, opacity = 0.8, group = name)
  
  
  map <- addCircleMarkers(map, data = single_aws[["aws.df"]][1,], lng = single_aws[["aws.df"]][1,"LON"], lat = single_aws[["aws.df"]][1,"LAT"],
                          radius = 5,  stroke = TRUE, color = "black", weight = 2.0,
                          fillColor = "#e9ff00", fillOpacity = 1.0,
                          group = "AWS")
  
  
  #add legend and control
  map <- addLegend(map, position = "topright", values = aws_name, color = "#e9ff00", labels = aws_name,
                   title = "Air temperature sensor", group = "AWS")
  
  map <- addLegend(map, position = "topright", values = values(raster), pal = pal,
                   title = name, group = name) #, labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))
  
  
  map <- addLayersControl(map,
                          position = "topleft",
                          baseGroups = c("Satellite"),
                          overlayGroups = c("AWS", name),
                          options = layersControlOptions(collapsed = FALSE)) %>%
  setView(single_aws[["aws.df"]][1,"LON"], single_aws[["aws.df"]][1,"LAT"], zoom = zoom)
  map
  return(map)
}

shadow_raster <- raster_map(aws_name = "De Bilt",
                            sensor_name = temperature_sensor_name,
                            raster = raster("output/DeBilt/solar_shadow_angles/rasters/AHN3/Shadows/DeBilt_AHN3_sa_99.7438895423655.tif"),
                            name = "Shadow angle (degrees)",
                            zoom = 18)

shadow_raster
mapshot(ahn_raster, file = "shadow_angle_map.png", url = NULL, remove_url = TRUE, remove_controls = c("zoomControl", "layersControl", "homeButton"), selfcontained = FALSE)


ahn_raster <- raster_map(aws_name = "De Bilt",
           sensor_name = temperature_sensor_name,
           raster = raster("data/AHN3/DeBilt/raw/DeBilt_AHN3_raw_ahn.tif"),
           radius = 500,
           zoom = 16,
           name= "AHN3 altitude (m)")
ahn_raster

mapshot(ahn_raster, file = "DeBilt_AHN3.png", url = NULL, remove_url = TRUE, remove_controls = c("zoomControl", "layersControl", "homeButton"), selfcontained = FALSE)
