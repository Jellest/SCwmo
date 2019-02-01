library(leaflet)
library(mapview)
raster_map <- function(aws_name, sensor_name, addition = "", diff_raster, shadow_raster ,aws.df = AWS.df){
  single_aws <- select_single_aws(aws_name = aws_name, sensor_name = sensor_name, aws.df = aws.df)[["aws.df"]]
  aws_name_trim <- getAWS_name_trim(aws.df = aws.df, aws_name = aws_name, addition = addition)
  
  overlays <- c("AWS", "AHN3 - AHN2", "AHN Shadow angles")
  luchtfoto <- "https://geodata.nationaalgeoregister.nl/luchtfoto/rgb/wmts/2018_ortho25/EPSG:3857/{z}/{x}/{y}.jpeg"
  map <- leaflet(options = leafletOptions(
    #crs = epsg28992, 
    minZoom = 4, maxZoom = 20
  )) %>%
    setView(single_aws[1,"LON"], single_aws[1,"LAT"], zoom = 8) %>%
    addTiles(luchtfoto, attribution = "KNMI, Kadaster, PDOK, AHN, 2018") %>%
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
    addCircleMarkers(data = single_aws[1,], lng = single_aws[1,"LON"], lat = single_aws[1,"LAT"],
                     radius = 5,  stroke = TRUE, color = "black", weight = 2.0,
                     fillColor = "#e9ff00", fillOpacity = 1.0,
                     group = "AWS")

  #add diff raster
  pal0 <- colorNumeric(c("#ffffff", "#000000"), values(diff_raster), na.color = "transparent")
  map <- addRasterImage(map, diff_raster, colors = pal0, opacity = 0.8, group = "AHN3 - AHN2")
  
  pal <- colorNumeric(c("#e5f5f9", "#99d8c9", "#2ca25f"), values(shadow_raster), na.color = "transparent")
  map <- addRasterImage(map, shadow_raster, colors = pal, opacity = 0.8, group = "AHN2 shadow angles")
  
  
  #add legend and control
  map <- addLegend(map, position = "topright", values = aws_name, color = "#e9f0", labels = aws_name,
                   title = "temperature sensor", group = "AWS")

  map <- addLegend(map, position = "topright", values = c(6:18), pal = pal0,
                     title = "AHN3 - AHN2", group = "AHN3 - AHN2")
  
  map <- addLegend(map, position = "topright", values = c(0:60), pal = pal,
                   title = "AHN2 shadow angles", group = "AHN2 shadow angles")
  

  map <- addLayersControl(map,
                          position = "topleft",
                          baseGroups = c("Satellite"),
                          overlayGroups = c("AHN3 - AHN2","AHN2 shadow angles"),
                          options = layersControlOptions(collapsed = FALSE)) %>%
  setView(single_aws[1,"LON"], single_aws[1,"LAT"], zoom = 17)
  return(map)
}

raster_map(aws_name = "Vlissingen",
           sensor_name = temperature_sensor_name,
           diff_raster = raster("data/AHN2/Vlissingen/raw/Vlissingen_aws_AHN2_raw_ahn.tif"),
           shadow_raster = raster("output/Vlissingen/solar_shadow_angles/rasters/AHN2/Shadows/Vlissingen_AHN2_sa_171.526696898183.tif")
          )
