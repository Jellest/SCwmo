generate_map <- function(aws_name, sensor_name, addition = "", aws.df = AWS.df, ahn, bgt.sf) {
  
  aws_name_trim <- getAWS_name_trim(aws.df = aws.df, aws_name = aws_name, addition = addition)
  single_aws <- select_single_aws(aws.df = aws.df, aws_name = aws_name, sensor_name =sensor_name)
  
  buffer_100m <- createBuffer(single_aws[["aws_rd.sf"]], distance = 100)
  buffer_30m <- createBuffer(single_aws[["aws_rd.sf"]], distance = 30)
  buffer_10m <- createBuffer(single_aws[["aws_rd.sf"]], distance = 10)
  buffer_5m <- createBuffer(single_aws[["aws_rd.sf"]], distance = 5)
  buffer_3m <- createBuffer(single_aws[["aws_rd.sf"]], distance = 3)

  bgt <- bgt.sf
  bgt <- st_transform(bgt.sf, epsg_rd)
  
  
  artificial_objects <- subset(bgt, object_typ == "pand" | object_typ == "wegdeel" | object_typ == "waterdeel")
  buildings <- subset(bgt, object_typ == "pand")
  water <- subset(bgt, object_typ == "waterdeel")
  roads <- subset(bgt, object_typ == "wegdeel")
  barren <- subset(bgt, object_typ == "onbegroeidterreindeel")
  vegetation <- subset(bgt, object_typ == "begroeidterreindeel")
  
  map <- mapview(bgt, col.region=c("#f1f4c7", "#f0495f", "#444444", "#5dcb1e", "#25e0ed"), map.types = c("Esri.WorldImagery"), zcol=c("object")) + mapview(single_aws[[3]], col.region ="yellow", layer.name="Temperature sensor", zcol=c("AWS"))
  
  
return (map)}
temp_map()

temp_map <-function(){

aws_name_trim <- getAWS_name_trim(aws.df = AWS.df, aws_name = "De Bilt", addition="")
single_aws <- select_single_aws(aws.df = AWS.df, aws_name = "De Bilt", sensor_name =temperature_sensor_name)
my_bgt <-st_read(dsn = "data/BGT/DeBilt/BGT_DeBilt.shp") 
bgt_map <- mapview(my_bgt, col.region=c("#f1f4c7", "#f0495f", "#444444", "#5dcb1e", "#25e0ed"), map.types = c("Esri.WorldImagery"), zcol=c("object")) + mapview(single_aws[["aws_rd.sf"]], col.region ="yellow", layer.name="Temperature sensor", zcol=c("AWS"))
generate_map("De Bilt", temperature_sensor_name, bgt.sf = my_bgt)


#bgt_map@map %>% setView(5.17939, 52.09886, zoom = 18)
#mapshot(bgt_map, file = "BGT.png", url = NULL, remove_url = TRUE, remove_controls = c("zoomControl", "layersControl", "homeButton"))
}         
