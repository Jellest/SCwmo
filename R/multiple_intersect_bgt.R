multiple_intersect_bgt <- function(aws.df = AWS.df, aws_list, sensor_name, criteria_columnName = "Criteria_Value", exportCSV = FALSE, exportShp = FALSE){
  presence_objects_list <- list()
  for(b in 1:length(aws_list)){
    aws_name <- aws_list[b]
    aws_name_trim <- getAWS_name_trim(aws.df = aws.df, aws_name = aws_name)
    single_aws <- select_single_aws(aws.df = aws.df, aws_name = aws_name, sensor_name =sensor_name)
    bgt.shp <- st_read(dsn = paste0("data/BGT/", aws_name_trim), layer = paste0("BGT_", aws_name_trim)) 
    bgt.sf <- st_transform(bgt.shp, epsg_rd)
    #View(single_aws[["aws_rd.sf"]])
    #bgt.sf <- st_as_sf(bgt.shp)
    presence_objects <- find_land_use_and_classes(aws.df = aws.df, aws_name = aws_name, coords = single_aws[["aws_rd.sf"]], bgt_shape = bgt.sf, temperature_criteria.df = single_aws[["aws.df"]][,c(1,5)], criteria_columnName = criteria_columnName, exportCSV = exportCSV, exportShp = exportShp)
  }
  return (presence_objects)
}

intersection <- multiple_intersect_bgt(aws_list = c("De Bilt"), sensor_name = temperature_sensor_name, exportCSV = TRUE, exportShp = FALSE)
View(intersection[["df"]])
