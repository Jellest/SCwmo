multiple_intersect_bgt <- function(aws_list, exportCSV, exportShp){
  if(missing(exportCSV)){
    exportCSV <- TRUE
  }
  
  if(missing(exportShp)){
    exportShp <- TRUE
  }
  
  for(b in 1:length(aws_list)){
    aws_name <- aws_list[b]
    aws_name_trim <- getAWS_name_trim(aws_name)
    single_aws <- select_single_aws(aws.df = AWS.df, aws_name = aws_name, sensor_name ="temp_150cm")
    bgt_path <- 
    bgt.shp <- st_read(dsn = paste0("data/BGT/", aws_name_trim), layer = paste0("BGT_", aws_name_trim)) 
    bgt.sf <- st_transform(bgt.shp, epsg_rd)
    #bgt.sf <- st_as_sf(bgt.shp)
    land_use <- presence_objects(aws_name = "De Bilt", coords = single_aws[["aws_rd.sf"]], bgt_shape = bgt.sf, temperature_criteria.df = single_aws[["aws.df"]][,c(1,5)], exportCSV = exportCSV, exportShp = exportShp)
    
    View(land_use[["df"]])
  }
}

bgt_test <- multiple_intersect_bgt(c("De Bilt"), exportCSV = TRUE, exportShp = TRUE)
