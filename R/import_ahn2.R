import_ahn2 <- function(station_name, station_coords, distance){
  ahn2_WFS_baseUrl <- "https://geodata.nationaalgeoregister.nl/ahn2/wfs?SERVICE=WFS&VERSION=1.0.0&REQUEST=GetFeature&TYPENAME=ahn2:ahn2_bladindex"
  #create 200 meter buffer arouond sensor
  surroundingBuffer <- buffer(station_coords,width=distance) #since RDcoords are in meters width is also in m
  
  #get BBOX extent of buffer area
  surroundingExtent <- extent(surroundingBuffer)
  my_EPSG <- "EPSG:28992"
  my_bbox <- paste(toString(surroundingExtent@xmin), toString(surroundingExtent@ymin), toString(surroundingExtent@xmax), toString(surroundingExtent@ymax), sep=",")
  
  ahn2_wfs <- paste("WFS:", ahn2_WFS_baseUrl, "&SRSNAME=", my_EPSG, "&BBOX=", my_bbox, sep="")
  ahn2_directory <- paste("data", "AHN2", sep=folder_structure)
  dir.create(paste(ahn2_directory, station_name, sep=folder_structure), showWarnings = FALSE)
  working_directory <- paste(ahn2_directory, station, sep=folder_structure)
  dir.create(paste(working_directory, "raw", sep=folder_structure), showWarnings = FALSE)
  dir.create(paste(working_directory, "terrain", sep=folder_structure), showWarnings = FALSE)
  ahn2_raw_directory <- paste(working_directory, "raw", sep=folder_structure)
  
  bladIndex_shape_filepath <- paste( working_directory , "", sep=folder_structure)
  print(bladIndex_shape_filepath)
  bladIndex_shape_file <- paste( bladIndex_shape_filepath, "bladIndex", ".shp", sep="")
  #ogr2ogr(src_datasource_name = ahn2_wfs , dst_datasource_name = bladIndex_shape_file, layer = "ahn2:ahn2_bladindex", overwrite = TRUE)
  
  bladIndex.shp <- readOGR(dsn = working_directory, layer = "bladIndex", stringsAsFactors=FALSE)

  gml_ids <- bladIndex.shp$gml_id
  blad_count <- length(gml_ids)
  gml_ids_split <- strsplit(gml_ids, " ")
  print(gml_ids_split)
  
  bladnrs <- bladIndex.shp$bladnr
  bladnrs_split <- strsplit(bladnrs, " ")
  print(bladnrs_split[[1]])
  ahn2_ruw_BaseUrl <- "https://geodata.nationaalgeoregister.nl/ahn2/extract/ahn2_05m_ruw/"
  ahn2_ruw_downloadLink <- paste(ahn2_ruw_BaseUrl, "r", bladnrs[[1]]  ,".tif.zip", sep="")
  print(ahn2_ruw_downloadLink)
  ahn2_ruw <- download.file(ahn2_ruw_downloadLink, destfile = paste(ahn2_raw_directory, "r", bladnrs[[1]], ".zip", sep=""))
  ahn2_ruw_unzip <- unzip(ahn2_ruw, overwrite = TRUE, exdir = ahn2_raw_directory)
}
import_ahn2("deBilt", debilt, 1000)
