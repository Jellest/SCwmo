import_ahn2 <- function(aws_name, station_coords, resolution, radius, raw_ahn2, terrain_ahn2){
  aws_name_trim <- getAWS_name_trim(aws_name)
  if(missing(resolution)){
    resolution_name = "05m"
  } else {
    if(resolution == 0.5){
      resolution_name = "05m"
    } else if(resolution == 5){
      resolution_name = "5"
    }
  }
  
  if(missing(raw_ahn2)){
    raw_ahn2 = TRUE
  }
  if(missing(terrain_ahn2)){
    terrain_ahn2 = FALSE
  }
  #create radius buffer arouond sensor
  point.sf <- st_as_sf(station_coords)
  surroundingBuffer <<- st_buffer(point.sf,dist=radius) #since RDcoords are in meters width is also in m

  #get BBOX extent of buffer area
  surroundingExtent <- extent(surroundingBuffer)
  my_EPSG <- "EPSG:28992"
  my_bbox <- paste(toString(surroundingExtent@xmin), toString(surroundingExtent@ymin), toString(surroundingExtent@xmax), toString(surroundingExtent@ymax), sep=",")
  
  #get AHN2 bladIndex
  ahn2_directory <- paste("data", "AHN2", sep="/")
  bladIndex_shape_filepath <- paste(ahn2_directory , sep="/")
  bladIndex_shape_file <- paste( bladIndex_shape_filepath, "/", "bladIndex", ".shp", sep="")
  print(bladIndex_shape_file)
  if(!file.exists(bladIndex_shape_file)){
    print("Download ahn2 wfs blad Index")
    ahn2_WFS_baseUrl <- "https://geodata.nationaalgeoregister.nl/ahn2/wfs?SERVICE=WFS&VERSION=1.0.0&REQUEST=GetFeature&TYPENAME=ahn2:ahn2_bladindex"
    ahn2_wfs <- paste("WFS:", ahn2_WFS_baseUrl, "&SRSNAME=", my_EPSG, sep="")
    ogr2ogr(src_datasource_name = ahn2_wfs , dst_datasource_name = bladIndex_shape_file, layer = "ahn2:ahn2_bladindex", overwrite = TRUE)
  }
  #load intersected blad indexes
  bladIndex.shp <- readOGR(dsn = ahn2_directory, layer = "bladIndex", stringsAsFactors=FALSE)
  p4s <- "+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.4171,50.3319,465.5524,-0.398957,0.343988,-1.8774,4.0725 +units=m +no_defs"
  bladIndex.shp <- spTransform(bladIndex.shp, p4s)
  bladIndex.sf <- st_as_sf(bladIndex.shp)

  st_agr(bladIndex.sf) <- "constant"
  st_agr(surroundingBuffer) <- "constant"
  bladIndex_buffer_intsct.sf <- st_intersection(bladIndex.sf, surroundingBuffer)
  
  bladnrs <<- bladIndex_buffer_intsct.sf$bladnr
  # bladnrs_split <- strsplit(bladnrs, " ")
  # print(paste("bladnrs:",bladnrs_split[1]))
  # gml_ids <- bladIndex.shp$gml_id
  # View(gml_ids)
  # blad_count <- length(gml_ids)
  # gml_ids_split <- strsplit(gml_ids, " ")
  # print(gml_ids_split)
  
  dir.create(paste(ahn2_directory, aws_name_trim, sep="/"), showWarnings = FALSE)
  aws_working_directory <- paste(ahn2_directory, aws_name_trim, sep="/")
  
  ahn2_atomFeed_BaseUrl <- paste("https://geodata.nationaalgeoregister.nl/ahn2/extract/ahn2_", sep="")
  #download raw ahn2 of corresponding resolultion
  indiv_raw_rasters <- list()
  if(raw_ahn2 == TRUE){  
    ahn2_raw_directory <- paste(aws_working_directory, "raw", sep="/")
    if(!file.exists(paste(ahn2_raw_directory, "/", aws_name_trim, "_raw_ahn2", '.tif', sep=""))){
      dir.create(paste(aws_working_directory, "raw", sep="/"), showWarnings = FALSE)
      print(ahn2_raw_directory)
      xList <<- c()
      ylist <- c()
      for(r in 1:length(bladnrs)){
        #https://geodata.nationaalgeoregister.nl/ahn2/extract/ahn2_05m_ruw/r32cn1.tif.zip
        ahn2_raw_downloadLink <- paste(ahn2_atomFeed_BaseUrl, resolution_name, "_ruw/r",  bladnrs[[r]], ".tif.zip", sep="")
        print(ahn2_raw_downloadLink)
        ahn2_rawZip_file_path <- paste(ahn2_raw_directory, "/", "r", bladnrs[[r]], ".zip", sep="")
        ahn2_raw_file_path <- paste(ahn2_raw_directory, "/", "r", bladnrs[[r]], ".tif",sep="")
        if(!file.exists(ahn2_raw_file_path)){
          download.file(ahn2_raw_downloadLink, destfile = ahn2_rawZip_file_path)
          unzip(ahn2_rawZip_file_path, overwrite = TRUE, exdir = ahn2_raw_directory)
          #indiv_raw_rasters[[r]] <- raster()
          file.remove(ahn2_rawZip_file_path)
        }
        #aws_mask<-raster::buffer(spatialpoint,width=distance)
        ahn2_sheet_raw <-stack(ahn2_raw_file_path)
        proj4string(ahn2_sheet_raw)<-CRS("+init=epsg:28992") 
        ahn2_crop<-raster::crop(ahn2_sheet_raw,surroundingBuffer)
        Xs <- c(xmin(ahn2_crop), xmax(ahn2_crop))
        Ys <- c(ymin(ahn2_crop), ymax(ahn2_crop))
        xList <<- rbind(xList, Xs)
        yList <<- rbind(yList, Ys)
        indiv_raw_rasters[[r]] <- raster(ahn2_raw_file_path)
        
      }
      
      indiv_raw_rasters$filename <- paste(aws_name_trim, "_raw_ahn2", '.tif', sep="")
      if(length(bladnrs) > 1){
        indiv_raw_rasters$overwrite <- TRUE
        print("Merging all raw rasters...")
        ahn2_raw_raster <- do.call(merge, indiv_raw_rasters)
        View(ahn2_raw_raster)
        print(str(ahn2_raw_raster))
        writeRaster(paste(ahn2_raw_directory, "/", aws_name_trim, "_raw_ahn2", '.tif', sep=""), overwrite = TRUE)
        message("Merge raw rasters complete.")
      } else if(length(bladnrs) == 1){
        ahn2_raw_raster <- indiv_raw_rasters[[1]]
      }
    }
  }
  
  #download terrain ahn2 of corresponding resolution
  indiv_terrain_rasters <- list()
  if(terrain_ahn2 == TRUE){
    ahn2_terrain_directory <- paste(aws_working_directory, "terrain", sep="/")
    if(!file.exists(paste(ahn2_terrain_directory, "/", aws_name_trim, "_terrain_ahn2", '.tif', sep=""))){
      dir.create(paste(aws_working_directory, "terrain", sep="/"), showWarnings = FALSE)
      print(ahn2_terrain_directory)
      for(t in 1:length(bladnrs)){
        #https://geodata.nationaalgeoregister.nl/ahn2/extract/ahn2_05m_int/i32cn1.tif.zip
        ahn2_terrain_downloadLink <- paste(ahn2_atomFeed_BaseUrl, resolution_name, "_int/i", bladnrs[[t]] ,".tif.zip", sep="")
        print(ahn2_terrain_downloadLink)
        ahn2_terrainZip_file_path <- paste(ahn2_terrain_directory, "/", "i", bladnrs[[t]], ".zip", sep="")
        ahn2_terrain_file_path <- paste(ahn2_terrain_directory, "/", "i", bladnrs[[t]], ".tif",sep="")
        if(!file.exists(ahn2_terrain_file_path)){
          ahn2_terrain_zip <- download.file(ahn2_terrain_downloadLink, destfile = ahn2_terrainZip_file_path)
          ahn2_terrain <- unzip(ahn2_terrainZip_file_path, overwrite = TRUE, exdir = ahn2_terrain_directory)
          file.remove(ahn2_terrainZip_file_path)
        }
      }
      indiv_terrain_rasters$filename <- paste(aws_name_trim, "_terrain_ahn2", '.tif', sep="")
      if(length(bladnrs) > 1){
        indiv_terrain_rasters$overwrite <- TRUE
        print("Merging all terrain rasters...")
        ahn2_terrain_raster <- do.call(merge, indiv_terrain_rasters)
        message("Merge terrain rasters complete.")
      } else if(length(bladnrs) == 1){
        ahn2_raw_raster <- indiv_raw_rasters[[1]]
      }
    }
  }
  
  if(raw_ahn2 == TRUE & terrain_ahn2 == TRUE){
    ahn2_rasters <- stack(ahn2_raw_raster, ahn2_terrain_raster)
  } else if(raw_ahn2 == TRUE & terrain_ahn2 == FALSE){
    ahn2_rasters <- stack(ahn2_raw_raster)
  } else if(raw_ahn2 == FALSE & terrain_ahn2 == TRUE){
    ahn2_rasters <- stack(ahn2_terrain_raster)
  } else {
    ahn2_rasters <- NULL
  }

return (ahn2_rasters)}

test_raster <- import_ahn2(aws_name = "De Bilt", station_coords = selected_aws_temp[["aws_rd.sp"]], resolution = 0.5, radius = 500, raw_ahn2 = TRUE, terrain_ahn2 = FALSE)
#test_ahn_raster <- import_ahn2(aws_name = "De Bilt", station_coords = point.sf, resolution = 0.5, radius = 2000, raw_ahn2 = TRUE, terrain_ahn2 = FALSE)


point.sp <- data.frame("X"=135000.000,"Y"=456250.000)
coordinates(point.sp) <- ~X+Y
crs(point.sp) <- CRS("+init=epsg:28992")
point.sf <- st_as_sf(point.sp)
