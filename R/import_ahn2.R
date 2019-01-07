import_ahn <- function(aws_name, station_coords, resolution, radius, raw_ahn, terrain_ahn, AHN3, remove_sheets){
  aws_name_trim <- getAWS_name_trim(aws_name)
  if(missing(resolution)){
    resolution_name <- "05m"
  } else {
    if(resolution == 0.5){
      resolution_name <- "05m"
    } else if(resolution == 5){
      resolution_name <- "5"
    }
  }
  
  if(missing(raw_ahn)){
    raw_ahn <- TRUE
  }
  if(missing(terrain_ahn)){
    terrain_ahn <- FALSE
  }
  
  if(missing(AHN3)){
    AHN <- "AHN2"
  } else if(AHN3 == TRUE){
    AHN  <- "AHN3"
  } else {
    AHN <- "AHN2"
  }
  
  if(missing(remove_sheets)){
    remove_sheets = FALSE
  }
  
  #create radius buffer arouond sensor
  point.sf <- st_as_sf(station_coords)
  surroundingBuffer <<- st_buffer(point.sf,dist=radius) #since RDcoords are in meters width is also in m
  surroundingBuffer.sp <<- buffer(station_coords, width=radius)
  #get BBOX extent of buffer area
  surroundingExtent <- extent(surroundingBuffer)
  my_EPSG <- "EPSG:28992"
  my_bbox <- paste(toString(surroundingExtent@xmin), toString(surroundingExtent@ymin), toString(surroundingExtent@xmax), toString(surroundingExtent@ymax), sep=",")
  
  #get AHN bladIndex
  ahn_directory <- paste("data", AHN, sep="/")
  if (!dir.exists(ahn_directory)){
    dir.create(ahn_directory)
  }
  bladIndex_shape_filepath <- paste(ahn_directory , sep="/")
  bladIndex_shape_file <- paste( bladIndex_shape_filepath, "/", "bladIndex", ".shp", sep="")
  print(bladIndex_shape_file)
  if(!file.exists(bladIndex_shape_file)){
    print("Download ahn wfs blad Index")
    ahn_WFS_baseUrl <- paste0("https://geodata.nationaalgeoregister.nl/", tolower(AHN), "/wfs?SERVICE=WFS&VERSION=1.0.0&REQUEST=GetFeature&TYPENAME=", tolower(AHN), ":", tolower(AHN), "_bladindex")
    print(ahn_WFS_baseUrl)
    ahn_wfs <- paste("WFS:", ahn_WFS_baseUrl, "&SRSNAME=", my_EPSG, sep="")
    ogr2ogr(src_datasource_name = ahn_wfs , dst_datasource_name = bladIndex_shape_file, layer = paste0(tolower(AHN),":", tolower(AHN), "_bladindex"), overwrite = TRUE)
  }
  #load intersected blad indexes
  bladIndex.shp <- readOGR(dsn = ahn_directory, layer = "bladIndex", stringsAsFactors=FALSE)
  p4s <- "+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.4171,50.3319,465.5524,-0.398957,0.343988,-1.8774,4.0725 +units=m +no_defs"
  bladIndex.shp <- spTransform(bladIndex.shp, p4s)
  bladIndex.sf <- st_as_sf(bladIndex.shp)

  st_agr(bladIndex.sf) <- "constant"
  st_agr(surroundingBuffer) <- "constant"
  bladIndex_buffer_intsct.sf <- st_intersection(bladIndex.sf, surroundingBuffer)
  
  bladnrs <<- bladIndex_buffer_intsct.sf$bladnr
  # if(AHN == "AHN3"){
  #   upperbladnrs <- list()
  #   for (b in 1:length(bladnrs)){
  #     upperbladnrs <- c(upperbladnrs, toupper(bladnrs[b]))
  #   }
  #   bladnrs <<- upperbladnrs
  # }
  #bladnrs_split <- strsplit(bladnrs, " ")
  #print(paste("bladnrs:",bladnrs_split[1]))
  #gml_ids <- bladIndex.shp$gml_id
  #View(gml_ids)
  #blad_count <- length(gml_ids)
  #gml_ids_split <- strsplit(gml_ids, " ")
  #print(gml_ids_split)
  
  dir.create(paste(ahn_directory, aws_name_trim, sep="/"), showWarnings = FALSE)
  aws_working_directory <- paste(ahn_directory, aws_name_trim, sep="/")
  
  ahn_atomFeed_BaseUrl <- paste("https://geodata.nationaalgeoregister.nl/", tolower(AHN), "/extract/", tolower(AHN), "_", sep="")
  #download raw ahn of corresponding resolultion
  indiv_raw_rasters <- list()
  raw_intersects <<- list()
  if(raw_ahn == TRUE){  
    ahn_raw_directory <- paste(aws_working_directory, "raw", sep="/")
    if(!file.exists(paste(ahn_raw_directory, "/", aws_name_trim, "_raw_ahn", '.tif', sep=""))){
      dir.create(paste(aws_working_directory, "raw", sep="/"), showWarnings = FALSE)
      print(ahn_raw_directory)
      rawXList <<- c()
      rawYList <<- c()
      paste("Amount of sheets found:", length(bladnrs), sep=" ")
      ahn_raw_file_paths <<- c()
      for(r in 1:length(bladnrs)){
        #ahn2: https://geodata.nationaalgeoregister.nl/ahn2/extract/ahn2_05m_ruw/r32cn1.tif.zip
        #ahn3: https://geodata.nationaalgeoregister.nl/ahn3/extract/ahn3_05m_dsm/R_32CN1.zip
        if(AHN == "AHN3"){
          tifZip = ".ZIP"
          ahn_raw_letter <- "R"
          ahn_raw_naming <- paste0("_dsm/", ahn_raw_letter,"_")
          ahn_raw_downloadLink <- paste(ahn_atomFeed_BaseUrl, resolution_name, ahn_raw_naming,  toupper(bladnrs[[r]]), tifZip, sep="")
          ahn_raw_file_path <- paste(ahn_raw_directory, "/r_", tolower(bladnrs[[r]]), ".tif",sep="")
        } else {
          tifZip = ".tif.zip"
          ahn_raw_letter <- "r"
          ahn_raw_naming <- paste0("_ruw/", ahn_raw_letter)
          ahn_raw_downloadLink <- paste(ahn_atomFeed_BaseUrl, resolution_name, ahn_raw_naming,  tolower(bladnrs[[r]]), tifZip, sep="")
          ahn_raw_file_path <- paste(ahn_raw_directory, "/", ahn_raw_letter, tolower(bladnrs[[r]]), ".tif",sep="")
        }

        print(ahn_raw_downloadLink)
        ahn_rawZip_file_path <- paste(ahn_raw_directory, "/", ahn_raw_letter, tolower(bladnrs[[r]]), ".zip", sep="")

        if(!file.exists(ahn_raw_file_path)){
          download.file(ahn_raw_downloadLink, destfile = ahn_rawZip_file_path)
          unzip(ahn_rawZip_file_path, overwrite = TRUE, exdir = ahn_raw_directory)
          file.remove(ahn_rawZip_file_path)
        }
        ahn_sheet_raw <-stack(ahn_raw_file_path)
       
        proj4string(ahn_sheet_raw)<-CRS("+init=epsg:28992") 
        ahn_raw_crop <- raster::crop(ahn_sheet_raw,surroundingBuffer)
        #plot(ahn_raw_crop)
        rXs <- c(xmin(ahn_raw_crop), xmax(ahn_raw_crop))
        rYs <- c(ymin(ahn_raw_crop), ymax(ahn_raw_crop))
        rawXList <<- rbind(rawXList, rXs)
        rawYList <<- rbind(rawYList, rYs)
        indiv_raw_rasters[[r]] <- ahn_raw_crop
      }
      
      indiv_raw_rasters$filename <- paste(aws_name_trim, "_raw_ahn", '.tif', sep="")
      if(length(bladnrs) > 1){
        indiv_raw_rasters$overwrite <- TRUE
        print("Merging all raw rasters...")
        ahn_raw_raster_filename <- paste(ahn_raw_directory, "/", aws_name_trim, "_raw_ahn", '.tif', sep="")
        print(ahn_raw_raster_filename)
        ahn_raw_raster <- do.call(merge, indiv_raw_rasters)
        writeRaster(ahn_raw_raster, ahn_raw_raster_filename, overwrite = TRUE)
        message("Merge raw rasters complete.")
        
        if(remove_sheets == TRUE){
          file.remove(ahn_raw_raster_filename)
        }
      } else if(length(bladnrs) == 1){
        ahn_raw_raster <- indiv_raw_rasters[[1]]
      }
    } else {
      print(paste("Cropped raw raster for", aws_name, "already exists.",sep =" "))
      ahn_raw_raster <- raster(paste(ahn_raw_directory, "/", aws_name_trim, "_raw_ahn", '.tif', sep=""))
    }
    
    
  }
  
  #download terrain ahn of corresponding resolution
  indiv_terrain_rasters <- list()
  if(terrain_ahn == TRUE){
    ahn_terrain_directory <- paste(aws_working_directory, "terrain", sep="/")
    terrainXList <<- c()
    terrainYList <<- c()
    if(!file.exists(paste(ahn_terrain_directory, "/", aws_name_trim, "_terrain_ahn", '.tif', sep=""))){
      dir.create(paste(aws_working_directory, "terrain", sep="/"), showWarnings = FALSE)
      print(ahn_terrain_directory)
      for(t in 1:length(bladnrs)){
        #AHN2: https://geodata.nationaalgeoregister.nl/ahn2/extract/ahn2_05m_int/i32cn1.tif.zip
        #AHN3: https://geodata.nationaalgeoregister.nl/ahn3/extract/ahn3_05m_dtm/M_32CN1.ZIP
        if(AHN == "AHN3"){
          tifZip = ".ZIP"
          ahn_terrain_letter <- "M"
          ahn_terrain_naming <- paste0("_dtm/", ahn_terrain_letter,"_")
          ahn_terrain_downloadLink <- paste(ahn_atomFeed_BaseUrl, resolution_name, ahn_terrain_naming,  toupper(bladnrs[[t]]), tifZip, sep="")
          ahn_terrain_file_path <- paste(ahn_terrain_directory, "/m_", tolower(bladnrs[[t]]), ".tif",sep="")
        } else {
          tifZip = ".tif.zip"
          ahn_terrain_letter <- "i"
          ahn_terrain_naming <- paste0("_int/", ahn_terrain_letter)
          ahn_terrain_downloadLink <- paste(ahn_atomFeed_BaseUrl, resolution_name, ahn_terrain_naming,  tolower(bladnrs[[t]]), tifZip, sep="")
          ahn_terrain_file_path <- paste(ahn_terrain_directory, "/", ahn_terrain_letter, tolower(bladnrs[[t]]), ".tif",sep="")
        }
        
        print(ahn_terrain_downloadLink)
        ahn_terrainZip_file_path <- paste(ahn_terrain_directory, "/", ahn_terrain_letter, bladnrs[[t]], ".zip", sep="")
        if(!file.exists(ahn_terrain_file_path)){
          ahn_terrain_zip <- download.file(ahn_terrain_downloadLink, destfile = ahn_terrainZip_file_path)
          ahn_terrain <- unzip(ahn_terrainZip_file_path, overwrite = TRUE, exdir = ahn_terrain_directory)
          file.remove(ahn_terrainZip_file_path)
        }
        ahn_sheet_terrain <-stack(ahn_terrain_file_path)
        proj4string(ahn_sheet_terrain)<-CRS("+init=epsg:28992") 
        ahn_terrain_crop<-raster::crop(ahn_sheet_terrain,surroundingBuffer)
        tXs <- c(xmin(ahn_terrain_crop), xmax(ahn_terrain_crop))
        tYs <- c(ymin(ahn_terrain_crop), ymax(ahn_terrain_crop))
        terrainXList <<- rbind(terrainXList, tXs)
        terrainYList <<- rbind(terrainYList, tYs)
        indiv_terrain_rasters[[t]] <- ahn_terrain_crop
      }
      
      indiv_terrain_rasters$filename <- paste(aws_name_trim, "_terrain_ahn", '.tif', sep="")
      if(length(bladnrs) > 1){
        indiv_terrain_rasters$overwrite <- TRUE
        print("Merging all terrain rasters...")
        ahn_terrain_raster_filename <- paste(ahn_terrain_directory, "/", aws_name_trim, "_terrain_ahn", '.tif', sep="")
        print(ahn_terrain_raster_filename)
        ahn_terrain_raster <- do.call(merge, indiv_terrain_rasters)
        writeRaster(ahn_terrain_raster, ahn_terrain_raster_filename, overwrite = TRUE)
        message("Merge terrain rasters complete.")
        
        if(remove_sheets == TRUE){
          file.remove(ahn_terrain_raster_filename)
        }
      } else if(length(bladnrs) == 1){
        ahn_terrain_raster <- indiv_terrain_rasters[[1]]
        if(remove_sheets == TRUE){
          
        }
      }
    } else {
      print(paste("Cropped terrain raster for", aws_name, "already exists.",sep =" "))
      ahn_terrain_raster <- raster(paste(ahn_terrain_directory, "/", aws_name_trim, "_terrain_ahn", '.tif', sep=""))
    }
  }
  
  if(raw_ahn == TRUE & terrain_ahn == TRUE){
    ahn_rasters <- stack(ahn_raw_raster, ahn_terrain_raster)
  } else if(raw_ahn == TRUE & terrain_ahn == FALSE){
    ahn_rasters <- stack(ahn_raw_raster)
  } else if(raw_ahn == FALSE & terrain_ahn == TRUE){
    ahn_rasters <- stack(ahn_terrain_raster)
  } else {
    ahn_rasters <- NULL
  }
  


return (ahn_rasters)}

test_raster <- import_ahn(aws_name = "De Bilt",
                          station_coords = test_point.sp,#station_coords = selected_aws_temp[["aws_rd.sp"]],
                          resolution = 0.5, radius = 500,
                          raw_ahn = TRUE, terrain_ahn = TRUE,
                          AHN3 = FALSE)
#test_ahn_raster <- import_ahn2(aws_name = "De Bilt", station_coords = point.sf, resolution = 0.5, radius = 2000, raw_ahn = TRUE, terrain_ahn = FALSE)


test_point.sp <- data.frame("X"=135000.000,"Y"=456250.000)
coordinates(test_point.sp) <- ~X+Y
crs(test_point.sp) <- CRS("+init=epsg:28992")
test_point.sf <- st_as_sf(test_point.sp)
