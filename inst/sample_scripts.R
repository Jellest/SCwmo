### sample scripts

## bgt_import

# single features test script

# single_bgt_features <- data.frame(station = character(), feature_type = character(), has_features = logical(), features_count = numeric(), stringsAsFactors = FALSE)
# my_rowname <- paste(station, "pand", sep="_")
# 
# data_location <- data_locations[storage_settings + 1]
# ogrListLayers(bgt_wfs)
# bgt_single <- read_bgt(wfs = bgt_wfs, bgt_object_name = "bgt:pand", object_name_short = "pand", storage_location = data_location)
# single_count <- length(bgt_single)
# entry_test <- data.frame(station, "pand", TRUE, single_count, stringsAsFactors=FALSE)
# names(entry_test) <- c("station", "feature_type", "has_features", "features_count")
# rownames(entry_test) <- my_rowname
# single_bgt_features <- rbind(single_bgt_features, entry_test)


# create selection shape test

# shape_location_test <- paste(data_location, raw_folder , "pand", ".shp", sep="") 
# 
# dir.create(file.path(data_location, "selections"), showWarnings = FALSE)
# new_name <- paste(data_location, selection_folder , "pand_select", ".shp", sep="") 
# ogr2ogr(shape_location_test,
#         new_name,
#         layer = "pand",
#         where = "gml_id = 'pand.10632890'")
# 
# ogrinfo(shape_location_test,
#         layer = "pand",
#         where = "gml_id = 'pand.10632890'")


# select / remove columns data frame
# temperature_landuse_criteria.df <- AWS.df[c(1,5)]
# temperature_landuse_criteria.df <- temperature_landuse_criteria.df[-(1:10), ]


#import bgt
# bgt_shp_files <- list.files(paste("data", "BGT", aws_name, "raw", sep=folder_structure), pattern = ".shp")
# 
# names(bgt_list) <- bgt_objects_shortname_list
# pand_shape <- bgt_list[pand]
# crs(pand_shape)<- CRS("+init=epsg:28992")
# crs(pand_shape)
# crs(bgt_shape)
# 
# desk <- "C:/users/Jelle/Desktop"
# mshp<- "C:/users/Jelle/Desktop/test.shp"
# ogr2ogr(src_datasource_name = desk    , dst_datasource_name = mshp, layer = 'pand', overwrite = TRUE)


## create individual point
# point <- data.frame("X"=4,"Y"=6)
# coordinates(point) <- ~Y+X
# crs(point) <- CRS("+init=epsg:28992") 
