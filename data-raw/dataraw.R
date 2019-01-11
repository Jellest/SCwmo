library(devtools)
library(data.table)
library(raster)
# AWS coordinates
AWS.df<-fread("data/coordinates/AWS_coordinates.csv", data.table = FALSE)
T#row.names(AWS.df) <- make.names(),TRUE)


#AWS.df$IT_DATETIME<-as.Date(AWS.df$IT_DTETIME,format="%Y%m%d_240000_000000")
devtools::use_data(AWS.df,overwrite=TRUE)
AWS_wgs.sp <-AWS.df

coordinates(AWS_wgs.sp) <-~LON+LAT
crs(AWS_wgs.sp) <-CRS("+init=epsg:4326")
AWS_rd.sp <- spTransform(AWS_wgs.sp, CRS = CRS(epsg_rd))
devtools::use_data(AWS_wgs.sp,overwrite=TRUE)
devtools::use_data(AWS_rd.sp,overwrite=TRUE)

#ahn2 de bilt
#raw
ahn2_deBilt_sheet_raw<-stack("data/AHN2/deBilt/raw/r32cn1.tif")
proj4string(ahn2_deBilt_sheet_raw)<-CRS(epsg_rd) 
devtools::use_data(ahn2_deBilt_raw,overwrite = TRUE)
#terrain
ahn2_deBilt_sheet_terrain<-stack("data/AHN2/deBilt/terrain/i32cn1.tif")
proj4string(ahn2_deBilt_sheet_terrain)<-CRS(epsg_rd) 

#combine layers
ahn2_DeBilt_sheet <- stack(ahn2_deBilt_sheet_raw, ahn2_deBilt_sheet_terrain)

#LGN7
lgn7 <- stack("data/LGN7/LGN7.tif") 
proj4string(lgn7)<-CRS(epsg_rd) 
lgn7_codes <- fread("data/LGN7/LGN7_codes.csv")
# AWS.RD<-spTransform(AWS.sp,CRS(epsg_rd))

#CBS bodemkaart
cbs_bodem_gebruik.shp<-readOGR("data/cbs_bomdemGebruik", "BBG2012hoofdgroep")
devtools::use_data(cbs_bodem_gebruik.shp,overwrite = TRUE)

# library(lubridate)
# start<-as.Date("2010-01-01")
# stop<-as.Date("2010-12-31")
# time.seq<-seq(start,stop,by="day")
# yday(time.seq)

#guieline criteria
guideline_criteria <- fread("wmoSC/data-raw/guideline_criteria.csv")
