library(devtools)
library(data.table)
library(raster)
# AWS coordinates
AWS.df<-fread("data/coordinates/AWS_coordinates.csv")
#row.names(AWS.df) <- make.names(),TRUE)


#AWS.df$IT_DATETIME<-as.Date(AWS.df$IT_DTETIME,format="%Y%m%d_240000_000000")
devtools::use_data(AWS.df,overwrite=TRUE)
AWS_wgs.sp <-AWS.df



coordinates(AWS_wgs.sp) <-~LON+LAT
crs(AWS_wgs.sp) <-CRS("+init=epsg:4326")
AWS_rd.sp <- spTransform(AWS_wgs.sp, CRS = CRS("+init=epsg:28992"))
devtools::use_data(AWS_wgs.sp,overwrite=TRUE)
devtools::use_data(AWS_rd.sp,overwrite=TRUE)

#ahn2 de bilt
ahn2_deBilt<-stack("data/AHN2/deBilt/raw/r32cn1.tif")
proj4string(ahn2_deBilt)<-CRS("+init=epsg:28992") 
devtools::use_data(ahn2_deBilt,overwrite = TRUE)
# AWS.RD<-spTransform(AWS.sp,CRS("+init=epsg:28992"))

#CBS bodemkaart
cbs_bodem_gebruik.shp<-readOGR("data/cbs_bomdemGebruik", "BBG2012hoofdgroep")
devtools::use_data(cbs_bodem_gebruik.shp,overwrite = TRUE)

# library(lubridate)
# start<-as.Date("2010-01-01")
# stop<-as.Date("2010-12-31")
# time.seq<-seq(start,stop,by="day")
# yday(time.seq)
