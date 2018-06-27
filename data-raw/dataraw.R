library(data.table)
library(raster)
# AWS coordinates
AWS.df<-fread("/nobackup/users/stuurman/data/AWScoords.csv")
AWS.df$IT_DATETIME<-as.Date(AWS.df$IT_DATETIME,format="%Y%m%d_240000_000000")

devtools::use_data(AWS.df,overwrite=TRUE)

AWS.sp<-AWS.df
coordinates(AWS.sp)<-~DS_LAT+DS_LON
crs(AWS.sp)<-CRS("+init=epsg:4326") #coordinaten kloppen wss niet of iets anders gaat mis
devtools::use_data(AWS.sp,overwrite=TRUE)

#De Bilt sensor coordinates
sensor_coordsDeBilt.df <- fread("/nobackup/users/stuurman/data/coordinates/coordinates_deBilt.csv")
devtools::use_data(sensor_coordsDeBilt.df,overwrite = TRUE)

sensor_coordsDeBilt.sp<-sensor_coordsDeBilt.df
coordinates(sensor_coordsDeBilt.sp)<-~X+Y
crs(sensor_coordsDeBilt.sp)<-CRS("+init=epsg:28992")
devtools::use_data(sensor_coordsDeBilt.sp,overwrite=TRUE)

#ahn3 de bilt
ahn3_deBilt<-stack("/nobackup/users/stuurman/data/r_32cn1.tif")
proj4string(ahn3_deBilt)<-CRS("+init=epsg:28992") 
devtools::use_data(ahn3_deBilt,overwrite = TRUE)
# AWS.RD<-spTransform(AWS.sp,CRS("+init=epsg:28992"))  #rgdal wil niet installeren op pc132090

# library(lubridate)
# start<-as.Date("2010-01-01")
# stop<-as.Date("2010-12-31")
# time.seq<-seq(start,stop,by="day")
# yday(time.seq)
