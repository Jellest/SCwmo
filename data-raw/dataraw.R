library(data.table)
library(raster)

AWS.df<-fread("/nobackup/users/stuurman/data/AWScoords.csv")
AWS.df$IT_DATETIME<-as.Date(AWS.df$IT_DATETIME,format="%Y%m%d_240000_000000")

devtools::use_data(AWS.df,overwrite=TRUE)

AWS.sp<-AWS.df
coordinates(AWS.sp)<-~DS_LAT+DS_LON
crs(AWS.sp)<-CRS("+init=epsg:4326") #coordinaten kloppen wss niet of iets anders gaat mis

devtools::use_data(AWS.sp,overwrite=TRUE)

# AWS.RD<-spTransform(AWS.sp,CRS("+init=epsg:28992"))  #rgdal wil niet installeren op pc132090

# library(lubridate)
# start<-as.Date("2010-01-01")
# stop<-as.Date("2010-12-31")
# time.seq<-seq(start,stop,by="day")
# yday(time.seq)
