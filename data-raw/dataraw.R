library(data.table)
library(raster)

AWS.df<-fread("/nobackup/users/stuurman/data/AWScoords.csv")
AWS.df$IT_DATETIME<-as.Date(AWS.df$IT_DATETIME,format="%Y%m%d_240000_000000")

devtools::use_data(AWS.df,overwrite=TRUE)

AWS.sp<-AWS.df
coordinates(AWS.sp)<-~DS_LAT+DS_LON
  
# library(lubridate)
# start<-as.Date("2010-01-01")
# stop<-as.Date("2010-12-31")
# time.seq<-seq(start,stop,by="day")
# yday(time.seq)
