library(scatterplot3d)
library(lubridate)
library(insol)
#'Calculating the solar angles for AWS locations
#'
#'@title solar angles
#'@description iets over wat de functie doet 
#'@param lat latitude of the station in WGS84
#'@param lon longitude  of the station in WGS84
#'@param elv elevation of the measurement
#'@param jd julian day
#'@author Jelle Stuurman
#'@examples
#'
#'data(AWS.df)
#'solar_angles(jd=seq(1,20,by=0.1)) #for the first 20 days with an accuracy of approximately 2.5 hours
#'@return dataframe with variables lat (latitude), lon (longitude), elv (elevation), jd (julian day), azimuth, zenith,... 
#'@export
solar_angles<-function(lon, lat, altitude, julian_day){
  requireNamespace("insol")
  
  sun_vector <- sunvector(julian_day,lat,lon,1)
  sun_position <- sunpos(sun_vector)
  summary(sun_position)
  
  m <- cbind(lat, lon,  altitude, julian_day, sun_position)
  df <- data.frame(m)
  df$elevation <- 90 - df$zenith
  df_ah <- subset(df, zenith < 90)
  
  return(list(df, df_ah))
}

# all_solar_angles <- data.frame(solar_angles(lon = aws_debilt_wgs.sp@coords[,"lon"],
#                        lat = aws_debilt_wgs.sp@coords[,"lat"],
#                        altitude = 42.70,
#                        julian_day = intervals))
# all_solar_angles$elevation <- 90 - all_solar_angles$zenith
# above_horizon_solar_angles <- subset(all_solar_angles, zenith < 90)

julian_day_hour <- function(year,month,day, s_hour, f_hour){
  if(missing(s_hour)){
    s_hour <- 0
  }
  if(missing(f_hour)){
    f_hour <- 23
  }
  jd <- JD(seq(ISOdate(year,month,day,s_hour),ISOdate(year,month,day,f_hour),by='60 min'))
  return(jd)
}

month_solar_angles <- list()
for (m in 1:6){
  month_solar_angles[[m]] <- data.frame(solar_angles(lon = aws_debilt_wgs.sp@coords[,"lon"],
                                                     lat = aws_debilt_wgs.sp@coords[,"lat"],
                                                     altitude = 42.70,
                                                     julian_day = julian_day_hour(2017, m, 21))[1]) 
}
month_solar_angles[[7]] <- data.frame(solar_angles(lon = aws_debilt_wgs.sp@coords[,"lon"],
                                                   lat = aws_debilt_wgs.sp@coords[,"lat"],
                                                   altitude = 42.70,
                                                   julian_day = julian_day_hour(2017, 12, 21))[1])

plot(month_solar_angles[[1]]$azimuth, month_solar_angles[[1]]$elevation, type="l", xlim=c(0,360), ylim=c(0,70), xlab="azimuth", ylab="elevation")
for (p in 2:length(month_solar_angles)){
  par(new=T)
  plot(month_solar_angles[[p]]$azimuth, month_solar_angles[[p]]$elevation, type="l",  xlim=c(0,360), ylim=c(0,70), axes = FALSE, xlab="", ylab="")
}