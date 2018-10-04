library(scatterplot3d)
library(lubridate)
library(insol)
library(ggplot2)
library(reshape)
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
solar_angles<-function(aws, sensor, lon, lat, altitude, julian_day){
  requireNamespace("insol")
  
  sun_vector <- sunvector(julian_day,lat,lon,0)
  sun_position <- sunpos(sun_vector)
  summary(sun_position)
  
  m <- cbind(lat, lon,  altitude, julian_day, sun_position)
  df <- data.frame(m)
  df$elevation <- 90 - df$zenith
  df$aws <- aws
  df$sensor <- sensor
  return(list("all_angles"= df))
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
  jd <- JD(seq(ISOdate(year,month,day,s_hour, 0),ISOdate(year,month,day,f_hour, 30),by='15 min'))
  return(jd)
}

month_all_solar_angles2 <- list()
all_solar_angles2 <-  data.frame(aws = character(0), sensor = character(0), lon = numeric(0), lat = numeric(0), altitude = numeric(0), julian_day = numeric(0), azimuth = numeric(0), zenith = numeric(0), elevation = numeric(0), stringsAsFactors = FALSE)


month_all_solar_angles2[[1]] <- data.frame(solar_angles(aws = "De Bilt",
                                                       sensor = "temp_150cm",
                                                       lon = aws_debilt_wgs.sp@coords[,"lon"],
                                                       lat = aws_debilt_wgs.sp@coords[,"lat"],
                                                       altitude = 42.70,
                                                       julian_day = julian_day_hour(2017, 12, 21))[["all_angles"]], stringsAsFactors = FALSE)

all_solar_angles2 <- rbind(all_solar_angles2, month_all_solar_angles2[[1]])
for (m in 2:7){
  month_all_solar_angles2[[m]] <- data.frame(solar_angles(aws = "De Bilt",
                                                         sensor = "temp_150cm",
                                                         lon = aws_debilt_wgs.sp@coords[,"lon"],
                                                         lat = aws_debilt_wgs.sp@coords[,"lat"],
                                                         altitude = 42.70,
                                                         julian_day = julian_day_hour(2017, m-1, 21))[["all_angles"]]) 
  all_solar_angles2 <- rbind(all_solar_angles2, month_all_solar_angles2[[m]])
}





plot(month_all_solar_angles[[1]]$azimuth, month_all_solar_angles[[1]]$elevation, type="l", xlim=c(0,360), ylim=c(0,70), xlab="azimuth", ylab="elevation")
for (p in 2:length(month_solar_angles)){
  par(new=T)
  plot(month_all_solar_angles[[p]]$azimuth, month_all_solar_angles[[p]]$elevation, type="l",  xlim=c(0,360), ylim=c(0,70), axes = FALSE, xlab="", ylab="")
}


plot(ah_sangles[[1]]$azimuth, month_ah_solar_angles[[1]]$elevation, type="l", xlim=c(0,360), ylim=c(0,70), xlab="azimuth", ylab="elevation")
for (p in 2:length(month_solar_angles)){
  par(new=T)
  plot(ah_sangles[[p]]$azimuth, month_ah_solar_angles[[p]]$elevation, type="l",  xlim=c(0,360), ylim=c(0,70), axes = FALSE, xlab="", ylab="")
}


ah_sangles <- subset(all_solar_angles, elevation >0)
all_solar_angles_melt <- reshape::melt(all_solar_angles, id = "julian_day")

sun_chart <- ggplot(shades, aes(x=azimuth, y=shadow_angle)) + geom_area() +
  geom_line(data = month_all_solar_angles2[[7]], aes(x=azimuth, y=elevation, colour="21 June")) +
  geom_line(data = month_all_solar_angles2[[6]], aes(x=azimuth, y=elevation, colour="21 May")) +
  geom_line(data = month_all_solar_angles2[[5]], aes(x=azimuth, y=elevation, colour="21 April")) +
  geom_line(data = month_all_solar_angles2[[4]], aes(x=azimuth, y=elevation, colour="21 March")) +
  geom_line(data = month_all_solar_angles2[[3]], aes(x=azimuth, y=elevation, colour="21 February")) +
  geom_line(data = month_all_solar_angles2[[2]], aes(x=azimuth, y=elevation, colour="21 January")) +
  geom_line(data = month_all_solar_angles2[[1]], aes(x=azimuth, y=elevation, colour="21 December")) +
  labs(x = "azimuth (degrees)", y = "elevation (degrees)") + xlim(50,310) + ylim(0,65) + theme_bw() + scale_colour_manual("", breaks = c("21 June", "21 May", "21 April", "21 March", "21 February", "21 January", "21 December"), values = c("21 December"="black", "21 January"="purple", "21 February"="orange", "21 March"="brown", "21 April"="green", "21 May"="yellow", "21 June"="red"))
sun_chart
ggsave("sun_chart.png")

