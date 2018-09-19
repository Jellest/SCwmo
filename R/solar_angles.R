library(scatterplot3d)
library(lubridate)
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
solar_angles<-function(lon, lat, elv, julian_day){
  requireNamespace("insol")
  

  
  sun_vector <- sunvector(julian_day,lat,lon,1)
  sun_position <- sunpos(sun_vector)
  summary(sun_position)
  
  
  df <- cbind(lat, lon,  elv, julian_day, sun_position)
  #df["altitude"] <- 90 - df["zenith"]
  return(df)
}
intervals <- JD(seq(ISOdate(2017,01,01,0),ISOdate(2017,12,31,23),by='60 min'))
angles <- solar_angles(lon = aws_debilt_wgs.sp@coords[,"lon"],
                       lat = aws_debilt_wgs.sp@coords[,"lat"],
                       elv= 42.70,
                       julian_day = intervals)
altitudeangle <- function(sunposition){
  altitude_angle <- 90 - sunposition["zenith"]
  sunposition["altitude"] <- altitude_angle
  return (sunposition)}

sapply(angles, altitudeangle)
plot()
