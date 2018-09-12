#'Calculating the solar angles for AWS locations
#'
#'@title solar angles
#'@description iets over wat de functie doet 
#'@param lat latitude of the station in WGS84
#'@param lon longitude  of the station in WGS84
#'@param elv elevation
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
  scatterplot3d(sun_vector)
  sun_position <- sunpos(sun_vector)

  #columns LAT LON ELV JD SUNPOS SUNVECTOR
  df <- cbind(lat, lon,  elv, julian_day, sun_position, sun_vector)
  return(df)
}
juneday <- JD(seq(ISOdate(2012,6,21,0),ISOdate(2012,6,21,23,30),by='60 min'))
angles <- solar_angles(lon = aws_debilt_rd.sp@coords[,"lon"],
                       lat = aws_debilt_rd.sp@coords[,"lat"], elv= 42.70, julian_day = juneday)
plot(angles)

