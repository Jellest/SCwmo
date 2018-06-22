#'Calculating the solar angles for AWS locations
#'
#'@title solar angles
#'@description iets over wat de functie doet 
#'@param lat latitude of the station in WGS84
#'@param lon longitude 
#'@param elv elevation
#'@param jd julian day
#'@author Jelle Stuurman
#'@examples
#'
#'data(AWS.df)
#'solar_angles(jd=seq(1,20,by=0.1)) #for the first 20 days with an accuracy of approximately 2.5 hours
#'@return dataframe with variables lat (latitude), lon (longitude), elv (elevation), jd (julian day), azimuth, zenith,... 
#'@export
solar_angles<-function(lat = 54.32556,
                       lon = 2.935833,
                       elv = 42.70,
                       jd = 3){
  requireNamespace("insol")

  sv <- sunvector(jd,lat,lon,1)
  sp <- sunpos(sv)

  #columns LAT LON ELV JD SUNPOS SUNVECTOR
  df <- cbind(lat, lon,  elv, jd, sp, sv)
  return(df)
}
