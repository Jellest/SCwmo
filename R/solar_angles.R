#'Calculating the solar angles for AWS locations
#'
#'@description iets over wat de functie doet 
#'@param lat latitude of the station in WGS84
#'@param lon longitude 
#'@param elv elevation
#'@param jd julian daysola
#'@param DS_CODE aws code
#'@author Jelle Stuurman
#'@examples
#'
#'data(AWS.df)
#' #solar_angles()
#'
#'@return
solar_angles<-function(DS_CODE = "201_H",
                       lat = 54.32556,
                       lon = 2.935833,
                       elv = 42.70,
                       jd = 3){
  requireNamespace("insol")
  library("insol")
  require("scatterplot3d")
  jd_interval <- seq(ISOdate(2010,6,21,0),ISOdate(2010,6,21,22,0),by='60 min')
  scatterplot3d(sunvector(jd_interval,54.32556,2.935833,42.70),
                xlim(-1,1),ylim=c(-1,1),zlim=c(0,1),pch=8,color='orange')
  
  sv <- sunvector(jd,lat,lon,1)
  sp <- sunpos(sv)
  #print(sp)
  
  #columns DS_CODE LAT LON ELV JD SUNPOS SUNVECTOR
  df <- data.frame(DS_CODE, lat, lon,  elv, jd, sp, sv)
  print(df)
  
  return(df)
}
solar_angles()
