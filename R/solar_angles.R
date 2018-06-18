#'Calculating the solar angles for AWS locations
#'
#'@param lat latitude of the station in WGS84
#'@param lon longitude 
#'@param elv elevation
#'@param jd julian day
#'
#'@examples
#'
#'data(AWS.df)
#' #solar_angles()
#'
#'@return
solar_angles<-function(lat,lon,elv,jd){
  requireNamespace("insol")
  
  #sunpos()
  #sunvector()
  
  #df
  #columns DS_CODE LAT LON ELV JD SUNPOS SUNVECTOR
  return(df)
  
}