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

solar_angles <- function(X, Y, day, month, year, s_hour, f_hour, minutes_interval, LONLAT){
  if(missing(LONLAT)){
    LONLAT = FALSE
  }
  if(LONLAT != FALSE & LONLAT != TRUE){
   stop("set LONLAT to TRUE or FALSE only. Set LATLON to TRUE if X and Y are longitude and lattitde coordinates.")
  }

  if(LONLAT == FALSE){
    point.sp <- data.frame("X"=X,"Y"=Y)
    coordinates(point.sp) <- ~X+Y
    crs(point.sp) <- CRS("+init=epsg:28992")
    point.sf <- st_as_sf(point.sp)
    wgs_point.sp <- spTransform(point.sp, CRS = CRS("+init=epsg:4238"))
    View(wgs_point.sp)
    lon = wgs_point.sp@coords[,"lon"]
    lat = wgs_point.sp@coords[,"lat"]
  } else if(LONLAT == TRUE){
    lon = X
    lat = Y
  }
  
  if(missing(minutes_interval)){
    minutes_int = "60 mins"
  } else {
    minutes_int = paste0(minutes_interval, " min")
  }
  
  get_solar_angles<-function(lon, lat, julian_day){
    
    requireNamespace("insol")
    
    sun_vector <- sunvector(jd = julian_day,
                            latitude = lat,
                            longitude = lon,
                            timezone = 1)
    
    sun_position <- sunpos(sun_vector)
    summary(sun_position)
    
    m <- cbind(lat, lon, julian_day, sun_position)
    df <- data.frame(m)
    df$elevation <- 90 - df$zenith
    # if(!missing(aws_name)){
    #   df$aws <- aws_name
    # }
    # if(!missing(sensor_name)){
    #   df$sensor <- sensor_name
    # }
    return(df)
  }
  
  # all_solar_angles <- data.frame(solar_angles(lon = aws_debilt_wgs.sp@coords[,"lon"],
  #                        lat = aws_debilt_wgs.sp@coords[,"lat"],
  #                        altitude = 42.70,
  #                        julian_day = intervals))
  # all_solar_angles$elevation <- 90 - all_solar_angles$zenith
  # above_horizon_solar_angles <- subset(all_solar_angles, zenith < 90)
  
  julian_day_hour <- function(year,month,day, s_hour, f_hour, minutes_interval){
    if(missing(s_hour)){
      s_hour <- 0
    }
    if(missing(f_hour)){
      f_hour <- 23
    }
    
    jd <- JD(seq(ISOdate(year,month,day,s_hour, 0),ISOdate(year,month,day,f_hour, 59),by=minutes_int))
    return(jd)
  }
  
  
  all_solar_angles <- data.frame(get_solar_angles(lon = lon,
                                              lat = lat,
                                              julian_day = julian_day_hour(year = year, month = month, day = day, minutes_interval = minutes_interval)), stringsAsFactors = FALSE)
  
  #select only above horizon elevation anngles
  ah_solar_angles <- subset(all_solar_angles, elevation > 0)
  
return(list("all angles"= all_solar_angles, "ah angles" = ah_solar_angles))
}

testangles <- solar_angles(X = 5.17939,
                               Y = 52.09886,
                               day = 21,
                               month = 12,
                               year = 2018,
                           minutes_interval = 30,
                               LONLAT = TRUE)

#all_solar_angles <- rbind(all_solar_angles2, month_all_solar_angles2[[1]])
  
# for (m in 2:7){
#   month_all_solar_angles[[m]] <- data.frame(solar_angles(aws = "De Bilt",
#                                                          sensor = "temp_150cm",
#                                                          lon = aws_debilt_wgs.sp@coords[,"lon"],
#                                                          lat = aws_debilt_wgs.sp@coords[,"lat"],
#                                                          julian_day = julian_day_hour(2017, m-1, 21))[["all_angles"]]) 
#   all_solar_angles <- rbind(all_solar_angles2, month_all_solar_angles2[[m]])
# }

