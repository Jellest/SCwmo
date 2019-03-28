library(lubridate)
library(insol)
#'Calculating the solar angles at locations
#'
#'@title solar angles
#'@description Determine solarangles at the selected point for the determined year(s), moth(s) day(s) for the selected time interval.
#'@param spatialpoint single sf point.
#'@param LONLAT Logic. Dfeault FALSE. Set to TRUE if spatialpoint is in WGS84
#'@param day vector list. Days of the month to determine solar angles.
#'@param month numerical vector list. Numerical months of the year to determine solar angles.
#'@param year vector list. years to determine solar angles
#'@param s_hour Numeric. start hour to determine solar angles. dEFULT 0
#'@param f_hour Numeric. finish hour to determine solar angles. Default 23.
#'@param minutes.interval default 60. numerical minutes interval
#'@author Jelle Stuurman
#'@return dataframe with variables lat (latitude), lon (longitude), elv (elevation), jd (julian day), azimuth, zenith,... This includes all

solar_angles <- function(spatialpoint, AWS = FALSE, day, month, year, s_hour, f_hour, minutes.interval = 60){
  # if(LONLAT != FALSE & LONLAT != TRUE){
  #  stop("set LONLAT to TRUE or FALSE only. Set LATLON to TRUE if X and Y are longitude and lattitde coordinates.")
  # }
  #print(AWS)
  #View(spatialpoint)
  if(AWS == FALSE){
      #convert to WGS 84 and get coordinates
      spatialpoint.wgs <- sf::st_transform(spatialpoint, rgdal::CRSargs(CRS("+init=epsg:4326")))
      geom <- data.frame(sf::st_coordinates(spatialpoint.wgs))
      LON <- geom$X
      LAT = geom$Y
  } else {
      #get LONLAT coordinates from AWs list
      LON <- spatialpoint$LON
      LAT <- spatialpoint$LAT
  }
  #print(LON)

  minutes_int = paste0(minutes.interval, " min")

  get_solar_angles<-function(lon, lat, julian_day){

    #requireNamespace("insol")

    sun_vector <- insol::sunvector(jd = julian_day,
                            latitude = LAT,
                            longitude = LON,
                            timezone = 1)

    sun_position <- insol::sunpos(sun_vector)
    summary(sun_position)

    m <- cbind(LON, LAT, julian_day, sun_position)
    df <- data.frame(m)
    df$elevation <- 90 - df$zenith
    return(df)
  }

  julian_day_hour <- function(year,month,day, s_hour, f_hour, minutes.interval){
    #print(paste(day, month, year))
    if(missing(s_hour)){
      s_hour <- 0
    }
    if(missing(f_hour)){
      f_hour <- 23
    }

    jd <- insol::JD(base::seq(base::ISOdate(year,month,day,s_hour, 0),base::ISOdate(year,month,day,f_hour, 59),by=minutes.interval))
    return(jd)
  }

  julian_day <- julian_day_hour(year = year, month = month, day = day, s_hour, f_hour, minutes.interval = minutes_int)

  getDate <- function(day, month){
    if(month == 1){
      month_name = "Jan"
    }
    if(month == 2){
      month_name = "Feb"
    }
    if(month == 3){
      month_name = "Mar"
    }
    if(month == 4){
      month_name = "Apr"
    }
    if(month == 5){
      month_name = "May"
    }
    if(month == 6){
      month_name = "Jun"
    }
    if(month == 7){
      month_name = "Jul"
    }
    if(month == 8){
      month_name = "Aug"
    }
    if(month == 9){
      month_name = "Sep"
    }
    if(month == 10){
      month_name = "Oct"
    }
    if(month == 11){
      month_name = "Nov"
    }
    if(month == 12){
      month_name = "Dec"
    }
    date <- paste(day, month_name, sep="-")
    #print(date)
    return (date)
  }

  #print(paste(day, month))
  all_solar_angles <- data.frame(get_solar_angles(lon = LON,
                                              lat = LAT,
                                              julian_day = julian_day), day = getDate(day, month), stringsAsFactors = FALSE)

  #select only above horizon elevation anngles
  ah_solar_angles <- dplyr::filter(all_solar_angles, elevation > 0)

return(list("all angles"= all_solar_angles, "ah angles" = ah_solar_angles))
}
