#' AWS coordinates 
#'#' Dataset contains lat lon of all the aws stations. 
#'
#' @format A data frame with 46 rows and 9 variables:
#' \describe{
#'   \item{IT_DATETIME}{price, in US dollars}
#'   \item{DS_CODE}{weight of the diamond, in carats}
#'   \item{DS_NAME}{weight of the diamond, in carats}
#'   \item{DS_DESC}{weight of the diamond, in carats}
#'   \item{DS_LAT}{weight of the diamond, in carats}
#'   \item{DS_LON}{weight of the diamond, in carats}
#'   \item{DS_ALT}{weight of the diamond, in carats}
#'   \item{REH1.TG}{weight of the diamond, in carats}
#'   \item{REH1.Q_TG}{weight of the diamond, in carats}
#' }
#' @examples 
#' data(AWS.df)
#' str(AWS.df)
#' summary(AWS.df)
#' 
"AWS.df"

#' spatial data
#' 
#' some desc
#' 
#' @slot DS_CODE iets
#' 
"AWS.sp"

#'AHN3 data for the Bilt
#'@description data can be downloaded from \url{https://www.pdok.nl/nl/ahn3-downloads}
#'
#'@slot r_32cn1 kaartblad number for the bilt with height elevation data
#'
"ahn3_deBilt"

#' coordinates sensors De Bilt 
#' Dataset contains x, y, lat lon of all the sensors at The Bilt.
#'  
#' @format A data frame with 11 rows and 5 variables:
#' \describe{
#'   \item{X}{RD X Coordinate, in meters}
#'   \item{Y}{RD Y coordinate, in meters}
#'   \item{LAT}{Latitude}
#'   \item{LON}{Longitude}
#'   \item{Sensor}{name of sensor}
#' }
#' @examples 
#' data(sensor_coordsDeBilt.df)
#' str(sensor_coordsDeBilt.df)
#' summary(sensor_coordsDeBilt.df)
"sensor_coordsDeBilt.df"


#' coordinates sensors De Bilt
#'
#' some description
#'
#' @slot Sensor
"sensor_coordsDeBilt.sp"