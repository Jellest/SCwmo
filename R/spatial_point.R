#'Create a sf spatial point from X/Y or LAT?LON coordinates
#'
#'@title spatial point
#'@description create a sf spatial point in RD New coordinates
#'@param X X coordidnate in RD New or WGS84 (LON)
#'@param Y Y coordidnate in RD New or WGS84 (LAT)
#'@param LONLAT Optional. Default FALSE. Set to TRUE if X and Y are in Longitude and Latitude format. Output will be in RD New format.
#'@author Jelle Stuurman
#'create_spatialPoint(X = , Y = , LAT LON = TRUE)
#'@return sf spatial point in RD New coordinates format

create_spatialPoint <- function(X, Y, LONLAT){
  if(missing(LONLAT)){
    LONLAT = FALSE
  }
  point.df <- data.frame(X = X, Y = Y, stringsAsFactors=FALSE)
  if(LONLAT == FALSE){
    projcrs <- rgdal::CRSargs(CRS("+init=epsg:28992"))
  } else {
    projcrs <- rgdal::CRSargs(CRS("+init=epsg:4326"))
  }
  point.sf <- sf::st_as_sf(x = point.df, coords = c('X', 'Y'), crs = projcrs)

  if(LONLAT == TRUE){
    point.sf <- st_transform(point.sf, rgdal::CRSargs(CRS("+init=epsg:28992")))
  }
  return (point.sf)
}

testpoint.sf <- data.frame(X = 135000.000, Y = 456250.000, stringsAsFactors=FALSE)
create_spatialPoint(X = 135000, Y = 456250, LONLAT = FALSE)
