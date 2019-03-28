#'Create buffer from point
#'
#'@title Create buffer
#'@description Create buffer in RD new format
#'@param point Required. sf point in RD New format
#'
#'@param distance Required. distance in metres
#'@param crs Optional. Default RD New projection string
#'@author Jelle Stuurman
#'@return buffer
createBuffer <- function(point, distance, crs = rgdal::CRSargs(CRS("+init=epsg:28992"))){
  buffer <- sf::st_buffer(point, dist=distance)
  sf::st_crs(buffer, crs)
  return (buffer)
}
