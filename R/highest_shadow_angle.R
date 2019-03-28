#'Correct highest possible shadow angle
#'
#'@title highest shadow angle
#'@description Correct highest possible shadow angle 
#'@param spatialpoint single sf point.
#'@param x single azimuth angle
#'@param shadow_angle_raw shadow angle
#'@param minutes.interval minutes interval
#'@author Jelle Stuurman
#'@return highest shadow angle value
highest_shadow_angle <- function(spatialpoint, AWS = FALSE,
                                 x,
                                 shadow_angle_raw,
                                 minutes.interval){
  my_solar_angles <- solar_angles(spatialpoint = spatialpoint, AWS = AWS,
                               day = 21,
                               month = 6,
                               year = 2018,
                               s_hour = 0,
                               f_hour = 23,
                               minutes.interval = minutes.interval)[["ah angles"]]
  
  june <- dplyr::select(my_solar_angles, azimuth, elevation)
  highest_shadow_angle <- stats::approx(x = june[,"azimuth"],
            y = june[,"elevation"],
            xout = x)$y
  if(shadow_angle_raw > highest_shadow_angle){
    shadow_angle <- highest_shadow_angle 
  } else {
    shadow_angle <- shadow_angle_raw
  }
return (shadow_angle)
}