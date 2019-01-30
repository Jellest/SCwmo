highest_shadow_angle <- function(aws,
                                 aws.df = AWS.df,
                                 aws_name,
                                 sensor_name,
                                 x,
                                 shadow_angle_raw
                                ){
  print("checking and corercting angle")
  #aws <- select_single_aws(aws.df = aws.df, aws_name = aws_name, sensor_name = sensor_name)
  solar_angles <- solar_angles(X = aws[["aws.df"]][1,"LON"],
                               Y = aws[["aws.df"]][1,"LAT"],
                               year = 2018,
                               month = 6,
                               day = 21,
                               s_hour = 0,
                               f_hour = 23,
                               minutes_interval = 15,
                               LONLAT = TRUE
                               )
  june <- select(solar_angles[["all angles"]], azimuth, elevation)
  #plot(june)
  highest_shadow_angle <- approx(x = june[,"azimuth"],
            y = june[,"elevation"],
            xout = x)$y
  if(shadow_angle_raw > highest_shadow_angle){
    shadow_angle <- highest_shadow_angle 
  } else {
    shadow_angle <- shadow_angle_raw
  }
return (shadow_angle)}

