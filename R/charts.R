
#sun chart
sun_shade_angles_chart <- function(solar_angles, shadow_angles){
  # plot(month_all_solar_angles[[1]]$azimuth, month_all_solar_angles[[1]]$elevation, type="l", xlim=c(0,360), ylim=c(0,70), xlab="azimuth", ylab="elevation")
  # for (p in 2:length(month_solar_angles)){
  #   par(new=T)
  #   plot(month_all_solar_angles[[p]]$azimuth, month_all_solar_angles[[p]]$elevation, type="l",  xlim=c(0,360), ylim=c(0,70), axes = FALSE, xlab="", ylab="")
  # }
  # 
  # 
  # plot(ah_sangles[[1]]$azimuth, month_ah_solar_angles[[1]]$elevation, type="l", xlim=c(0,360), ylim=c(0,70), xlab="azimuth", ylab="elevation")
  # for (p in 2:length(month_solar_angles)){
  #   par(new=T)
  #   plot(ah_sangles[[p]]$azimuth, month_ah_solar_angles[[p]]$elevation, type="l",  xlim=c(0,360), ylim=c(0,70), axes = FALSE, xlab="", ylab="")
  # }
  if(!missing(solar_angles) & !missing(shadow_angles)){
    chart <- ggplot(shadow_angles, aes(x=azimuth, y=shadow_angle)) + geom_area() +
      geom_line(data = solar_angles[[7]][["ah angles"]], aes(x=azimuth, y=elevation, colour="21 June")) +
      geom_line(data = solar_angles[[6]][["ah angles"]], aes(x=azimuth, y=elevation, colour="21 May")) +
      geom_line(data = solar_angles[[5]][["ah angles"]], aes(x=azimuth, y=elevation, colour="21 April")) +
      geom_line(data = solar_angles[[4]][["ah angles"]], aes(x=azimuth, y=elevation, colour="21 March")) +
      geom_line(data = solar_angles[[3]][["ah angles"]], aes(x=azimuth, y=elevation, colour="21 February")) +
      geom_line(data = solar_angles[[2]][["ah angles"]], aes(x=azimuth, y=elevation, colour="21 January")) +
      geom_line(data = solar_angles[[1]][["ah angles"]], aes(x=azimuth, y=elevation, colour="21 December")) +
      labs(x = "azimuth (degrees)", y = "elevation (degrees)") +
      xlim(50,310) +
      ylim(0,65) +
      theme_bw() +
      theme(text = element_text(size=24)) +
      scale_colour_manual("", breaks = c("21 June", "21 May", "21 April", "21 March", "21 February", "21 January", "21 December"), values = c("21 December"="black", "21 January"="purple", "21 February"="orange", "21 March"="brown", "21 April"="green", "21 May"="yellow", "21 June"="red"))
    print(chart)  
    #ggsave("sun_chart.png")
    return(chart)
  }
}
