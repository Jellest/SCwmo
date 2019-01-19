
#sun chart
sun_shade_angles_chart <- function(data_path, aws_name,  AHN3 = FALSE){
  if(missing(aws_name)){
    aws_name <- ""
  }
  if(AHN3 == TRUE){
    AHN <- "AHN3"
  } else {
    AHN <- "AHN2"
  }
  
  aws_name_trim <- getAWS_name_trim(aws_name)
  
  data <- fread(data_path, data.table = FALSE)
  
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
  
  if("elevation" %in% colnames(data)){
    solar_angles <- TRUE
  } else {
    solar_angles <- FALSE
  }
  
  if("shadow_angle" %in% colnames(data)){
    shadow_angles <- TRUE
  } else {
    shadow_angles = FALSE
  }
  
  if(solar_angles == TRUE & shadow_angles == TRUE){
    chart <- ggplot(data, aes(x=azimuth, y=shadow_angle)) + geom_area() +
      geom_line(data = filter(data, day == "21-Jun"), aes(x=azimuth, y=elevation, colour="21 June")) +
      geom_line(data = filter(data, day == "21-May"), aes(x=azimuth, y=elevation, colour="21 May")) +
      geom_line(data = filter(data, day == "21-Apr"), aes(x=azimuth, y=elevation,  colour="21 April")) +
      geom_line(data = filter(data, day == "21-Mar"), aes(x=azimuth, y=elevation, colour="21 March")) +
      geom_line(data = filter(data, day == "21-Feb"), aes(x=azimuth, y=elevation, colour="21 February")) +
      geom_line(data = filter(data, day == "21-Jan"), aes(x=azimuth, y=elevation, colour="21 January")) +
      geom_line(data = filter(data, day == "21-Dec"), aes(x=azimuth, y=elevation, colour="21 December")) +
      labs(x = "azimuth (degrees)", y = "elevation (degrees)") +
      xlim(50,310) +
      ylim(0,65) +
      theme_bw() +
      theme(text = element_text(size=24)) +
      scale_colour_manual("", breaks = c("21 June", "21 May", "21 April", "21 March", "21 February", "21 January", "21 December"), values = c("21 December"="black", "21 January"="purple", "21 February"="orange", "21 March"="brown", "21 April"="green", "21 May"="yellow", "21 June"="red"))
    if(aws_name != ""){
      ggsave(paste0("output/solar_shadow_angles/", aws_name_trim,"/", aws_name_trim, "_", AHN, "_solar_shadow_chart.png"))
    } else {
      ggsave("output/solar_shadow_angles/solar_shadow_chart.png")
    }
    return(chart)
  }
}
