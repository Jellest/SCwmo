#sun shadow chart
sun_shade_angles_chart <- function(aws.df = AWS.df, data_path, aws_name,  angle_selection_byIndexNr = "all", AHN3 = FALSE, extract_method = 'bilinear'){
  if(missing(aws_name)){
    aws_name <- ""
  }
  if(AHN3 == TRUE){
    AHN <- "AHN3"
  } else {
    AHN <- "AHN2"
  }
  
  aws_name_trim <- getAWS_name_trim(aws.df = aws.df, aws_name = aws_name)
  
  data_exists <- file.exists(data_path)
  if(angle_selection_byIndexNr == "all"){
    data <- fread(data_path, data.table = FALSE)[]
  } else {
    data <- fread(data_path, data.table = FALSE)[angle_selection_byIndexNr]
  }
  #View(data)
  
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
    shadow_angles <- FALSE
  }
  
  if((solar_angles == TRUE & shadow_angles == TRUE) | data_exists == TRUE){
    #add shadow chart, layout, axes and labels
    chart <- ggplot(data, aes(x=azimuth, y=shadow_angle)) + geom_area() +
      ggtitle(paste("Sun and shadow chart for", aws_name)) +
      #labs(fill = "Your Title") +
      #labs(x = "azimuth (degrees)", y = "elevation (degrees)") +
      #xlim(48,312) +
      scale_x_continuous(name="azimuth (degrees)", limits=c(50, 310), breaks=seq(50,310,50)) +
      scale_y_continuous(name="elevation (degrees)", limits=c(0, 65), breaks=seq(0,65,10)) +
      #ylim(0,65) +# scale_y_continuous(breaks=seq(0,70,10)) +
      theme_bw() +
      theme(text = element_text(size=12))
      #scale_colour_manual("", breaks = c("21 June", "21 May", "21 April", "21 March", "21 February", "21 January", "21 December"), values = c("21 December"="black", "21 January"="purple", "21 February"="orange", "21 March"="brown", "21 April"="green", "21 May"="yellow", "21 June"="red"))
    
    breaks_list <- c()
    values_list <- c()
    
    #add present months
    if("21-Jun" %in% data$day == TRUE){
      chart <- chart + geom_line(data = filter(data, day == "21-Jun"), aes(x=azimuth, y=elevation, colour="21 June")) 
      breaks_list <- cbind(breaks_list, "21 June")
      values_list <- cbind(values_list, "21 June"="red")
    }
    if("21-May" %in% data$day == TRUE){
      chart <- chart + geom_line(data = filter(data, day == "21-May"), aes(x=azimuth, y=elevation, colour="21 May")) 
      breaks_list <- cbind(breaks_list, "21 May")
      values_list <- cbind(values_list, "21 May"="yellow")
    }
    if("21-Apr" %in% data$day == TRUE){
      chart <- chart + geom_line(data = filter(data, day == "21-Apr"), aes(x=azimuth, y=elevation,  colour="21 April"))
      breaks_list <- cbind(breaks_list, "21 April")
      values_list <- cbind(values_list, "21 April"="green")
    }
    if("21-Mar" %in% data$day == TRUE){
      chart <- chart + geom_line(data = filter(data, day == "21-Mar"), aes(x=azimuth, y=elevation, colour="21 March"))
      breaks_list <- cbind(breaks_list, "21 March")
      values_list <- cbind(values_list, "21 March"="brown")
    } 
    if("21-Feb" %in% data$day == TRUE){
      chart <- chart + geom_line(data = filter(data, day == "21-Feb"), aes(x=azimuth, y=elevation, colour="21 February"))
      breaks_list <- cbind(breaks_list, "21 February")
      values_list <- cbind(values_list, "21 February"="orange")
    } 
    if("21-Jan" %in% data$day == TRUE){
      chart <- chart + geom_line(data = filter(data, day == "21-Jan"), aes(x=azimuth, y=elevation, colour="21 January"))
      breaks_list <- cbind(breaks_list, "21 January")
      values_list <- cbind(values_list, "21 January"="purple")
    }
    if("21-Dec" %in% data$day == TRUE){
      chart <- chart + geom_line(data = filter(data, day == "21-Dec"), aes(x=azimuth, y=elevation, colour="21 December"))
      breaks_list <- cbind(breaks_list, "21 December")
      values_list <- cbind(values_list, "21 December"="black")
    }  
    
    #add legend
    chart <- chart + scale_colour_manual("", breaks = breaks_list, values = values_list)
    
    if(aws_name != ""){
      ggsave(paste0("output/solar_shadow_angles/", aws_name_trim,"/", aws_name_trim, "_", AHN, "_", extract_method, "_solar_shadow_chart.png"))
    } else {
      ggsave(paste0("output/solar_shadow_angles/", AHN, "_", extract_method, "_solar_shadow_chart.png"))
    }
    
    return(chart)
  }
}

sun_shade_angles_chart(data_path = "output/solar_shadow_angles/DeBilt/DeBilt_AHN2_ah_solar_shadow_angles_complete.csv", aws_name = "De Bilt",  AHN3 = FALSE, extract_method = 'bilinear')
