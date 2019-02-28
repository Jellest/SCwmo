#sun shadow chart
sun_shade_angles_chart <- function(aws.df = AWS.df, data_path, aws_name,  addition = "", angle_selection_byIndexNr = "all", AHN3 = FALSE, extract_method = 'bilinear'){
  if(missing(aws_name)){
    aws_name <- ""
  }
  if(AHN3 == TRUE){
    AHN <- "AHN3"
  } else {
    AHN <- "AHN2"
  }
  
  aws_name_trim <- getAWS_name_trim(aws.df = aws.df, aws_name = aws_name, addition = addition)
  
  data_exists <- file.exists(data_path)
  if(angle_selection_byIndexNr == "all"){
    data <- fread(data_path, data.table = FALSE)
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
      #ggtitle(paste("Sun and shadow chart for", aws_name_trim)) +
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
      values_list <- cbind(values_list, "21 June"="yellow")
    }
    if("21-May" %in% data$day == TRUE){
      chart <- chart + geom_line(data = filter(data, day == "21-May"), aes(x=azimuth, y=elevation, colour="21 May")) 
      breaks_list <- cbind(breaks_list, "21 May")
      values_list <- cbind(values_list, "21 May"="black")
    }
    if("21-Apr" %in% data$day == TRUE){
      chart <- chart + geom_line(data = filter(data, day == "21-Apr"), aes(x=azimuth, y=elevation,  colour="21 April"))
      breaks_list <- cbind(breaks_list, "21 April")
      values_list <- cbind(values_list, "21 April"="blue")
    }
    if("21-Mar" %in% data$day == TRUE){
      chart <- chart + geom_line(data = filter(data, day == "21-Mar"), aes(x=azimuth, y=elevation, colour="21 March"))
      breaks_list <- cbind(breaks_list, "21 March")
      values_list <- cbind(values_list, "21 March"="purple")
    } 
    if("21-Feb" %in% data$day == TRUE){
      chart <- chart + geom_line(data = filter(data, day == "21-Feb"), aes(x=azimuth, y=elevation, colour="21 February"))
      breaks_list <- cbind(breaks_list, "21 February")
      values_list <- cbind(values_list, "21 February"="red")
    } 
    if("21-Jan" %in% data$day == TRUE){
      chart <- chart + geom_line(data = filter(data, day == "21-Jan"), aes(x=azimuth, y=elevation, colour="21 January"))
      breaks_list <- cbind(breaks_list, "21 January")
      values_list <- cbind(values_list, "21 January"="green")
    }
    if("21-Dec" %in% data$day == TRUE){
      chart <- chart + geom_line(data = filter(data, day == "21-Dec"), aes(x=azimuth, y=elevation, colour="21 December"))
      breaks_list <- cbind(breaks_list, "21 December")
      values_list <- cbind(values_list, "21 December"="orange")
    }  
    
    #add legend
    chart <- chart + scale_colour_manual("", breaks = breaks_list, values = values_list)
    
    if(aws_name != ""){
      ggsave(paste0("output/", aws_name_trim, "/solar_shadow_angles/", aws_name_trim, "_", AHN, "_", extract_method, "_solar_shadow_chart.png"))
      #ggsave("test.png")
    } else {
      #ggsave(paste0("output/solar_shadow_angles/", AHN, "_", extract_method, "_solar_shadow_chart.png"))
      #ggsave("test.png")
    }
    
    return(chart)
  }
}

plot(sun_shade_angles_chart(data_path = "DeBilt_AHN3_ah_solar_shadow_angles.csv", aws_name = "De Bilt", addition = "", AHN3 = TRUE, extract_method = 'bilinear'))



class_differences <- function(data_path){
  data <- fread(data_path, data.table = FALSE)

  #rownames(data) <- AWS_temperature_names
  #selected_data <- select(data, AWS, AHN_selected, exact_match, partial_match, automated_class_is, class_difference)
  #View(selected_data)
  colnames(data) <- c("Difference", "Count")
  #View(data)
  #mycols <- c("#cb181d", "#fee0d2", "#fcae91", "#fb6a4a", "#238b45")
  
  data <- data.frame(c(-3, -2, -1, 0, 1, 2, 3),c("-3", "-2", "-1", "0", "1", "2", "3"), c(0,1,0,19,4,5,5))
  colnames(data) <- c("Difference", "Difference_str", "Count")
  bar <- ggplot(data, aes(x = Difference_str, y = Count)) +
    geom_bar(stat="identity", width = 0.8, fill="#6C8EBF") +
    #coord_polar(theta = "y", start = 0) +
    #scale_fill_manual(values = mycols) +

    ylim(0,20) +
    xlab("Class Difference") +
    ylab("Number of AWS sites") +
    #scale_x_discrete(labels = unlist(data$Difference_str)) +
    #xlim(-3,3) +
    theme(axis.text.x = element_text(face = "bold", size=12),
         axis.text.y = element_text(face = "bold", size=12),
         axis.title.x = element_text(colour = "#6C8EBF", face = "bold", size =14),
         axis.title.y = element_text(colour = "#6C8EBF", face = "bold", size =14),
         panel.grid.major.y = element_line(size = 1, colour="#DAE8FC"),
         panel.grid.minor.y = element_line(colour="#DAE8FC"),
         panel.grid.major.x = element_blank(),
         panel.grid.minor.x = element_blank(),
         panel.border = element_blank())
    #geom_text(aes(y = lab.ypos, label = ), color = "white") +
  #scale_fill_brewer(palette = "Reds")
  bar
  ggsave("class_differences.png", plot = last_plot())
  ggsave(filename = "C:/Users/3691233/Dropbox/Apps/ShareLaTeX/Automated Temperature SC  MSc Thesis/Figures/Results/class_differences.png", plot = last_plot())
  
}

class_differences(data_path = "output/class_differences.csv")

bar_plot <- function(){
  data <- fread("output/class_counts_ext.csv", data.table = FALSE)[,1:4]
  my_plot <- ggplot(data, aes(factor(Criteria_abr, levels = c("Total", "Sh & HeSo", "Sh", "HeSo", "VH")), Count, fill = factor(SC_Class, levels = c("Class 1", "Class 2", "Class 3", "Class 4", "Class 5")))) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_manual(values = c("#6C8EBF", "yellow", "purple", "orange", "red")) +
    labs(fill = "") +
    ylim(0,25) +
    xlab("Deterimant SC Criteria") + ylab("number of AWS sites") +
    theme(legend.position = c(1,1), legend.justification = c(1,1),
          axis.text.x = element_text(face = "bold", size=14),
          axis.text.y = element_text(face = "bold", size=14),
          axis.title.x = element_text(colour = "#6C8EBF", face = "bold", size =16),
          axis.title.y = element_text(colour = "#6C8EBF", face = "bold", size =16),
          panel.grid.major.y = element_line(size = 1, colour="#DAE8FC"),
          panel.grid.minor.y = element_line(colour="#DAE8FC"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_line(size = 1, colour="#DAE8FC"),
          plot.background = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          legend.text = element_text(size=12, face="bold")) +
    geom_vline(xintercept = c(1.5), colour="#6C8EBF", size = 0.6) +
    geom_vline(xintercept = c(2.5, 3.5, 4.5), colour="#6C8EBF", size=0.2)
    #geom_col(position="dodge")
  
  my_plot
  ggsave(filename = "overviewClasses.png", plot = last_plot())
  ggsave(filename = "//nobackup/users/stuurman/Dropbox/Apps/ShareLaTeX/Automated Temperature SC  MSc Thesis/Figures/Results/overviewClasses.png", plot = last_plot())
}

bar_plot()

