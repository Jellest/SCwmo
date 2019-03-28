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