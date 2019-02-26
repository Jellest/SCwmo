temp <- function(){
  # class_count <- fread("output/class_counts.csv", data.table = FALSE)[,1:3]
  #  rownames(class_count) <- c("class 1 count" ,
  #                           "class 2 count",
  #                             "class 3 count",
  #                             "class 4 count",
  #                             "class 5 count",
  #                             "shades count",
  #                             "objects count",
  #                           "shadesObjects count")
  #  class_count <- cbind(c("Class 1" ,
  #                         "Class 2",
  #                         "Class 3",
  #                         "Class 4",
  #                         "Class 5",
  #                         "shades count",
  #                         "objects count",
  #                       "sh  adesObjects count"), class_count)
  # colnames(class_count) <- c("Class", "SC Class", "Count", "Relative", "Total", "Both", "Shades", "Heat_sources", "Vegetation")
  # 
  # class_count <- class_count[1:5,]
  # 
  # View(class_count)
  # 
  # shades_distr <- fread("output/all_shades_distribution.csv", data.table = FALSE)
  # rownames(shades_distr) <- AWS_temperature_names
  # View(shades_distr)
  # 
  # theme_set(theme_bw())
  # my_plot <- ggplot(class_count, aes(x=Class, y=Count)) + 
  #   geom_bar(stat="identity", width=.8, fill="#6C8EBF") + 
  #   #labs(title="Overview ASC SC Class values", 
  #        #subtitle="All 34 AWS") + 
  #   ylim(0,25) +
  #   xlab("Final SC") + ylab("Number of AWS sites") +
  #   theme(axis.text.x = element_text(face = "bold", size=12),
  #         axis.text.y = element_text(face = "bold", size=12),
  #         axis.title.x = element_text(colour = "#6C8EBF", face = "bold", size =14),
  #         axis.title.y = element_text(colour = "#6C8EBF", face = "bold", size =14),
  #         panel.grid.major.y = element_line(size = 1, colour="#DAE8FC"),
  #         panel.grid.minor.y = element_line(colour="#DAE8FC"),
  #         panel.grid.major.x = element_blank(),
  #         panel.border = element_blank())
  # 
  # 
  # 
  data <- fread("output/class_counts_ext.csv", data.table = FALSE)[,1:3]
  #View(data)
  specific_plot <- ggplot(data, aes(factor(SC_Class), Count, fill = factor(Criteria, levels = c("total", "shadow & heat sources", "shadow", "heat sources", "vegetation height")))) +
    geom_bar(stat = "identity", position = position_dodge(width = 1))) +
    ggtitle("Default color comparison") +
    scale_fill_manual(values = c("#6C8EBF", "red", "gray", "yellow","purple")) +
    ylim(0,25) +
    xlab("Final SC") + ylab("Number of AWS sites") +
    theme(axis.text.x = element_text(face = "bold", size=12),
          axis.text.y = element_text(face = "bold", size=12),
          axis.title.x = element_text(colour = "#6C8EBF", face = "bold", size =14),
          axis.title.y = element_text(colour = "#6C8EBF", face = "bold", size =14),
          panel.grid.major.y = element_line(size = 1, colour="#DAE8FC"),
          panel.grid.minor.y = element_line(colour="#DAE8FC"),
          panel.grid.major.x = element_blank(),
          panel.border = element_blank())
  
  specific_plot
}

temp()

results <- fread()
