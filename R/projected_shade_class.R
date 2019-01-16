projected_shade_class <- function(data_path, cv_colName, aws_name){
  if(missing(aws_name)){
    aws_name <- ""
  }
  aws_name_trim <- getAWS_name_trim(aws_name)
  
  data <- fread(data_path)
  
  tshac <- dplyr::filter(guideline_criteria, Variable == "temperature" & Criteria_Type == "shading")
  
  if(missing(cv_colName)){
    cv_colName <- "Criteria_Value"
  }
  
  class_1_filter <-  filter(tshac, Class == "1")
  class_1_cv <- as.numeric(class_1_filter[,cv_colName][1])
  
  class_2_filter <-  filter(tshac, Class == "2")
  class_2_cv <- as.numeric(class_2_filter[,cv_colName][1])
  
  class_3_filter <-  filter(tshac, Class == "3")
  class_3_cv <- as.numeric(class_3_filter[,cv_colName][1])
  
  class_2_3_cv <- as.numeric(class_2_filter[,cv_colName][1]) 
  
  class_4_filter <-  filter(tshac, Class == "4")
  class_4_cv <- as.numeric(class_4_filter[,cv_colName][1])
  
  class_5_filter <-  filter(tshac, Class == "5")
  class_5_cv <- as.numeric(class_5_filter[,cv_colName][1])
  
  # class_1_cv <- 2
  # class_2_cv <- 2.2
  # class_3_cv <- 2.6
  # class_4_cv <- 2.8
  # class_5_cv <- 3.0

  # class_1_shading <-subset(solar_shading_angles, elevation > class_1_cv)
  # class_2_shading <- subset(solar_shading_angles, elevation > class_2_cv)
  # class_3_shading <- subset(solar_shading_angles, elevation > class_3_cv)
  # class_2_3_shading <- subset(solar_shading_angles, elevation > class_2_cv) 
  # class_4_shading <- subset(solar_shading_angles, elevation > class_4_cv)
  
  for(n in seq(1, nrow(data), 1)){
    data[n,"meet_class1"] <- TRUE
    data[n,"meet_class2"] <- TRUE
    data[n,"meet_class3"] <- TRUE
    data[n,"meet_class4"] <- TRUE
    data[n,"meet_class5"] <- TRUE
    
    #class 1
    if(data[n,"shadow_angle"] < class_1_cv & data[n,"shadow_angle"] > data[n,"elevation"]){
      data[n,"meet_class1"] <- TRUE
      data[n,"meet_class2"] <- TRUE
      data[n,"meet_class3"] <- TRUE
      data[n,"meet_class4"] <- TRUE
      data[n,"meet_class5"] <- TRUE
    }
    
    #class 2 / 3
    if(data[n,"shadow_angle"] >= class_1_cv & data[n,"shadow_angle"] < class_2_3_cv & data[n,"shadow_angle"] > data[n,"elevation"]){
      data[n,"meet_class1"] <- FALSE
      data[n,"meet_class2"] <- TRUE
      data[n,"meet_class3"] <- TRUE
      data[n,"meet_class4"] <- TRUE
      data[n,"meet_class5"] <- TRUE
    }
    
    #class 4
    if(data[n,"shadow_angle"] >= class_2_3_cv & data[n,"shadow_angle"] < class_4_cv & data[n,"shadow_angle"] > data[n,"elevation"]){
      data[n,"meet_class1"] <- FALSE
      data[n,"meet_class2"] <- FALSE
      data[n,"meet_class3"] <- FALSE
      data[n,"meet_class4"] <- TRUE
      data[n,"meet_class5"] <- TRUE
    }
    
    ##class 4
    # if(data[n,"shadow_angle"] >= class_3_cv & data[n,"shadow_angle"] < class_4_cv & data[n,"shadow_angle"] > data[n,"elevation"]){
    #   data[n,"meet_class1"] <- FALSE
    #   data[n,"meet_class2"] <- FALSE
    #   data[n,"meet_class3"] <- FALSE
    #   data[n,"meet_class4"] <- TRUE
    #   data[n,"meet_class5"] <- TRUE
    # }
    
    #class 5
    if(data[n,"shadow_angle"] >= class_4_cv & data[n,"shadow_angle"] > data[n,"elevation"]){
      data[n,"meet_class1"] <- FALSE
      data[n,"meet_class2"] <- FALSE
      data[n,"meet_class3"] <- FALSE
      data[n,"meet_class4"] <- FALSE
      data[n,"meet_class5"] <- TRUE
    }
    
    if(data[n,"meet_class5"] == TRUE){
      data[n,"final_indiv_class"] <- 5
    }
    if(data[n,"meet_class4"] == TRUE){
      data[n,"final_indiv_class"] <- 4
    }
    if(data[n,"meet_class3"] == TRUE){
      data[n,"final_indiv_class"] <- 3
    }
    if(data[n,"meet_class2"] == TRUE){
      data[n,"final_indiv_class"] <- 2
    }
    if(data[n,"meet_class1"] == TRUE){
      data[n,"final_indiv_class"] <- 1
    }

  }
  final_class <- max(data[,"final_indiv_class"]) 
  #print(final_class)
  data$final_Class <- final_class 
  
  if(aws_name != ""){
    fwrite(data, paste0("output/solar_shadow_angles/", aws_name_trim, "/", aws_name_trim, "_ah_solar_shadow_angles_classes.csv"))
  } else {
    fwrite(data, "output/solar_shadow_angles/ah_solar_shadow_angles_classes.csv")
  }
  return (data)
}
