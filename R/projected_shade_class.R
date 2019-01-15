projected_shade_class <- function(solar_shading_angles, cv_colName){
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
  
  df <- solar_shading_angles 
  
  for(n in seq(1, nrow(df), 1)){
    df[n,"meet_class1"] <- TRUE
    df[n,"meet_class2"] <- TRUE
    df[n,"meet_class3"] <- TRUE
    df[n,"meet_class4"] <- TRUE
    df[n,"meet_class5"] <- TRUE
    
    #class 1
    if(df[n,"shadow_angle"] < class_1_cv & df[n,"shadow_angle"] > df[n,"elevation"]){
      df[n,"meet_class1"] <- TRUE
      df[n,"meet_class2"] <- TRUE
      df[n,"meet_class3"] <- TRUE
      df[n,"meet_class4"] <- TRUE
      df[n,"meet_class5"] <- TRUE
    }
    
    #class 2 / 3
    if(df[n,"shadow_angle"] >= class_1_cv & df[n,"shadow_angle"] < class_2_3_cv & df[n,"shadow_angle"] > df[n,"elevation"]){
      df[n,"meet_class1"] <- FALSE
      df[n,"meet_class2"] <- TRUE
      df[n,"meet_class3"] <- TRUE
      df[n,"meet_class4"] <- TRUE
      df[n,"meet_class5"] <- TRUE
    }
    
    #class 4
    if(df[n,"shadow_angle"] >= class_2_3_cv & df[n,"shadow_angle"] < class_4_cv & df[n,"shadow_angle"] > df[n,"elevation"]){
      df[n,"meet_class1"] <- FALSE
      df[n,"meet_class2"] <- FALSE
      df[n,"meet_class3"] <- FALSE
      df[n,"meet_class4"] <- TRUE
      df[n,"meet_class5"] <- TRUE
    }
    
    ##class 4
    # if(df[n,"shadow_angle"] >= class_3_cv & df[n,"shadow_angle"] < class_4_cv & df[n,"shadow_angle"] > df[n,"elevation"]){
    #   df[n,"meet_class1"] <- FALSE
    #   df[n,"meet_class2"] <- FALSE
    #   df[n,"meet_class3"] <- FALSE
    #   df[n,"meet_class4"] <- TRUE
    #   df[n,"meet_class5"] <- TRUE
    # }
    
    #class 5
    if(df[n,"shadow_angle"] >= class_4_cv & df[n,"shadow_angle"] > df[n,"elevation"]){
      df[n,"meet_class1"] <- FALSE
      df[n,"meet_class2"] <- FALSE
      df[n,"meet_class3"] <- FALSE
      df[n,"meet_class4"] <- FALSE
      df[n,"meet_class5"] <- TRUE
    }
    
    if(df[n,"meet_class5"] == TRUE){
      df[n,"final_indiv_class"] <- 5
    }
    if(df[n,"meet_class4"] == TRUE){
      df[n,"final_indiv_class"] <- 4
    }
    if(df[n,"meet_class3"] == TRUE){
      df[n,"final_indiv_class"] <- 3
    }
    if(df[n,"meet_class2"] == TRUE){
      df[n,"final_indiv_class"] <- 2
    }
    if(df[n,"meet_class1"] == TRUE){
      df[n,"final_indiv_class"] <- 1
    }

  }
  final_class <- max(df[,"final_indiv_class"]) 
  print(final_class)
  df$final_Class <- final_class 
  
  return (df)
}

ah_ssa_deBilt <- fread("output/solar_shadow_angles/DeBilt_ah_solar_shadow_angles.csv")

ah_ssa_deBilt_classes <- projected_shade_class(ah_ssa_deBilt)
fwrite(ah_ssa_deBilt_classes, "output/solar_shadow_angles/DeBilt_ah_solar_shadow_angles_classes.csv")
