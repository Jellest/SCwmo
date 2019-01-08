projected_shade_class <- function(solar_shading_angles){
  tshac <- dplyr::filter(guideline_criteria, Variable == "temperature" & Criteria_Type == "shading")

  cv_colName <- "Criteria_Value"
  
  # class_1_cv <-  tshac[which(tshac, Class == 1),cv_colName]
  # class_2_cv <-  tshac[which(tshac, Class == 2),cv_colName]
  # class_3_cv <-  tshac[which(tshac, Class == 3),cv_colName]
  # class_2_3_cv <-tshac[which(tshac, Class == 2),cv_colName]
  # class_4_cv <-  tshac[which(tshac, Class == 4),cv_colName]
  # class_5_cv <-  tshac[which(tshac, Class == 5),cv_colName]
  
  class_1_filter <-  filter(tshac, Class == "1")
  class_1_cv <- class_1_filter[,cv_colName][1]
  
  class_2_filter <-  filter(tshac, Class == "2")
  class_2_cv <- class_2_filter[,cv_colName][1]
  
  class_3_filter <-  filter(tshac, Class == "3")
  class_3_cv <- class_3_filter[,cv_colName][1]
  
  class_2_3_cv <- class_2_filter[,cv_colName][1] 
  
  class_4_filter <-  filter(tshac, Class == "4")
  class_4_cv <- class_4_filter[,cv_colName][1]
  
  class_5_filter <-  filter(tshac, Class == "5")
  class_5_cv <- class_5_filter[,cv_colName][1]
  
  
  class_1_cv <- 2
  class_2_cv <- 2.2
  class_3_cv <- 2.6
  class_4_cv <- 2.8
  class_5_cv <- 3.0
  
  # class_2_shading <-subset(solar_shading_angles, elevation > class_2_cv)
  # class_2_3_shading <- subset(solar_shading_angles, elevation > class_2_cv)
  # class_2_shading <- subset(solar_shading_angles, elevation > class_2_cv)
  
  class_1_shading <-subset(solar_shading_angles, elevation > class_1_cv)
  class_2_shading <- subset(solar_shading_angles, elevation > class_2_cv)
  class_3_shading <- subset(solar_shading_angles, elevation > class_3_cv)
  class_2_3_shading <- subset(solar_shading_angles, elevation > class_2_cv) 
  class_4_shading <- subset(solar_shading_angles, elevation > class_4_cv)
  
  df <- solar_shading_angles 
  
  for(e in seq(1, nrow(df), 1)){
    print(e)
    df[e,"meet_class1"] <- TRUE
    df[e,"meet_class2"] <- TRUE
    df[e,"meet_class3"] <- TRUE
    df[e,"meet_class4"] <- TRUE
    df[e,"meet_class5"] <- TRUE
    
    #class 1
    if(df[e,"elevation"] < class_1_cv){
      df[e,"meet_class1"] <- TRUE
      df[e,"meet_class2"] <- TRUE
      df[e,"meet_class3"] <- TRUE
      df[e,"meet_class4"] <- TRUE
      df[e,"meet_class5"] <- TRUE
    }
    
    #class 2
    if(df[e,"elevation"] >= class_2_cv & df[e,"elevation"] < class_3_cv & df[e,"shadow_angle"] > df[e,"elevation"]){
      df[e,"meet_class1"] <- FALSE
      df[e,"meet_class2"] <- FALSE
      df[e,"meet_class3"] <- TRUE
      df[e,"meet_class4"] <- TRUE
      df[e,"meet_class5"] <- TRUE
    }# else if(df[e,"elevation"] >= 5 & df[e,"elevation"] < 7 & df[e,"shadow_angle"] < df[e,"elevation"]){
    #   meet_class1 <- TRUE
    #   meet_class2[e] <- TRUE
    #   meet_class3[e] <- TRUE
    #   meet_class4[e] <- TRUE
    #   meet_class5[e] <- TRUE
    # }
    
    #class 3
    if(df[e,"elevation"] >= class_3_cv & df[e,"elevation"] < class_4_cv & df[e,"shadow_angle"] > df[e,"elevation"]){
      df[e,"meet_class1"] <- FALSE
      df[e,"meet_class2"] <- FALSE
      df[e,"meet_class3"] <- FALSE
      df[e,"meet_class4"] <- TRUE
      df[e,"meet_class5"] <- TRUE
    }# else if(df[e,"elevation"] >= 7 & df[e,"elevation"] < 20 & df[e,"shadow_angle"] < df[e,"elevation"]){
    #   meet_class1 <- TRUE
    #   meet_class2[e] <- TRUE
    #   meet_class3[e] <- TRUE
    #   meet_class4[e] <- TRUE
    #   meet_class5[e] <- TRUE
    # }
    
    #class 4
    if(df[e,"elevation"] >= class_4_cv & df[e,"shadow_angle"] > df[e,"elevation"]){
      df[e,"meet_class1"] <- FALSE
      df[e,"meet_class2"] <- FALSE
      df[e,"meet_class3"] <- FALSE
      df[e,"meet_class4"] <- FALSE
      df[e,"meet_class5"] <- TRUE
    }# else if(df[e,"elevation"] >= 20 & df[e,"shadow_angle"] < df[e,"elevation"]){
    #   meet_class1 <- FALSE
    #   meet_class2[e] <- FALSE
    #   meet_class3[e] <- FALSE
    #   meet_class4[e] <- TRUE
    # }
    if(df[e,"meet_class5"] == TRUE){
      df[e,"final_class"] <-5
    }
    if(df[e,"meet_class4"] == TRUE){
      df[e,"final_class"] <- 4
    }
    if(df[e,"meet_class3"] == TRUE){
      df[e,"final_class"] <-3
    }
    if(df[e,"meet_class2"] == TRUE){
      df[e,"final_class"] <-2
    }
    if(df[e,"meet_class1"] == TRUE){
      df[e,"final_class"] <-1
    }
  }
  View(df)
  return (df)
}

test_ssa_criteria <- projected_shade_class(test_ssa[["ssa"]])
