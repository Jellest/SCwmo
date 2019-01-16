vegetation_classes <- function (df, cv_colName){
  if(missing(cv_colName)){
    cv_colName <- "Criteria_Value"
  }
  vhc <- dplyr::filter(guideline_criteria, Variable == "temperature" & Criteria_Type == "vegetation height")
  
  class_1_filter <-  filter(vhc, Class == "1")
  class_1_cv <- as.numeric(class_1_filter[,cv_colName][1])
  
  class_2_filter <-  filter(vhc, Class == "2")
  class_2_cv <- as.numeric(class_2_filter[,cv_colName][1])
  
  class_3_filter <-  filter(vhc, Class == "3")
  class_3_cv <- as.numeric(class_3_filter[,cv_colName][1])
  
  class_4_filter <-  filter(vhc, Class == "4")
  class_4_cv <- as.numeric(class_4_filter[,cv_colName][1])
  
  class_5_filter <-  filter(vhc, Class == "5")
  class_5_cv <- as.numeric(class_5_filter[,cv_colName][1])
  
  if(df[1,"Median"] < class_1_cv){
    df[1,"meet_class1"] <- TRUE
    df[1,"meet_class2"] <- TRUE
    df[1,"meet_class3"] <- TRUE
    df[1,"meet_class4"] <- TRUE
    df[1,"meet_class5"] <- TRUE
  }
  
  if(df[1,"Median"] > class_1_cv & df[1,"Median"] < class_2_cv){
    df[1,"meet_class1"] <- FALSE
    df[1,"meet_class2"] <- TRUE
    df[1,"meet_class3"] <- TRUE
    df[1,"meet_class4"] <- TRUE
    df[1,"meet_class5"] <- TRUE 
  }
  
  if(df[1,"Median"] > class_2_cv & df[1,"Median"] < class_3_cv){
    df[1,"meet_class1"] <- FALSE
    df[1,"meet_class2"] <- FALSE
    df[1,"meet_class3"] <- TRUE
    df[1,"meet_class4"] <- TRUE
    df[1,"meet_class5"] <- TRUE 
  }
  
  if(df[1,"Median"] > class_3_cv){
    df[1,"meet_class1"] <- FALSE
    df[1,"meet_class2"] <- FALSE
    df[1,"meet_class3"] <- FALSE
    df[1,"meet_class4"] <- TRUE
    df[1,"meet_class5"] <- TRUE 
  }
  

  if(df[1,"meet_class4"] == TRUE){
    df[1,"final_class"] <- 4
  }
  if(df[1,"meet_class3"] == TRUE){
    df[1,"final_class"] <- 3
  }
  if(df[1,"meet_class2"] == TRUE){
    df[1,"final_class"] <- 2
  }
  if(df[1,"meet_class1"] == TRUE){
    df[1,"final_class"] <- 1
  }

  View(df)
  return(df)
}
