vegetation_classes <- function (df, cv_colName = "Criteria_Value", aws_name, AHN3 = FALSE, exportCSV = FALSE){
  if(missing(aws_name)){
    aws_name <- ""
    aws_name_trim <- ""
  } else {
    aws_name_trim <- getAWS_name_trim(aws_name)
  }
  
  if(AHN3 == TRUE){
    AHN <- "AHN3"
  } else {
    AHN <- "AHN2"
  }
  
  vhc <- dplyr::filter(guideline_criteria, Variable == "temperature" & Criteria_Type == "vegetation height")
  vhc <- check_criteria(df = vhc, cv_colName = cv_colName)
  
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
  

  if(exportCSV == TRUE){
    if(!dir.exists("output/vegetation_height")){
      dir.create("output/vegetation_height", showWarnings = FALSE)
    }
    if(dir.exists(paste0("output/vegetation_height/", aws_name_trim)) == FALSE & aws_name_trim != ""){
      dir.create(paste0("output/vegetation_height/", aws_name_trim), showWarnings = FALSE)
    }
    
    if(aws_name != ""){
      fwrite(df, paste0("output/vegetation_height/", aws_name_trim, "/", aws_name_trim, "_", AHN, "_vegetation_height_classes.csv"))
    } else {
      fwrite(df, "output/vegetation_height/", AHN, "_vegetation_height_classes.csv")
    }
  }
  View(df)
  return(df)
}
