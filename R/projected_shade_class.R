projected_shade_class <- function(aws.df = AWS.df, data_path, criteria_columnName = "Criteria_Value", aws_name, AHN3 = FALSE){
  if(missing(aws_name)){
    aws_name <- ""
  }
  if(AHN3 == TRUE){
    AHN <- "AHN3"
  } else {
    AHN <- "AHN2"
  }
  
  aws_name_trim <- getAWS_name_trim(aws.df = aws.df, aws_name = aws_name)
  
  data <- fread(data_path, data.table = FALSE)
  
  tshac <- dplyr::filter(guideline_criteria, Variable == "temperature" & Criteria_Type == "shading")
  tshac <- check_criteria(df = tshac, criteria_columnName = criteria_columnName)
  
  class_1_filter <-  filter(tshac, Class == "1")
  class_1_cv <- as.numeric(class_1_filter[,criteria_columnName][1])
  
  class_2_filter <-  filter(tshac, Class == "2")
  class_2_cv <- as.numeric(class_2_filter[,criteria_columnName][1])
  
  class_3_filter <-  filter(tshac, Class == "3")
  class_3_cv <- as.numeric(class_3_filter[,criteria_columnName][1])
  
  class_2_3_cv <- as.numeric(class_2_filter[,criteria_columnName][1]) 
  
  class_4_filter <-  filter(tshac, Class == "4")
  class_4_cv <- as.numeric(class_4_filter[,criteria_columnName][1])
  
  class_5_filter <-  filter(tshac, Class == "5")
  class_5_cv <- as.numeric(class_5_filter[,criteria_columnName][1])
  
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

  class1_count <- sum(1 == data[,"final_indiv_class"])
  class2_count <- sum(2 == data[,"final_indiv_class"])
  class3_count <- sum(3 == data[,"final_indiv_class"])  
  class4_count <- sum(4 == data[,"final_indiv_class"])
  class5_count <- sum(5 == data[,"final_indiv_class"])
  
  for(c in seq(1, nrow(data), 1)){
    data[c,"class1_count"] <- class1_count
    data[c,"class2_count"] <- class2_count
    data[c,"class3_count"] <- class3_count  
    data[c,"class4_count"] <- class4_count
    data[c,"class5_count"] <- class5_count
  }
  
  final_class <- max(data[,"final_indiv_class"]) 
  #print(final_class)
  data$final_class <- final_class 
  if(aws_name_trim == ""){
    dir.create("output/solar_shadow_angles", showWarnings = FALSE)
  }
  if(!dir.exists(paste0("output/", aws_name_trim))){
    dir.create(paste0("output/", aws_name_trim), showWarnings = FALSE)
  }
  if(!dir.exists(paste0("output/", aws_name_trim, "/solar_shadow_angles"))){
    dir.create(paste0("output/", aws_name_trim, "/solar_shadow_angles"), showWarnings = FALSE)
  }
  
  if(aws_name != ""){
    fwrite(data, paste0("output/", aws_name_trim, "/solar_shadow_angles/", aws_name_trim, "_", AHN, "_ah_solar_shadow_angles_classes.csv"))
  } else {
    fwrite(data, paste0("output/solar_shadow_angles/",AHN, "_ah_solar_shadow_angles_classes.csv"))
  }
  #View(data)
  message(paste0("Shading passed criteria for class ", final_class, "."))
  return (data)
}

projected_shade_class(data_path = "output/solar_shadow_angles/Voorschoten/Voorschoten_AHN2_ah_solar_shadow_angles.csv",
                      aws_name = "De Bilt")

