summary_classifcation <- function(aws_name, sensor_name, class_selection){
  if(missing(class_selection)){
    class_selection <- "final_class"
  }
  aws_name_trim <- getAWS_name_trim(aws_name)
  
  shc <- fread(paste0("output/solar_shadow_angles/", aws_name_trim, "/", aws_name_trim, "_ah_solar_shadow_angles_classes.csv"))
  luc <- fread(paste0("output/land use/", aws_name_trim, "/", aws_name_trim, "_solar_shadow_classes.csv"))
  vhc <- fread(paste0("output/vegetation_height/", aws_name_trim, "/", aws_name_trim, "_vegetation_height_classes.csv"))
  
  shc_finalClass <- shc[1,class_selection] 
  luc_finalClass <- luc[1,class_selection]
  vhc_finalClass <- vhc[1,class_selection]
  
  indiv_classes <- c(shc_finalClass, luc_finalClass, vhc_finalClass)
  
  max_class <- max(indiv_classes)
  df <- dta.frame(AWS = character(0), sensor_name = character(0), shades = numeric(), land_use = numeric(0), vegetation_height = numeric(0), final_class = numeric(0))
  
  results <- cbind(aws_name, sensor_name, indiv_classes, max_class)
  df <- rbind(df, results)
  
  View(df)
  return(df)
}

summary_classifcation("De Bilt", "temp_150cm")
