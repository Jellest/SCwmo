#'vegetation height classes
#'
#'@title vegetation height Classes
#'@description Determine vegetation height classes
#'@param df summary statistics dataframe. Must include the column Median.
#'@param spatialpoint single sf point in RD new coordinates
#'@param AHN3 Default TRUE. Set to FALSE if AHN2 needs to be used.
#'@author Jelle Stuurman
#'@return dataframe with Class value for the vegetation criteria
#' \enumerate{
#'   \item height_raster: Height raster determined by subtracting the terrain from the raw AHN raster
#'   \item df: summary statistics of results
#'   \item raw_mask: mask raster of raw AHN
#'   \item terrain_mask: mask raster of terrain AHN
#' }
vegetation_classes <- function (df, criteria_columnName = "Criteria_Value", name, name.supplement = "", AHN3 = FALSE, exportCSV = FALSE){
  if(missing(name)){
    name <- ""
    name_trim <- ""
  } else {
    name_trim <- getName_trim(name = name, name.supplement = name.supplement)
  }
  
  if(AHN3 == TRUE){
    AHN <- "AHN3"
  } else {
    AHN <- "AHN2"
  }
  
  vhc <- dplyr::filter(guideline_criteria, Variable == "temperature" & Criteria_Type == "vegetation height")
  vhc <- check_criteria(df = vhc, criteria_columnName = criteria_columnName)
  
  class_1_filter <-  dplyr::filter(vhc, Class == "1")
  class_1_cv <- as.numeric(class_1_filter[,criteria_columnName][1])
  
  class_2_filter <-  dplyr::filter(vhc, Class == "2")
  class_2_cv <- as.numeric(class_2_filter[,criteria_columnName][1])
  
  class_3_filter <-  dplyr::filter(vhc, Class == "3")
  class_3_cv <- as.numeric(class_3_filter[,criteria_columnName][1])
  
  class_4_filter <-  dplyr::filter(vhc, Class == "4")
  class_4_cv <- as.numeric(class_4_filter[,criteria_columnName][1])
  
  class_5_filter <-  dplyr::filter(vhc, Class == "5")
  class_5_cv <- as.numeric(class_5_filter[,criteria_columnName][1])
  
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
    if(name_trim == ""){
      base::dir.create("output/vegetation_height", showWarnings = FALSE)
    }
    
    if(!dir.exists(paste0("output/", name_trim))){
      base::dir.create(paste0("output/", name_trim), showWarnings = FALSE)
    }
    if(!dir.exists(paste0("output/", name_trim,"/vegetation_height"))){
      base::dir.create(paste0("output/", name_trim, "/vegetation_height"), showWarnings = FALSE)
    }

    
    if(name != ""){
      data.table::fwrite(df, paste0("output/", name_trim, "/vegetation_height/", name_trim, "_", AHN, "_vegetation_height_classes.csv"))
    } else {
      data.table::fwrite(df, "output/vegetation_height/", AHN, "_vegetation_height_classes.csv")
    }
  }
  message(paste0("Vegetation height passed criteria for class ", df[1,"final_class"], "."))
  return(df)
}
