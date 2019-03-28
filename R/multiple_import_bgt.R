#' multiple import BGT
#'@title smultiple import BGT
#'@description multiple import BGT 
#'@param name_list list with string names.
#'@param bgt_radius radius of AHN
#'@author Jelle Stuurman
multiple_import_bgt <- function(X = NULL, Y = NULL, name_list, AWS = FALSE, name.supplement = "", sensor.name = "", bgt_radius = 150, delete_raw_gmls = TRUE, redownload = TRUE){
  start_time <- Sys.time()
  all_objects_count <- data.frame(aws_name = as.character(), sensor.name = as.character(), radius = as.numeric(), objectname = as.character(), count = as.numeric(), stringsAsFactors=FALSE)
  all_objects_counts_names <- c("name", "sensor.name", "radius", "object_name", "object_count")
  names(all_objects_count) <- all_objects_counts_names
  all_bgt_objects <- data.frame(name = character(0), sensor.name = character(0), radius = numeric(0), feature_type = character(0), has_features = logical(0), features_count = numeric(0), stringsAsFactors = FALSE)
  
  #loading all AWS on land takes 10 minutes.
  for (a in 1:length(name_list)){
    if(AWS == TRUE){
      LONLAT = FALSE
      spatialpoint <- select_single_aws(name = name_list[a], sensor.name = sensor.name)
    } else {
      if(length(X) != length(Y) | length(Y) != length(name_list) | length(X) != length(name_list)){
        stop("Length of X/Y and/or names are not equal.")
      }
      spatialpoint <- create_spatialPoint(X = X[a], Y = Y[a], LONLAT = LONLAT)
      if(is.null(X) == TRUE | is.null(Y) == TRUE){
        stop("X and/or Y coordinates are missing.")
      }
    }
    print(name_list[a])
    single_BGT <- import_single_bgt(spatialpoint = spatialpoint , name = name_list[a], sensor.name, name.supplement = name.supplement, radius = bgt_radius, delete_raw_gmls = delete_raw_gmls, redownload = redownload)
    all_objects_count <- rbind(all_objects_count, single_BGT[["relevant_objects_count"]], stringsAsFactors=FALSE) 
    all_bgt_objects <- rbind(all_bgt_objects, single_BGT[["all_objects_count"]], stringsAsFactors=FALSE)
    print(" ")
    print(" ")
  }
  fwrite(all_objects_count, "datasets/BGT/all_relevant_objects_count.csv")
  fwrite(all_bgt_objects, "datasets/BGT/all_objects_count.csv") 
  message("BGT has been loaded and saved for ", length(name_list), ".")
  end_time <- Sys.time()
  elapsed_time <- ceiling(end_time - start_time)
  return(list("all_objects_count" = all_objects_count, "all_bgt_objects"=all_bgt_objects))
}