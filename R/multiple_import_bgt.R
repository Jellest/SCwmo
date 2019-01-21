multiple_import_bgt <- function(aws_list, radius = 150, delete_raw_gmls = TRUE){
  start_time <- Sys.time()
  all_objects_count <- data.frame(aws_name = as.character(), sensor_name = as.character(), radius = as.numeric(), objectname = as.character(), count = as.numeric(), stringsAsFactors=FALSE)
  all_objects_counts_names <- c("AWS", "sensor_name", "radius", "object_name", "object_count")
  names(all_objects_count) <- all_objects_counts_names
  all_bgt_objects <- data.frame(AWS = character(0), sensor_name = character(0), radius = numeric(0), feature_type = character(0), has_features = logical(0), features_count = numeric(0), stringsAsFactors = FALSE)
  
  #loading all AWS on land takes 10 minutes.
  for (a in 1:length(aws_list)){
    print(aws_list[a])
    BGT_single_aws <- import_single_bgt(aws_name =  aws_list[a], sensor_name = temperature_sensor_name, radius = radius, delete_raw_gmls = delete_raw_gmls)
    all_objects_count <- rbind(all_objects_count, BGT_single_aws[["objects_count"]], stringsAsFactors=FALSE) 
    all_bgt_objects <- rbind(all_bgt_objects, BGT_single_aws[["bgt_objects"]], stringsAsFactors=FALSE)
    print(" ")
    print(" ")
  }
  fwrite(all_objects_count, "data/BGT/all_objects_count.csv")
  message("BGT has been loaded and saved for ", length(aws_list), " AWS sites.")
  end_time <- Sys.time()
  elapsed_time <- ceiling(end_time - start_time)
  return(list("all_objects_count" = all_objects_count, "all_bgt_objects"=all_bgt_objects))
}

multiple_import_bgt(c("De Bilt"))
