aws_list <- dplyr::filter(AWS.df, DS_DESC == "AWS" & Aparatuur == "site")[,1]

import_bgt <- function(aws_list, delete_raw_gmls){
  if(missing(delete_raw_gmls)){
    delete_raw_gmls <- FALSE
  }
  all_objects_count <- data.frame(aws_name = as.character(), objectname = as.character(), count = as.numeric(), stringsAsFactors=FALSE)
  all_objects_counts_names <- c("AWS", "object_name", "object_count")
  names(all_objects_count) <- all_objects_counts_names
  all_bgt_objects <- data.frame(aws = character(0), feature_type = character(0), has_features = logical(0), features_count = numeric(0), stringsAsFactors = FALSE)
  #loading all AWS on land takes 10 minutes.
  for (a in 1:length(aws_list)){
    print(aws_list[a])
    BGT_single_aws <- import_single_bgt(aws_list[a], "site", delete_raw_gmls)
    all_objects_count <- rbind(all_objects_count, BGT_single_aws[["objects_count"]], stringsAsFactors=FALSE) 
    all_bgt_objects <- rbind(all_bgt_objects, BGT_single_aws[["bgt_objects"]], stringsAsFactors=FALSE)
    print(" ")
    print(" ")
  }
  fwrite(all_objects_count, "data/BGT/all_objects_count.csv")
  message("BGT has been loaded and saved for", length(aws_list), "AWS sites.")
  return(list("all_objects_count" = all_objects_count, "all_bgt_objects"=all_bgt_objects))
}

all_aws_bgt <- import_bgt(aws_list = aws_list, delete_raw_gmls = TRUE)
