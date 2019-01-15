aws_list <- dplyr::filter(AWS.df, DS_DESC == "AWS" & Aparatuur == "site")[,1]

multiple_ahn <- function(list, delete_sheets){
  if(missing(delete_sheets)){
    delete_sheets = FALSE
  }
  start_time <- Sys.time()
  for (a in 1:length(list)){
    spatialpoint.sp <- select_single_aws(aws.df = AWS.df, aws_name = list[a], sensor_name = "temp_150cm")[["aws_rd.sp"]]
    print(paste0("Getting AHN of ", list[a]))
    import_ahn(aws_name = list[a],
               station_coords = spatialpoint.sp,#station_coords = selected_aws_temp[["aws_rd.sp"]],
               resolution = 0.5, radius = 500,
               raw_ahn = TRUE, terrain_ahn = TRUE,
               AHN3 = FALSE,
               delete_sheets = delete_sheets)
  }
  end_time <- Sys.time()
  elapsed_time <- ceiling(end_time - start_time)
  message(paste("Finished importing multiple AHN data sets. Elapsed Time:", elapsed_time, "seconds."))
}

multiple_ahn(c("De Bilt"), delete_sheets = TRUE)
