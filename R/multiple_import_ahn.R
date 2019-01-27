multiple_import_ahn <- function(aws.df = AWS.df, aws_list, sensor_name, addition = "", LONLAT = FALSE, radius, resolution = 0.5, AHN3= FALSE, raw_ahn = TRUE, terrain_ahn = TRUE, delete_sheets = TRUE, redownload_ahn = FALSE){
  start_time <- Sys.time()
  for (a in 1:length(aws_list)){
    spatialpoint.sp <- select_single_aws(aws.df = aws.df, aws_name = aws_list[a], sensor_name = sensor_name)[["aws_rd.sp"]]
    print(paste0("Getting AHN of ", aws_list[a]))
    import_single_ahn(aws.df = aws.df, 
                      aws_name = aws_list[a], addition = addition,
                      station_coords = spatialpoint.sp,#station_coords = selected_aws_temp[["aws_rd.sp"]],
                      LONLAT = LONLAT,
                      resolution = resolution, radius = radius,
                      raw_ahn = raw_ahn, terrain_ahn = terrain_ahn,
                      AHN3 = AHN3,
                      delete_sheets = delete_sheets,
                      redownload = redownload_ahn)
  }
  end_time <- Sys.time()
  elapsed_time <- ceiling(end_time - start_time)
  message(paste("Finished importing AHN data sets. Elapsed Time:", elapsed_time, "seconds."))
}

multiple_import_ahn(aws_list = c("Lauwersoog"), addition = "",
                    sensor_name = temperature_sensor_name,
                    LONLAT = FALSE,
                    radius = 500, resolution = 0.5,
                    AHN3 = TRUE, raw_ahn = TRUE, terrain_ahn = TRUE,
                    delete_sheets = TRUE, redownload_ahn = FALSE)
