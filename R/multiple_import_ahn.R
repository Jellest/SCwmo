#' multiple import AHN
#'@title smultiple import ahn
#'@description multiple import ahn 
#'@param name_list list with string names.
#'@param LONLAT Logic. Dfeault FALSE. Set to TRUE if spatialpoint is in WGS84
#'@param AHN3 Default TRUE. Set to FALSE if AHN2 needs to be used.
#'@param ahn_radius radius of AHN
#'@author Jelle Stuurman
multiple_import_ahn <- function(X = NULL, Y = NULL, name_list, sensor.name = "", name.supplement = "", AWS = TRUE, LONLAT = FALSE, ahn_radius, resolution = 0.5, AHN3= TRUE, raw_ahn = TRUE, terrain_ahn = TRUE, delete_sheets = TRUE, redownload_ahn = FALSE){
  start_time <- Sys.time()
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
    
    print(paste0("Getting AHN of ", name_list[a]))
    import_single_ahn(spatialpoint = spatialpoint, name = name_list[a], name.supplement = name.supplement,
                      resolution = resolution, radius = ahn_radius,
                      raw_ahn = raw_ahn, terrain_ahn = terrain_ahn,
                      AHN3 = AHN3,
                      delete_sheets = delete_sheets,
                      redownload = redownload_ahn)
  }
  end_time <- Sys.time()
  elapsed_time <- ceiling(end_time - start_time)
  message(paste("Finished importing AHN data sets. Elapsed Time:", elapsed_time, "seconds."))
}