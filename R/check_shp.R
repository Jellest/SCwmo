#'Check if Shp files and remove it
#'
#'@title Check and remove shapefile
#'@description Check if shapefile path exists and remove all assocaited files from memory disk
#'@param shape_path path of shapefile
#'@author Jelle Stuurman
check_shpExists <- function (shape_path){
  first_part_path <-  substr(shape_path, 1, nchar(shape_path)-4) 
  if(file.exists(shape_path) == TRUE){
    print(first_part_path)
    file.remove(shape_path)
    file.remove(paste0(first_part_path, ".shx"))
    file.remove(paste0(first_part_path, ".prj"))
    file.remove(paste0(first_part_path, ".dbf"))
  }
}