# directories <- list.dirs("output/")
# for(dir in directories){
#   #print(dir)
#   new_dir_name <- gsub("___", "", dir)
#   #print(new_dir_name)
#   file.rename(dir, new_dir_name)
# }
# 
# print(files)
# for(file in files){
#   #print(file)
#   new_file_name <- gsub("___", "", file)
#   print(new_file_name)
#   file.rename(file, "test")
# }
files <- list.files("final_output/output/all_summaries/", recursive = TRUE, full.names = FALSE, pattern = ".csv")

for(f in files){
  print(file)
  df <- data.table::fread(f, data.table = FALSE)
  
  rename <- FALSE
  if("AWS" %in% colnames(df)){
    df <- dplyr::rename(df, name = AWS)
    rename <- TRUE
  }
  if("Sensor" %in% colnames(df)){
    df <- dplyr::rename(df, sensor.name = Sensor)
    rename <- TRUE
  }
  #file.remove(file)
  if(rename == TRUE){
    data.table::fwrite(df, f)
  }
}

# sapply(files,FUN=function(eachPath){
#   file.rename(from=eachPath,to=sub(pattern="__" ,replacement="_",eachPath))
# })
