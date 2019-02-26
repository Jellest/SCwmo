directories <- list.dirs("output/")
for(dir in directories){
  #print(dir)
  new_dir_name <- gsub("___", "", dir)
  #print(new_dir_name)
  file.rename(dir, new_dir_name)
}

files <- list.files("output", recursive = TRUE, full.names = T)
print(files)
for(file in files){
  #print(file)
  new_file_name <- gsub("___", "", file)
  print(new_file_name)
  file.rename(file, "test")
}


sapply(files,FUN=function(eachPath){
  file.rename(from=eachPath,to=sub(pattern="__" ,replacement="_",eachPath))
})
