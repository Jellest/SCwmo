# computer configurations
storage_settings <- 2 # 1 = Linux Jelle, 2 = windows Jelle

getWoringDirectory <- function(storage_settings){
  working_directories <- c("/nobackup/users/stuurman/","C:/Users/KNMI/Dropbox/KNMI/R_SPQJ")
  wd <- working_directories[storage_settings]
}
working_directory<- getWoringDirectory(storage_settings                        )
setwd(working_directory)

# folder structure

getFolderStructure <- function(storage_settings){
  folder_structures <- c("/", "\\")
  fs <- folder_structures[storage_settings]
return (fs)
  } 
folder_structure <- getFolderStructure(storage_settings)
