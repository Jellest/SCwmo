# computer configurations
storage_settings <- 2 # 1 = Linux Jelle, 2 = windows Jelle

getWorkingDirectory <- function(storage_settings){
  # make sure home directory is directry that refers to R folders and data foder.
  working_directories <- c("/nobackup/users/stuurman","C:/Users/KNMI/Dropbox/KNMI/R_SPQJ")
  wd <- working_directories[storage_settings]
}
home_directory <- getWorkingDirectory(storage_settings)
setwd(home_directory)

# folder structure

# getFolderStructure <- function(storage_settings){
#   folder_structures <- c("/", "/")
#   fs <- folder_structures[storage_settings]
# return (fs)
#   } 
# folder_structure <- getFolderStructure(storage_settings)
