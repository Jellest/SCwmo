# computer configurations
setup_settings <- NULL 
home_directory <- NULL
folder_structure <- NULL

hd <- function(settings){
  # make sure home directory is directry that includes  R folder and the data foder.
  setup_settings <<- settings
  home_directories <- c("/nobackup/users/stuurman","C:/Users/Jelle/Dropbox/KNMI/R_SPQJ")
  home_directory <<- home_directories[settings]
  folder_structures <- c("/", "/")
  folder_structure <<- folder_structures[settings]
  setwd(home_directory)
  print(paste("setup_settings:", settings, sep="  "))
  print(paste("home directory:", home_directory, sep=" "))
}
