# computer configurations
home_directory <- NULL
hd <- function(settings){
  # make sure home directory is directry that includes  R folder and the data foder.
  home_directories <- c("/nobackup/users/stuurman","C:/Users/Jelle/Dropbox/KNMI/R_SPQJ")
  home_directory <<- home_directories[settings]
  setwd(home_directory)
  print(paste("setup_settings:", settings, sep="  "))
  print(paste("home directory:", home_directory, sep=" "))
}

libs <- function(){
  library(raster)
  library(rgeos)
  library(rgdal)
  library(gdalUtils)
  library(gdata)
  library(sp)
  library(sf)
  library(lubridate)
  library(insol)
  library(ggplot2)
  library(leaflet)
  library(mapview)
  library(devtools)
  library(data.table)
  library(dplyr)
}

start <- function(settings){
  hd(settings)
  libs()
}
