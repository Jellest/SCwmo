# computer configurations
start <- function(settings){
  hd <- function(settings){
    # make sure home directory is directry that includes  R folder and the data foder.
    home_directories <- c("/nobackup/users/stuurman","C:/Users/Jelle/Dropbox/KNMI/R_SPQJ", "C:/Users/3691233/Dropbox/KNMI/R_SPQJ")
    home_directory <<- home_directories[settings]
    setwd(home_directory)
    print(paste("setup_settings:", settings, sep="  "))
    print(paste("home directory:", home_directory, sep=" "))
  }
  
  libs <- function(){
    library(raster)
    library(rgdal)
    library(gdalUtils)
    #library(gdata)
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
    library(horizon)
    library(xtable)
    library(htmlwidgets)
    library(htmltools)
    library(tmap)
    library(rasterVis)
    library(RCurl)
  }
  hd(settings)
  libs()
  
  #global variables
  #AWS.df<<-fread("datasets/coordinates/AWS_coordinates.csv", data.table = FALSE)
  #manual_SC_values <<- fread("datasets/classifcations/manual_classification_values.csv", data.table = FALSE) 
  #guideline_criteria <<- fread("datasets/wmo_guidelines/guideline_criteria.csv", data.table = FALSE)

}

#AWS_test.df<<-fread("datasets/coordinates/AWS_coordinates.csv", data.table = FALSE)

#installl Perl
#installXLSXsupport()
install.packages(c("horizon", "raster", "rgeos", "gdalUtils", "gdata", "sp", "sf", "lubridate", "insol", "ggplot2", "leaflet", "mapview", "devtools", "data.table", "dplyr", "htmlwidgets", "htmltools", "rasterVis", "xtable"), dependencies = TRUE )
