# computer configurations
start <- function(settings){
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
  }
  hd(settings)
  libs()
  
  #globals
  AWS.df<<-fread("data/coordinates/AWS_coordinates.csv", data.table = FALSE)
  guideline_criteria <<- fread("wmoSC/data-raw/guideline_criteria.csv")
  epsg_rd <<- "+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.4171,50.3319,465.5524,-0.398957,0.343988,-1.8774,4.0725 +units=m +no_defs"
  sAWS_names <<- c("De Bilt", "Voorschoten", "Wijk aan zee", "Vlissingen", "Arcen", "Rotterdam")
  temp_names <<- c("Wijk aan zee", "Vlissingen", "Arcen", "Rotterdam")
  sAWS_list.df <<- filter(AWS.df, AWS %in% sAWS_names)
  sAWStemperature_list.df <<- filter(AWS.df, AWS %in% sAWS_names & Sensor == "temp_150cm")
  temp_list.df <<- filter(AWS.df, AWS %in% temp_names & Sensor == "temp_150cm")
}

#installl Perl
#installXLSXsupport()
# install.packages(c("horizon", "raster", "rgeos", "gdalUtils", "gdata", "sp", "sf", "lubridate", "insol", "ggplot2", "leaflet", "mapview", "devtools", "data.table", "dplyr"), dependencies = TRUE )