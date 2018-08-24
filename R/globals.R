#global variables

#De Bilt station
station <- "deBilt"
debilt<-data.frame("lat"=140802.698, "lon"=456881.8)
coordinates(debilt)<-~lat+lon

#sotage settings
storage_settings <- 2

data_locations <- c(paste("/nobackup/users/stuurman/data/", dataset, "/", station, "/", sep=""), paste("C:\\Users\\KNMI\\Dropbox\\KNMI\\R_SPQJ\\data\\", dataset, "\\", station, "\\", sep=""), paste("C:\\Users\\KNMI\\Dropbox\\KNMI\\R_SPQJ\\data\\", dataset, "\\", station, sep=""))
data_location <- data_locations[storage_settings + 1]