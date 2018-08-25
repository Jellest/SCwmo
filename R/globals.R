### Global variables

# Load computer config first. See inst? computer_config in inst>computer_config.R

#De Bilt station
station <- "deBilt"
debilt<-data.frame("lat"=140802.698, "lon"=456881.8)
coordinates(debilt)<-~lat+lon

data_locations <- c(paste("/nobackup/users/stuurman/data/", dataset, "/", station, sep=""), paste("C:\\Users\\KNMI\\Dropbox\\KNMI\\R_SPQJ\\data\\", dataset, "\\", station, "\\", sep=""), paste("C:\\Users\\KNMI\\Dropbox\\KNMI\\R_SPQJ\\data\\", dataset, "\\", station, sep=""))
data_location <- data_locations[storage_settings]

raw_folders <- c(paste("/", "raw", "/", sep=""), paste("\\", "raw", "\\", sep=""))
raw_folder <- raw_folders[storage_settings]

selection_folders <- c(paste("/", "selections", "/", sep=""), paste("\\", "selections", "\\", sep=""))
selection_folder <- raw_folders[storage_settings]