library(devtools)
library(data.table)
#globals
#dataraw <- function(){
# epsg_rd <- "+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.9999079 +x_0=155000 +y_0=463000 +ellps=bessel +towgs84=565.4171,50.3319,465.5524,-0.398957,0.343988,-1.8774,4.0725 +units=m +no_defs"
# epsg_wgs <- "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# usethis::use_data(epsg_rd, overwrite = TRUE, internal = TRUE)
# usethis::use_data(epsg_wgs, overwrite = TRUE, internal = TRUE)

# AWS coordinates
AWS.df<-data.table::fread("data-raw/AWS_coordinates.csv", data.table = FALSE)
all_AWS_names <- base::unique(as.character(unlist(select(AWS.df, name))))
AWS_names <- dplyr::setdiff(all_AWS_names, c("Europlatform", "F3-FB-1 platform", "K13 platform", "Lichteiland Goeree", "platform  D15-FA-1", "platform  P11-B", "platform A12-CPP", "platform AWG-1", "platform F16-A", "platform Hoorn-Alfa", "platform J6-A", "platform K14-FA-1C", "platform L9-FF-1")) 
AWS_ahn3Only_names <- dplyr::setdiff(AWS_names, c("Maastricht AP", "Ell", "Arcen", "Hupsel", "Twenthe", "Nieuw Beerta", "Eelde", "Deelen"))
AWS_ahn2Only_names <- dplyr::setdiff(AWS_names, AWS_ahn3Only_names)

usethis::use_data(AWS.df, overwrite = TRUE, internal = TRUE)
usethis::use_data(AWS_names, overwrite = TRUE, internal = TRUE)
usethis::use_data(AWS_ahn3Only_names, overwrite = TRUE, internal = TRUE)
usethis::use_data(AWS_ahn2Only_names, overwrite = TRUE, internal = TRUE)

temperature_sensor_name <- "temp_150cm"
usethis::use_data(temperature_sensor_name, overwrite = TRUE)

#create sf spatial points
AWS.sf <- sf::st_as_sf(AWS.df, coords = c("X", "Y"), crs = epsg_rd)
st_crs(AWS.sf) <- epsg_rd  
usethis::use_data(AWS.sf,overwrite=TRUE, internal = TRUE)

#WMO SC guielines criteria
guideline_criteria <- data.table::fread("data-raw/guideline_criteria.csv")
usethis::use_data(guideline_criteria, overwrite = TRUE, internal = TRUE)

#manual SC values
manual_SC_values <- data.table::fread("data-raw/manual_classification_values.csv", data.table = FALSE)
usethis::use_data(manual_SC_values, overwrite = TRUE, internal = TRUE)


#bgt objects
bgt_objects.csv <- utils::read.table("data-raw/bgt_objects.csv", header = TRUE, sep=",", blank.lines.skip = TRUE)
usethis::use_data(bgt_objects.csv, overwrite = TRUE, internal = TRUE)
#}