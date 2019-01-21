generate_table <- function(aws.df = AWS.df, aws_name, sensor_name, file, AHN3 = FALSE){
  aws_name_trim <- getAWS_name_trim(aws.df = aws.df, aws_name = aws_name)
  
  if(AHN3 == TRUE){
    AHN <- "AHN3"
  } else {
    AHN <- "AHN2"
  }
  
  #csv files
  shcl <- fread(paste0("output/solar_shadow_angles/", aws_name_trim, "/", aws_name_trim, "_", AHN, "_ah_solar_shadow_angles_classes.csv"), data.table = FALSE)
  lucl <- fread(paste0("output/objects/", aws_name_trim, "/", aws_name_trim, "_objects_classes.csv"), data.table = FALSE)
  vhcl <- fread(paste0("output/vegetation_height/", aws_name_trim, "/", aws_name_trim, "_", AHN, "_vegetation_height_classes.csv"), data.table = FALSE)
  #View(vhcl)
  
  #shading classes
  #df <-  %>% select(matches("count|final_class"))
  #df <- df[1,]
  
  #vegetationo height
  df <- select(vhcl, minHeight, maxHeight, Median, final_class)
  
  #summary
  #select(classings[[1]],AWS, shades_class, objects_class, vegetation_height_class, final_class))
  #View(df)
  
  dfx = xtable(df)
  return (list("df" = df, "dfx" = dfx))
}

generate_table(aws_name = "De Bilt",
               sensor_name = temperature_sensor_name,
               file = ,
               AHN3 = FALSE
               )

