generate_table <- function(aws.df = AWS.df, aws_name, sensor_name, addition =  "", file, AHN3 = FALSE){
  aws_name_trim <- getAWS_name_trim(aws.df = aws.df, aws_name = aws_name, addition = addition)
  
  if(AHN3 == TRUE){
    AHN <- "AHN3"
  } else {
    AHN <- "AHN2"
  }
  
  #csv files
  shcl <- fread(paste0("output/", aws_name_trim, "/solar_shadow_angles/", aws_name_trim, "_", AHN, "_ah_solar_shadow_angles_classes.csv"), data.table = FALSE)
  lucl <- fread(paste0("output/", aws_name_trim, "/land_use/", aws_name_trim, "_landUse_classes.csv"), data.table = FALSE)
  vhcl <- fread(paste0("output/", aws_name_trim, "/vegetation_height/", aws_name_trim, "_", AHN, "_vegetation_height_classes.csv"), data.table = FALSE)
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

f_s <- fread("output/all_shades_distribution.csv", data.table = FALSE)

g <- select(f_s, AWS, total_count, notClass1_count, class1_count, class2_count, class3_count, class4_count, class5_count)



print.xtable(xtable(hs_stats()),
             type = getOption("xtable.type", "latex"),
             include.rownames = getOption("xtable.include.rownames", FALSE),
             booktabs = getOption("xtable.booktabs", TRUE))
