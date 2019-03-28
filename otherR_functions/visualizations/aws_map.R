aws_map <- function(){
  
  NL <- st_read(dsn = "data/boundariesNL.shp", layer = "boundariesNL")
  aws <- st_read(dsn = "data/coordinates/AWS_coordinates_temperature_rd.shp", layer = "AWS_coordinates_temperature_rd")
  #mapview(aws)
  #View(aws)
  aws_selection <- setdiff(AWS_temperature_names, c("Hoek van Holland", "Nieuw Beerta", "Rotterdam", "Voorschoten", "Herwijnen", "Vlissingen", "Eelde", "Woensdrecht"))
  #View(aws_selection)
  aws_s <- subset(aws, AWS %in% aws_selection)
  #View(aws_s)
  aws_hvh <- subset(aws, AWS == "Hoek van Holland")
  aws_Nb <- subset(aws, AWS == "Nieuw Beerta")
  aws_Rd <- subset(aws, AWS == "Rotterdam")
  aws_Vs <- subset(aws, AWS == "Voorschoten")
  aws_Hw <- subset(aws, AWS == "Herwijnen")
  aws_Vl <- subset(aws, AWS == "Vlissingen")
  aws_Ee <- subset(aws, AWS == "Eelde")
  aws_Wo <- subset(aws, AWS == "Woensdrecht")
  tmap_mode("plot")

  tm_NL <- tm_shape(NL) + tm_fill(col = "#DAE8FC")  
  tm_aws_s <- tm_NL + tm_shape(aws_s) + tm_dots(size = 0.4, col = '#6C8EBF', labels = aws_selection) + tm_text(text = 'AWS', ymod = 0.5, size = 0.7)
  tm_aws_hvh <- tm_aws_s + tm_shape(aws_hvh) + tm_dots(size = 0.4, col = '#6C8EBF', labels = "Hoek van Holland") + tm_text(text = 'AWS',ymod = 0.5, xmod = -1, size = 0.7) 
  #tmap_arrange(tm_NL, tm_aws)
  tm_aws_Vs <- tm_aws_hvh + tm_shape(aws_Vs) + tm_dots(size = 0.4, col = '#6C8EBF', labels = "Voorschoten") + tm_text(text = 'AWS',ymod = 0.5, xmod = -1.0, size = 0.7) 
  tm_aws_Rd <- tm_aws_Vs + tm_shape(aws_Rd) + tm_dots(size = 0.4, col = '#6C8EBF', labels = "Rotterdam") + tm_text(text = 'AWS',ymod = -0.5, xmod = -0.5, size = 0.7) 
  tm_aws_Hw <- tm_aws_Rd + tm_shape(aws_Hw) + tm_dots(size = 0.4, col = '#6C8EBF', labels = "Herwijnen") + tm_text(text = 'AWS',ymod = 0.5, xmod = 1.0, size = 0.7) 
  tm_aws_Vl <- tm_aws_Hw + tm_shape(aws_Vl) + tm_dots(size = 0.4, col = '#6C8EBF', labels = "Vlissingen") + tm_text(text = 'AWS', ymod=-0.5, xmod = 0.2, size = 0.7) 
  tm_aws_Nb <- tm_aws_Vl + tm_shape(aws_Nb) + tm_dots(size = 0.4, col = '#6C8EBF', labels = "Nieuw Beerta") + tm_text(text = 'AWS', ymod=0.5, xmod = -1.5, size = 0.7) 
  tm_aws_Ee <- tm_aws_Nb + tm_shape(aws_Ee) + tm_dots(size = 0.4, col = '#6C8EBF', labels = "Eelde") + tm_text(text = 'AWS', ymod=-0.5, size = 0.7) 
  tm_aws_Wo <- tm_aws_Ee + tm_shape(aws_Wo) + tm_dots(size = 0.4, col = '#6C8EBF', labels = "Woensdrecht") + tm_text(text = 'AWS', ymod=-0.5, xmod= 0.5, size = 0.7) 
  
  #tm_aws_Rd
  
  tmap_save(tm_aws_Wo, filename = "aws_map.png")

}
aws_map()
