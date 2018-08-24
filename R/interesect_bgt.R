#intersect bgt
library(sf)

intersect_bgt <- function(object_type, buffer_dist){
  
  shape_insct<- buffer(object_type, width= buffer_dist, dissolve=FALSE)
  plot(shape_insct)
  
}

shape_insct <- intersect_bgt(pand_shape, 50)

