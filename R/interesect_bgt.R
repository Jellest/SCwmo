#intersect bgt
library(sf)
library(mapview)

tm_shape(BGT_station.sp, projection = rd_new_CRS, unit = "m", bbox = surroundingExtent)

#BGT_station_wfs.sp <- spTransform(BGT_station.sp, wgs_CRS)


clip_bgt <- function(coords, bgt_shape, buffer_dist){
  bgt_shape.sf <-st_as_sf(bgt_shape) 
  st_crs(bgt_shape.sf, "+init=epsg:28992")
  
  coords.sf <- st_as_sf(coords)
  
  buffer <<- st_buffer(coords.sf, dist=buffer_dist)
  st_crs(buffer, "+init=epsg:28992")
  
  shape_insct.sf <- st_intersection(bgt_shape.sf, buffer)

  buildings.sf <<- subset(shape_insct.sf, object_typ == "pand")
  water.sf <<- subset(shape_insct.sf, object_typ == "waterdeel")
  roads.sf <<- subset(shape_insct.sf, object_typ == "wegdeel")
   
  View(buildings.sf)
  View(water.sf)
  View(roads.sf)
bgt_areas_m2<-list("buildings"=,)  
return(bgt_areas_m2)
}

addFeatures(map, aws_debilt_wgs.sp, group="AWS")
addFeatures(map, buffer, group="buildings")
clip_bgt(aws_debilt_rd.sp, BGT_station.sp, 100)


