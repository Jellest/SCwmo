perim <- function(){
  perim <- st_read(dsn = "data/coordinates/aws_Vlissingen_perim.shp")
  
  #ahn <- raster("output/Vlissingen/solar_shadow_angles/rasters/AHN3/Shadows/Vlissingen_AHN3_sa_171.526696898183.tif")
  ahn <- raster("data/AHN3/Vlissingen/raw/Vlissingen_AHN3_raw_ahn.tif")
  
  mask <- raster::mask(ahn, perim)
  crs(mask) <- epsg_rd
  writeRaster(mask, filename = "data/AHN3/Vlissingen/raw/Vlissingen_aws_AHN3_raw_ahn.tif", overwrite = TRUE)
  #plot(mask)
return (mask)}

perim()


ahn_diff <- perim()
ahn2_sa <- perim() 
ahn3_sa <- perim()
plot(ahn_diff, xlim=c(30455,30475), ylim=c(385080,385140))


stack <- raster::stack(ahn_diff, ahn2_sa, ahn3_sa)

plot(stack, xlim=c(30455,30475), ylim=c(385080,385140))

aws_perim <- function(){

  ahn2_Vlissingen <- raster("output/Vlissingen/solar_shadow_angles/rasters/aws_AHN2_Vlissingen.tif")
  ahn3_Vlissingen <- raster("output/Vlissingen/solar_shadow_angles/rasters/aws_AHN3_Vlissingen.tif")
  
  cuts=c(7,8,9,10)
  pal <- colorRampPalette(c("yellow","red"))
  #plot(ahn3_Vlissingen, breaks=cuts, col = pal(5), xlim=c(30455,30475), ylim=c(385080,385140))
  
  stack <- stack(ahn2_Vlissingen, ahn3_Vlissingen)
  names(stack) <- c("AHN2", "AHN3")
  plot(stack, xlim=c(30455,30475), ylim=c(385090,385134), breaks=cuts, col = pal(5),
       axes = FALSE, box = FALSE)

}

aws_perim()

vegetation_raster <- function(){
  cuts=c(0, 0.1,0.2,0.3,0.4,0.5,0.6,0.7)
  pal <- colorRampPalette(c("#edf8e9","#238b45"))
  plot(raster("output/DeBilt/vegetation_height/DeBilt_AHN3_10m_height_difference.tif"),
       xlim=c(140750,140770), ylim=c(456745,456765),
       breaks=cuts, col = pal(8))
}

vegetation_raster()

aws_perim_lp <- function(){
  ahn2_Vlissingen <- raster("output/Vlissingen/solar_shadow_angles/rasters/aws_AHN2_Vlissingen.tif")
  ahn3_Vlissingen <- raster("output/Vlissingen/solar_shadow_angles/rasters/aws_AHN3_Vlissingen.tif")
  
  pal <- colorRampPalette(c("yellow","red"))
  pal2<- colorRampPalette(c("#ffffb2","#fecc5c", "#fd8d3c", "#f03b20", "#bd0026"))
  stack <- stack(ahn2_Vlissingen, ahn3_Vlissingen)
  levelplot(stack, 
            margin=FALSE,                       
            colorkey=list(
              space="right",                   
              labels=list(at=7:10, font=4),
              #at=c(7:10),
              axis.line=list(col="black"),
              width=0.75
            ),    
            par.settings=list(
              strip.border=list(col="transparent"),
              strip.background=list(col="transparent"),
              axis.line=list(col="transparent")
            ),
            scales=list(draw=FALSE),            
            region=TRUE,
            col.regions=pal2,                   
            at=seq(7, 10, len=100),
            #names.attr=c('', ''),
            #names.attr=c("AHN2 (2007)", "AHN3 (2014)"),
            xlim=c(30455,30475),
            ylim=c(385090,385123),
            maxpixels = 2e9)
}

aws_rasters <- aws_perim_lp()
aws_perim_lp()
png(filename = "aws_rasters.png")

ahn_diff_lp <- function(){
  ahn2_Rotterdam <- raster("data/AHN2/Rotterdam/raw/Rotterdam_AHN2_raw_ahn.tif")
  ahn3_Rotterdam <- raster("data/AHN3/Rotterdam/raw/Rotterdam_AHN3_raw_ahn.tif")
  
  pal <- colorRampPalette(c("yellow","red"))
  
  stack <- stack(ahn2_Rotterdam, ahn3_Rotterdam)
  levelplot(stack, 
            margin=FALSE,                       
            colorkey=list(
              space="right",                   
              labels=list(at=-6:1, font=4),
              #at=c(7:10),
              axis.line=list(col="black"),
              width=0.75
            ),    
            par.settings=list(
              strip.border=list(col="transparent"),
              strip.background=list(col="transparent"),
              axis.line=list(col="transparent")
            ),
            scales=list(draw=TRUE),            
            region=TRUE,
            col.regions=pal,                   
            at=seq(-0.8, 1, len=1000),
            #names.attr=c('', ''),
            names.attr=c("AHN2 (2008)", "AHN3 (2014)"),
            xlim=c(90360,90400),
            ylim=c(441900,442000),
            maxpixels = 2e9)
}


ahn_diff_lp()



plot(raster("output/DeBilt/vegetation_height/DeBilt_AHN3_10m_height_difference.tif"))
