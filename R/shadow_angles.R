#'
#'
#'@title shadow angles
#'@description calculates the angles of to the obstacles for a point
#'@param sp
#'@param grid
#'@examples
#'#kaartblad AHN3 for the bilt equals 32cn1
#'#downloadlink: 
#'@export
#'
shadow_angles<-function(sp,
                        grid){
  requireNamespace("raster")
  #checking coord ref
  #(1) spatial points with epsg and elv
  
  #(2) grid with epsg and elv 
  #?sp::spTransform
  #?raster::crs
  
  #(3) sp and grid equal? overlap?
  
  #raster::extract(x = grid, y = sp, method=bilinear)
  #check if xyz match
  
  #if everything fits/no errors
  #crop and mask raster with a max distance from the points 
  #horizon_grid<-horizon::horizonSearch(grid)
  
  #sp_theta_azimuth<-extract(x = horizon_grid, y = sp, method=bilinear)
  
  #return(sp_theta_azimuth)
}