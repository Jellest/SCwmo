#general function

iterate_features = function(x){
  for(i in 1:nrow(x)){
    p = x[i,]
  }
  return(p)
}

calculate_area <- function(sp){
  for(i in 1:nrow(sp)){
    p = sp@data[i,]
    p[i,"Area"] <- gArea(p)
  }
  return(p)
}

CleanGlobEnvir <- function(pattern){
  rm(list = ls(envir=globalenv())[
  grep(pattern, ls(envir=globalenv()))], envir = globalenv())
}
