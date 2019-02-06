tmp <- function(){
  adjacents <- matrix(c(1, 1, 1, 1, 1, 
                   1, 1, 1, 1, 1,
                   1, 1, 0, 1, 1,
                   1, 1, 1, 1, 1,
                   1, 1, 1, 1, 1
                   ), ncol=5, byrow=TRUE
                 )
  adjacentsM <- matrix(1, nrow=21, ncol=21)
  adjacentsM[221] <- 0
  #print(adjacentsM)
  r  <- matrix(2, nrow=21, ncol=21)
  adj <- adjacent(r, cells=c(221), directions=adjacentsM, pairs=FALSE, include = TRUE, id= TRUE)
  for(v in 1:ncell(adjacentsM)){
    print(adjacentsM[v])
  }
  #print(adj)
 
}

tmp()
