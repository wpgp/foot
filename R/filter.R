#' Circular Filter
#' 
#' Filters for gridded data
#' 
#' @description Make an approximately circular filter inside of an 'n x n' matrix.
#' @param size The length of the side of a square matrix. The circular window has a radius of size/2.
#' @return Matrix with values inside the circle set to 1, the center set to 0, and corners as NA.
#' make weights window
#' based on: https://stackoverflow.com/questions/54742340/r-extract-a-circle-from-a-matrix
#' @name circularFilter
#' @export
make_circular_filter <- function(size=5){
  g <- expand.grid(1:size, 1:size)
  ctr <- c(size/2, size/2) + .5
  
  g$d2 <- sqrt((g$Var1-ctr[2])^2 + (g$Var2-ctr[1])^2)
  g$inside <- g$d2 <= size/2
  
  w <- matrix(NA, size, size)
  w[as.matrix(g[g$inside, c("Var1","Var2")])] <- 1
  w[ctr[1], ctr[2]] <- 0
  return(w)
}