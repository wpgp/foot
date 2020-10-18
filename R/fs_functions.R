
cv <- function(x){
  return(sd(x) / mean(x))
}


majority <- function(x){
  return(names(which.max(table(x))))
}


binary <- function(x){
  return(ifelse(is.null(x), 0, 1))
}


count <- function(x){
  return(length(x))
}


nnindex <- function(x){
  
}