
expargs <- function(a, b, argNames=c('cols','funs'), onlyUnique=TRUE){
  if(is.fs_varlist(a)) a <- list(a)
  if(is.fs_varlist(b)) stop("Functions cannot contain a 'varlist'.")
  df <- expand.grid(a, b, stringsAsFactors=F)
  names(df) <- argNames
  
  if(onlyUnique){
    df <- unique(df)
    rownames(df) <- 1:nrow(df)
  }
  return(df)
}


crossargs <- function(a, b, argNames=c('cols','funs'), ...){
  da <- depth(as.list(a))
  db <- depth(as.list(b))
  if(da != db) stop("Number of argument groups does not match.")
  
  if(da <= 1){
    args <- list(do.call(expargs, list(a, b, argNames)))
  } else{
    args <- Map(expargs, a, b, 
                argNames=replicate(length(as.list(a)), argNames, simplify=F))
  }
  return(args)
}


# https://stackoverflow.com/questions/13432863/determine-level-of-nesting-in-r
depth <- function(this, thisdepth=0){
  if(!is.list(this)){
    return(thisdepth)
  } else{
    return(max(unlist(lapply(this, depth, thisdepth = thisdepth + 1))))    
  }
}