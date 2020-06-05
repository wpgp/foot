#' Binary indicator of settled status
#' 
#' @description Calculate and summarise selected metrics of building 
#' footprint polygons within zones.
#' @inheritParams fs_area_mean
#' @return \code{data.table} of zonal indices and values.
#' 
#' @import data.table
#' 
#' @aliases fs_settled
#' @rdname fs_settled
#' 
#' @export 
fs_settled <- function(X, index=NULL, col=NULL) UseMethod("fs_settled")


#' @name fs_settled
#' @export
fs_settled.sp <- function(X, index=NULL, col=NULL){
  X <- sf::st_as_sf(X)
  
  result <- fs_settled(X, index, col)
  return(result)
}


#' @name fs_settled
#' @export
fs_settled.sf <- function(X, index=NULL, col=NULL){
  if(!is.null(col)){
    if(!col %in% names(X)){
      message("Error: column name not found.")
      stop()
    } else{
        names(X)[which(names(X)==col)] <- "fs_settled"
        result <- fs_settled_calc(X, index)
    }
  } else{
      X[["fs_settled"]] <- 1
      result <- fs_settled_calc(X, index)
  }
  return(result)
}


fs_settled_calc <- function(X, index){
  if(!"fs_settled" %in% names(X)){
    X[["fs_settled"]] <- 1
  }
  
  if(is.null(index)){
    warning("No index found, treating as one group.")
    index <- rep(1, nrow(X))
  } else{
    if(length(index)==1){
      if((is.numeric(index) & index <= ncol(X)) | 
         (is.character(index) & index %in% names(X))){
        index <- X[[index]]
      }
    } else if(length(index) != nrow(X)){
      message("Invalid index")
      stop()
    }
  } 
  
  colNam <- "fs_settled"  # changed name from _binary
  DT <- data.table::data.table(index=index, 
                               settled=X[["fs_settled"]])
  data.table::setkey(DT, index)
  result <- DT[, setNames(.(max(settled)), colNam), by=index]

  return(result)
}
