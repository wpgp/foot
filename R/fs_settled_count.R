#' Count of locations per pixel
#' 
#' @description Calculate selected metrics of building footprints
#' @param txt Text to append to "foot"
#' @return TBD.
#' @author Chris Jochem
#' 
#' @import data.table
#' 
#' @aliases fs_count
#' @rdname fs_count
#' 
#' @export 
fs_count <- function(X, index=NULL, col=NULL) UseMethod("fs_count")


#' @name fs_count
#' @export
fs_count.sp <- function(X, index=NULL, col=NULL){
  X <- sf::st_as_sf(X)
  
  result <- fs_count(X, index, col)
  return(result)
}


#' @name fs_count
#' @export
fs_count.sf <- function(X, index=NULL, col=NULL){
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
  
  if(!is.null(col)){
    if(!col %in% names(X)){
      message("Error: column name not found.")
      stop()
    } else{
        names(X)[which(names(X)==col)] <- "fs_count"
        result <- fs_count_calc(X, index)
    }
  } else{
      X[["fs_count"]] <- 1
      result <- fs_count_calc(X, index)
  }
  return(result)
}


fs_count_calc <- function(X, index){
  if(!"fs_count" %in% names(X)){
    X[["fs_count"]] <- 1
  }
  
  colNam <- "fs_count"
  DT <- data.table::data.table(index=index, 
                               settled=X[["fs_count"]])
  data.table::setkey(DT, index)
  result <- DT[, setNames(.(sum(settled)), colNam), by=index]

  return(result)
}
