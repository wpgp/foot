#' Binary settled status
#' 
#' @description Calculate selected metrics of building footprints
#' @param txt Text to append to "foot"
#' @return TBD.
#' @author Chris Jochem
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
  if(any(!st_geometry_type(X) %in% c("POLYGON", "MULTIPOLYGON") )){
    message("Area requires polygon shapes.")
    stop()
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


fs_settled_calc <- function(X, index, unit="binary"){
  if(!"fs_settled" %in% names(X)){
    X[["fs_settled"]] <- 1
  }
  
  colNam <- paste0("fs_settled_", unit)
  DT <- data.table::data.table(index=index, 
                               area_calc=X[["fs_settled"]])
  data.table::setkey(DT, index)
  result <- DT[, setNames(.(1), colNam), by=index]
  
  return(result)
}
