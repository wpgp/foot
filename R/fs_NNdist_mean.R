#' Nearest neighbour mean distance
#' 
#' @description Calculate and summarise selected metrics of building 
#' footprint polygons within zones.
#' @inheritParams fs_area_mean
#' @return \code{data.table} of zonal indices and values.
#' 
#' @import data.table
#' 
#' @aliases fs_nndist_mean
#' @rdname fs_nndist_mean
#' 
#' @export 
fs_nndist_mean <- function(X, index=NULL, unit=NULL, col=NULL) UseMethod("fs_nndist_mean")


#' @name fs_nndist_mean
#' @export
fs_nndist_mean.sp <- function(X, index=NULL, unit=NULL, col=NULL){
  X <- sf::st_as_sf(X)
  
  result <- fs_nndist_mean(X, index, unit, col)
  return(result)
}


#' @name fs_nndist_mean
#' @export
fs_nndist_mean.sf <- function(X, index=NULL, unit=NULL, col=NULL){
  if(!is.null(col)){
    if(!col %in% names(X)){
      message("Error: column name not found.")
      stop()
    } else{
        names(X)[which(names(X)==col)] <- "fs_nndist"
        result <- fs_nndist_mean_calc(X, index, unit)
    }
  } else{
      if(is.na(sf::st_crs(X))){
        warning("Objects have no spatial projection. Units ignored.")
        unit <- NULL
        
      } else{
        if(is.null(unit)){
          unit <- "m"
        }
      }
    
      X[["fs_nndist"]] <- fs_nndist(X, unit=unit) # using default search radius
      result <- fs_nndist_mean_calc(X, index, unit)
  }
  return(result)
}


fs_nndist_mean_calc <- function(X, index=NULL, unit=NULL){
  if(!"fs_nndist" %in% names(X)){
    X[["fs_nndist"]] <- fs_nndist(X, unit=unit) # default search radius
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
  
  colNam <- paste0("fs_nndist_", unit, "_mean")
  DT <- data.table::data.table(index=index, 
                               area_calc=X[["fs_nndist"]])
  data.table::setkey(DT, index)
  result <- DT[, setNames(.(mean(area_calc)), colNam), by=index]
  
  return(result)
}
