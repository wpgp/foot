#' Building perimeter mean calculation
#' 
#' @description Calculate and summarise selected metrics of building 
#' footprint polygons within zones.
#' @inheritParams fs_area_mean
#' @return \code{data.table} of zonal indices and values
#' 
#' @import data.table
#' 
#' @aliases fs_area_median
#' @rdname fs_area_median
#' 
#' @export 
fs_area_median <- function(X, index=NULL, unit=NULL, col=NULL) UseMethod("fs_area_median")


#' @name fs_area_median
#' @export
fs_area_median.sp <- function(X, index=NULL, unit=NULL, col=NULL){
  X <- sf::st_as_sf(X)
  
  result <- fs_area_median(X, index, unit, col)
  return(result)
}


#' @name fs_area_median
#' @export
fs_area_median.sf <- function(X, index=NULL, unit=NULL, col=NULL){
  if(!is.null(col)){
    if(!col %in% names(X)){
      message("Error: column name not found.")
      stop()
    } else{
      names(X)[which(names(X)==col)] <- "fs_area"
      result <- fs_area_median_calc(X, index, unit)
    }
  } else{
    if(any(!sf::st_geometry_type(X) %in% c("POLYGON", "MULTIPOLYGON") )){
      message("Area requires polygon shapes.")
      stop()
    }
    
    if(is.na(sf::st_crs(X))){
      warning("Polygons have no spatial projection. Units ignored.")
      unit <- NULL
      
    } else{
      if(is.null(unit)){
        unit <- "ha"
      }
    }
    
    X[["fs_area"]] <- fs_area(X, unit)
    result <- fs_area_median_calc(X, index, unit)
  }
  return(result)
}


fs_area_median_calc <- function(X, index=NULL, unit=NULL){
  if(!"fs_area" %in% names(X)){
    X[["fs_area"]] <- fs_area(X, unit)
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
  
  colNam <- paste0("fs_area_", unit, "_median")
  DT <- data.table::data.table(index=index, 
                               area_calc=X[["fs_area"]])
  data.table::setkey(DT, index)
  result <- DT[, setNames(.(median(area_calc)), colNam), by=index]
  
  return(result)
}
