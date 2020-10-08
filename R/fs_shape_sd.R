#' Building area standard deviation calculation
#' 
#' @description Calculate and summarise selected metrics of building 
#' footprint polygons within zones.
#' @inheritParams fs_area_mean
#' @return \code{data.table} of zonal indices and values
#' 
#' @import data.table
#' 
#' @aliases fs_shape_sd
#' @rdname fs_shape_sd
#' 
#' @export 
fs_shape_sd <- function(X, index=NULL, 
                        unit=NULL, col=NULL) UseMethod("fs_shape_sd")


#' @name fs_shape_sd
#' @export
fs_shape_sd.sp <- function(X, index=NULL, unit=NULL, col=NULL){
  X <- sf::st_as_sf(X)
  
  result <- fs_shape_sd(X, index, unit, col)
  return(result)
}


#' @name fs_shape_sd
#' @export
fs_shape_sd.sf <- function(X, index=NULL, unit=NULL, col=NULL){
  if(!is.null(col)){
    if(!col %in% names(X)){
      message("Error: column name not found.")
      stop()
    } else{
      names(X)[which(names(X)==col)] <- "fs_shape"
      result <- fs_shape_sd_calc(X, index, unit)
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
    
    X[["fs_shape"]] <- fs_shape(X, unit)
    result <- fs_shape_sd_calc(X, index, unit)
  }
  return(result)
}


fs_shape_sd_calc <- function(X, index, unit=NULL){
  if(!"fs_shape" %in% names(X)){
    X[["fs_shape"]] <- fs_shape(X, unit)
  }
  
  if(is.null(index)){
    message("No index found, treating as one group.")
    index <- rep(1, nrow(X))
  } else{
    if(length(index)==1){
      if((is.numeric(index) & index <= ncol(X)) | 
         (is.character(index) & index %in% names(X))){
        index <- X[[index]]
      }
    } else if(length(index) != nrow(X)){
      stop("Invalid index")
    }
  } 
  
  colNam <- paste0("fs_shape_sd")
  DT <- data.table::data.table(index=index, 
                               area_calc=X[["fs_shape"]])
  data.table::setkey(DT, index)
  result <- DT[, setNames(.(sd(area_calc)), colNam), by=index]
  
  return(result)
}
