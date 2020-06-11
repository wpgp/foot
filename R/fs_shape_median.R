#' Building shape measure
#' 
#' @description Calculate and summarise selected metrics of building footprint
#'   polygons within zones.
#' @inheritParams fs_area_mean
#' @return \code{data.table} of zonal indices and values
#' 
#' @details The shape index is calculated as the ratio of footprint polygon's
#'   area to the area of minimum bounding circle. The mbc is calculated using
#'   \code{fs_mbc} and \code{lwgeom}. Values closer 1 suggest more rounded
#'   shapes.
#' 
#' The function first looks for pre-calculated values of area in a field called
#' \code{fs_area}. If not present, the areas are calculated in m^2.
#'  
#' @import data.table
#' 
#' @aliases fs_shape_median
#' @rdname fs_shape_median
#' 
#' @export 
fs_shape_median <- function(X, 
                            index=NULL, 
                            unit=NULL, 
                            col=NULL) UseMethod("fs_shape_median")


#' @name fs_shape_median
#' @export
fs_shape_median.sp <- function(X, index=NULL, unit=NULL, col=NULL){
  X <- sf::st_as_sf(X)
  
  result <- fs_shape_median(X, index, unit, col)
  return(result)
}


#' @name fs_shape_median
#' @export
fs_shape_median.sf <- function(X, index=NULL, unit=NULL, col=NULL){
  if(!is.null(col)){
    # warning("Ignoring supplied column.")
    if(!col %in% names(X)){
      message("Error: column name not found.")
      stop()
    } else{
      names(X)[which(names(X)==col)] <- "fs_shape"
      result <- fs_shape_median_calc(X, index, unit)
    }
  } else{  # column not supplied
    if(any(!sf::st_geometry_type(X) %in% c("POLYGON", "MULTIPOLYGON") )){
      message("Area requires polygon shapes.")
      stop()
    }
    
    if(is.na(sf::st_crs(X))){
      warning("Polygons have no spatial projection. Units ignored.")
      unit <- NULL
    } else{
      if(is.null(unit)){
        unit <- "m^2"
      }
    }
    
    X[["fs_shape"]] <- fs_shape(X)
    result <- fs_shape_median_calc(X, index, unit)
  }
  return(result)
}


fs_shape_median_calc <- function(X, index, unit=NULL){
  if(!"fs_shape" %in% names(X)){
    if(!"fs_area" %in% names(X)){
      X[["fs_area"]] <- fs_area(X, "m^2")
    } else{
      units(X[["fs_area"]]) <- "m^2"
    }
    
    X[["fs_shape"]] <- fs_shape(X)
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
  
  colNam <- "fs_shape_median"
  DT <- data.table::data.table(index=index, 
                               area_calc=X[["fs_shape"]])
  data.table::setkey(DT, index)
  result <- DT[, setNames(.(median(area_calc)), colNam), by=index]
  
  return(result)
}
