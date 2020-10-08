#' Building area coefficient of variation (CV)
#' 
#' @description Calculate and summarise selected metrics of building 
#' footprint polygons within zones.
#' @inheritParams fs_area_mean
#' @return \code{data.table} of zonal indices and values
#' 
#' @import data.table
#' 
#' @aliases fs_area_cv
#' @rdname fs_area_cv
#' 
#' @export 
fs_area_cv <- function(X, index=NULL, unit=NULL, col=NULL) UseMethod("fs_area_cv")


#' @name fs_area_cv
#' @export
fs_area_cv.sp <- function(X, index=NULL, unit=NULL, col=NULL){
  X <- sf::st_as_sf(X)
  
  result <- fs_area_cv(X, index, unit, col)
  return(result)
}


#' @name fs_area_cv
#' @export
fs_area_cv.sf <- function(X, index=NULL, unit=NULL, col=NULL){
  if(!is.null(col)){
    if(!col %in% names(X)){
      message("Error: column name not found.")
      stop()
    } else{
        names(X)[which(names(X)==col)] <- "fs_area"
        result <- fs_area_cv_calc(X, index, unit)
    }
  } else{  # area column supplied
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
      result <- fs_area_cv_calc(X, index, unit)
  }
  return(result)
}


fs_area_cv_calc <- function(X, index, unit=NULL){
  if(!"fs_area" %in% names(X)){
    X[["fs_area"]] <- fs_area(X, unit)
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

  meanDT <- fs_area_mean(X, index=index, unit=unit, col="fs_area")
  sdDT <- fs_area_sd(X, index=index, unit=unit, col="fs_area")
  
  mCol <- paste0("fs_area_", unit, "_mean")
  sdCol <- paste0("fs_area_", unit, "_sd")
  
  DT <- merge(meanDT, sdDT, by="index")
  DT[, area_calc := get(sdCol) / get(mCol), by=index]
  DT[, area_calc := units::drop_units(area_calc)]
  
  colNam <- "fs_area_cv"
  data.table::setkey(DT, index)
  data.table::setnames(DT, "area_calc", colNam)
  
  return(DT[, list(index, fs_area_cv)])
}
