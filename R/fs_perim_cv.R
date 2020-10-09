#' Building perimeter coefficient of variation (CV)
#' 
#' @description Calculate and summarise selected metrics of building 
#' footprint polygons within zones.
#' #' @inheritParams fs_area_mean
#' @return \code{data.table} of zonal indices and values
#' 
#' @import data.table
#' 
#' @aliases fs_perim_cv
#' @rdname fs_perim_cv
#' 
#' @export 
fs_perim_cv <- function(X, index=NULL, unit=NULL, col=NULL) UseMethod("fs_perim_cv")


#' @name fs_perim_cv
#' @export
fs_perim_cv.sp <- function(X, index=NULL, unit=NULL, col=NULL){
  X <- sf::st_as_sf(X)
  
  result <- fs_perim_cv(X, index, unit, col)
  return(result)
}


#' @name fs_perim_cv
#' @export
fs_perim_cv.sf <- function(X, index=NULL, unit=NULL, col=NULL){
  if(!is.null(col)){
    if(!col %in% names(X)){
      message("Error: column name not found.")
      stop()
    } else{
        names(X)[which(names(X)==col)] <- "fs_perim"
        result <- fs_perim_cv_calc(X, index, unit)
    }
  } else{
      if(any(!sf::st_geometry_type(X) %in% c("POLYGON", "MULTIPOLYGON") )){
        message("Perimeter requires polygon shapes.")
        stop()
      }
      
      if(is.na(sf::st_crs(X))){
        warning("Polygons have no spatial projection. Units ignored.")
        unit <- NULL
        
      } else{
        if(is.null(unit)){
          unit <- "m"
        }
      }
    
      X[["fs_perim"]] <- fs_perimeter(X, unit)
      result <- fs_perim_cv_calc(X, index, unit)
  }
  return(result)
}


fs_perim_cv_calc <- function(X, index=NULL, unit=NULL){
  if(!"fs_perim" %in% names(X)){
    X[["fs_perim"]] <- fs_perimeter(X, unit)
  } 
  
  indexCol <- "index" # default
  
  if(is.null(index)){
    message("No index found, treating as one group.")
    index <- rep(1, nrow(X))
  } else{
    if(is.character(index)){ 
      if(length(index)==1){ 
        if(nrow(X)>1){ # it must be a column name
          if(!index %in% colnames(X)){
            stop("Index column not found in footprints.")
          } else{
            indexCol <- index
            index <- X[[indexCol]]
          }
        } # potential issue if 1 row X and 1 column name - won't affect calcs
      } else if(length(index != nrow(X))){
        stop("Invalid length of zonal index.")
      } 
    } else if(is.numeric(index)){
      if(length(index) != nrow(X)){
        stop("Invalid length of zonal index.")
      }
    }
  } 
  
  X[[indexCol]] <- index
  
  meanDT <- fs_perim_mean(X, index=indexCol, unit=unit, col="fs_perim")
  sdDT <- fs_perim_sd(X, index=indexCol, unit=unit, col="fs_perim")
  
  mCol <- paste0("fs_perim_", unit, "_mean")
  sdCol <- paste0("fs_perim_", unit, "_sd")
  
  DT <- merge(meanDT, sdDT, by=indexCol)
  DT[, area_calc := get(sdCol) / get(mCol), by=indexCol]
  DT[, area_calc := units::drop_units(area_calc)]
  
  colNam <- "fs_perim_cv"
  data.table::setkeyv(DT, indexCol)
  data.table::setnames(DT, "area_calc", colNam)
  
  keep <- c(indexCol, colNam)
  return(DT[, ..keep])
}
