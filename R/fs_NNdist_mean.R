#' Nearest neighbour mean distance
#' 
#' @description Calculate and summarise selected metrics of building 
#' footprint polygons within zones.
#' @inheritParams fs_area_mean
#' @param ... Additional arguments to pass to \code{fs_nndist}.
#' @return \code{data.table} of zonal indices and values.
#' 
#' @import data.table
#' 
#' @aliases fs_nndist_mean
#' @rdname fs_nndist_mean
#' 
#' @export 
fs_nndist_mean <- function(X, index=NULL, 
                           unit=NULL, col=NULL, ...) UseMethod("fs_nndist_mean")


#' @name fs_nndist_mean
#' @export
fs_nndist_mean.sp <- function(X, index=NULL, 
                              unit=NULL, col=NULL, ...){
  X <- sf::st_as_sf(X)
  
  result <- fs_nndist_mean(X, index, unit, col, ...)
  return(result)
}


#' @name fs_nndist_mean
#' @export
fs_nndist_mean.sf <- function(X, index=NULL, unit=NULL, col=NULL, ...){
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
    
      X[["fs_nndist"]] <- fs_nndist(X, unit=unit, ...) 
      result <- fs_nndist_mean_calc(X, index, unit)
  }
  return(result)
}


fs_nndist_mean_calc <- function(X, index=NULL, unit=NULL, ...){
  if(!"fs_nndist" %in% names(X)){
    X[["fs_nndist"]] <- fs_nndist(X, unit=unit, ...)
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
  
  colNam <- paste0("fs_nndist_", unit, "_mean")
  DT <- data.table::data.table(idxCol=index, 
                               area_calc=X[["fs_nndist"]])
  data.table::setnames(DT, "idxCol", indexCol)
  data.table::setkeyv(DT, indexCol)
  result <- DT[, setNames(.(mean(area_calc)), colNam), by=indexCol]
  
  return(result)
}
