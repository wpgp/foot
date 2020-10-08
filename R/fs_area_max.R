#' Building area maximum calculation
#' 
#' @description Calculate and summarise selected metrics of building 
#' footprint polygons within zones.
#' #' @inheritParams fs_area_mean
#' @return \code{data.table} of zonal indices and values
#' 
#' @import data.table
#' 
#' @aliases fs_area_max
#' @rdname fs_area_max
#' 
#' @export 
fs_area_max <- function(X, 
                        index=NULL, 
                        unit=NULL, 
                        col=NULL) UseMethod("fs_area_max")


#' @name fs_area_max
#' @export
fs_area_max.sp <- function(X, index=NULL, unit=NULL, col=NULL){
  X <- sf::st_as_sf(X)
  
  result <- fs_area_max(X, index, unit, col)
  return(result)
}


#' @name fs_area_max
#' @export
fs_area_max.sf <- function(X, index=NULL, unit=NULL, col=NULL){
  if(!is.null(col)){
    if(!col %in% names(X)){
      message("Error: column name not found.")
      stop()
    } else{
      names(X)[which(names(X)==col)] <- "fs_area"
      result <- fs_area_max_calc(X, index, unit)
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
        unit <- "m"
      }
    }
    
    X[["fs_area"]] <- fs_area(X, unit)
    result <- fs_area_max_calc(X, index, unit)
  }
  return(result)
}


fs_area_max_calc <- function(X, index=NULL, unit=NULL){
  if(!"fs_area" %in% names(X)){
    X[["fs_area"]] <- fs_area(X, unit)
  } else{
    unit <- st_crs(X)$units
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
  
  colNam <- paste0("fs_area_", unit, "_max")
  DT <- data.table::data.table(idxCol=index, 
                               area_calc=X[["fs_area"]])
  data.table::setnames(DT, "idxCol", indexCol)
  data.table::setkeyv(DT, indexCol)
  result <- DT[, setNames(.(max(area_calc)), colNam), by=indexCol]
  
  return(result)
}
