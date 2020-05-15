#' Building compactness measures
#' 
#' @description Calculate and summarise selected metrics of building footprint
#'   polygons within zones.
#' @inheritParams fs_area_mean
#' @return \code{data.table} of zonal indices and values
#' @author Chris Jochem
#' 
#' @details The compactness measure is the Polsby-Popper test. This is
#'   summarised as the mean for all footprints in the zone identified by
#'   \code{index}.
#'   
#'   \deqn{ PP_z = \frac{4$\pi$ * A_z}{P_z^2} },
#'   where P and A are the perimeter and area of zone 'z', respectively.
#'   
#' @source Polsby, Daniel D., and Robert D. Popper. 1991. “The Third Criterion:
#'   Compactness as a procedural safeguard against partisan gerrymandering.”
#'   Yale Law & Policy Review 9 (2): 301–353.
#' 
#' @import data.table
#' 
#' @aliases fs_compact_mean
#' @rdname fs_compact_mean
#' 
#' @export 
fs_compact_mean <- function(X, 
                            index=NULL, 
                            unit=NULL, 
                            col=NULL) UseMethod("fs_compact_mean")


#' @name fs_compact_mean
#' @export
fs_compact_mean.sp <- function(X, index=NULL, unit=NULL, col=NULL){
  X <- sf::st_as_sf(X)
  
  result <- fs_compact_mean(X, index, unit, col)
  return(result)
}


#' @name fs_compact_mean
#' @export
fs_compact_mean.sf <- function(X, index=NULL, unit=NULL, col=NULL){
  if(!is.null(col)){
    # warning("Ignoring supplied column.")
    if(!col %in% names(X)){
      message("Error: column name not found.")
      stop()
    } else{
      names(X)[which(names(X)==col)] <- "fs_compact"
      result <- fs_compact_mean_calc(X, index, unit)
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

    X[["fs_compact"]] <- fs_compact(X)
    result <- fs_compact_mean_calc(X, index, unit)
  }
  return(result)
}


fs_compact_mean_calc <- function(X, index, unit=NULL){
  if(!"fs_compact" %in% names(X)){
    if(!"fs_area" %in% names(X)){
      X[["fs_area"]] <- fs_area(X, "m^2")
    } else{
      units(X[["fs_area"]]) <- "m^2"
    }
    
    if(!"fs_perimeter" %in% names(X)){
      X[["fs_perimeter"]] <- fs_perimeter(X, "m")
    } else{
      units(X[["fs_perimeter"]]) <- "m"
    }
    
    X[["fs_compact"]] <- fs_compact(X)
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
  
  colNam <- "fs_compact_mean"
  DT <- data.table::data.table(index=index, 
                               area_calc=X[["fs_compact"]])
  data.table::setkey(DT, index)
  result <- DT[, setNames(.(mean(area_calc)), colNam), by=index]
  
  return(result)
}
