#' Building area total calculation
#' 
#' @description Calculate selected metrics of building footprints
#' @param txt Text to append to "foot"
#' @return TBD.
#' @author Chris Jochem
#' 
#' @import data.table
#' 
#' @aliases fs_area_total
#' @rdname fs_area_total
#' 
#' @export 
fs_area_total <- function(X, index=NULL, unit=NULL, col=NULL) UseMethod("fs_area_total")


#' @name fs_area_total
#' @export
fs_area_total.sp <- function(X, index=NULL, unit=NULL, col=NULL){
  X <- sf::st_as_sf(X)
  
  result <- fs_area_total(X, index, unit, col)
  return(result)
}


#' @name fs_area_total
#' @export
fs_area_total.sf <- function(X, index=NULL, unit=NULL, col=NULL){
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
  
  if(is.na(st_crs(X))){
    warning("Polygons have no spatial projection. Units ignored.")
    unit <- NULL
    
  } else{
    if(is.null(unit)){
      unit <- "ha"
    }
  }
  
  if(!is.null(col)){
    if(!col %in% names(X)){
      message("Error: column name not found.")
      stop()
    } else{
        names(X)[which(names(X)==col)] <- "fs_area"
        result <- fs_area_total_calc(X, index, unit)
    }
  } else{
      X[["fs_area"]] <- fs_area(X, unit)
      result <- fs_area_total_calc(X, index, unit)
  }
  return(result)
}


fs_area_total_calc <- function(X, index, unit=NULL){
  if(!"fs_area" %in% names(X)){
    X[["fs_area"]] <- fs_area(X, unit)
  }
  
  colNam <- paste0("fs_area_", unit, "_total")
  DT <- data.table::data.table(index=index, 
                               area_calc=X[["fs_area"]])
  data.table::setkey(DT, index)
  result <- DT[, setNames(.(sum(area_calc)), colNam), by=index]
  
  return(result)
}
