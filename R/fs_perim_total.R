#' Building area total calculation
#' 
#' @description Calculate selected metrics of building footprints
#' @param txt Text to append to "foot"
#' @return TBD.
#' @author Chris Jochem
#' 
#' @import data.table
#' 
#' @aliases fs_perim_total
#' @rdname fs_perim_total
#' 
#' @export 
fs_perim_total <- function(X, index=NULL, unit=NULL, col=NULL) UseMethod("fs_perim_total")


#' @name fs_perim_total
#' @export
fs_perim_total.sp <- function(X, index=NULL, unit=NULL, col=NULL){
  X <- sf::st_as_sf(X)
  
  result <- fs_perim_total(X, index, unit, col)
  return(result)
}


#' @name fs_perim_total
#' @export
fs_perim_total.sf <- function(X, index=NULL, unit=NULL, col=NULL){
  if(any(!st_geometry_type(X) %in% c("POLYGON", "MULTIPOLYGON") )){
    message("Perimeter requires polygon shapes.")
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
      unit <- "m"
    }
  }
  
  if(!is.null(col)){
    if(!col %in% names(X)){
      message("Error: column name not found.")
      stop()
    } else{
        names(X)[which(names(X)==col)] <- "fs_perim"
        result <- fs_perim_total_calc(X, index, unit)
    }
  } else{
      X[["fs_perim"]] <- fs_perim(X, unit)
      result <- fs_perim_total_calc(X, index, unit)
  }
  return(result)
}


fs_perim_total_calc <- function(X, index, unit=NULL){
  if(!"fs_perim" %in% names(X)){
    X[["fs_perim"]] <- fs_perimeter(X, unit)
  } else{
    unit <- st_crs(buildings)$units
  }
  
  colNam <- paste0("fs_perim_", unit, "_total")
  DT <- data.table::data.table(index=index, 
                               area_calc=X[["fs_perim"]])
  data.table::setkey(DT, index)
  result <- DT[, setNames(.(sum(area_calc)), colNam), by=index]
  
  return(result)
}
