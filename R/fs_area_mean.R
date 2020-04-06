#' Building area calculation
#' 
#' @description Calculate selected metrics of building footprints
#' @param txt Text to append to "foot"
#' @return TBD.
#' @author Chris Jochem
#' 
#' @import data.table
#' 
#' @aliases fs_area_mean
#' @rdname fs_area_mean
#' 
#' @export 
fs_area_mean <- function(X, index=NULL, unit="ha", col=NULL) UseMethod("fs_area_mn")


#' @name fs_area_mean
#' @export
fs_area_mean.sp <- function(X, index=NULL, unit="ha", col=NULL){
  X <- sf::st_as_sf(X)
  
  result <- fs_area_mean(X, index, unit, col)
  return(result)
}


#' @name fs_area_mean
#' @export
fs_area_mean.sf <- function(X, index=NULL, unit="ha", col=NULL){
  if(any(!st_geometry_type(X) %in% c("POLYGON", "MULTIPOLYGON") )){
    message("Area requires polygon shapes.")
    stop()
  }
  
  if(is.null(index)){
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
  
  if(!is.null(col)){
    if(!col %in% names(X)){
      message("Error: column name not found.")
      stop()
    } else{
        names(X)[which(names(X)==col)] <- "fs_area"
        fs_area_mean_calc(X, index, unit)
    }
  } else{
      X[["fs_area"]] <- fs_area(X, unit)
      fs_area_mean_calc(X, index, unit)
  }
}


fs_area_mean_calc <- function(X, index, unit='ha'){
  if(!"fs_area" %in% names(X)){
    X[["fs_area"]] <- fs_area(X, unit)
  }
  
  colNam <- paste0("fs_area_", unit, "_mean")
  DT <- data.table::data.table(index=index, 
                               area_calc=X[["fs_area"]])
  data.table::setkey(DT, index)
  result <- DT[, setNames(.(mean(area_calc)), colNam), by=index]
  
  return(result)
}
