#' Count of footprint locations per zone
#' 
#' @description Calculate and summarise selected metrics of building 
#' footprints within zones.
#' @param X Spatial object with building footprint polygons
#' @param index A string identifying a column within \code{X} which provides a
#'   zonal index for summarising values. Alternatively a vector of indices can
#'   be provided. If omitted, all observations in \code{X} are assumed to be
#'   within one zone.
#' @param unit character or \code{units} object to define area. Default is
#'   \code{NULL} which will use the units of the spatial reference system
#' @param col column name within \code{X} with pre-calculated area measures
#' @return \code{data.table} of zonal indices and values.
#' 
#' @details Note that this function is provided as a standalone calculation for
#'   convenience. The same summary measure can be executed within
#'   \code{calculate_footstats} by specifying \code{what='settled'} and
#'   \code{how='count'}.
#' 
#' @import data.table
#' 
#' @aliases fs_count
#' @rdname fs_count
#' 
#' @export 
fs_count <- function(X, index=NULL, col=NULL) UseMethod("fs_count")


#' @name fs_count
#' @export
fs_count.sp <- function(X, index=NULL, col=NULL){
  X <- sf::st_as_sf(X)
  
  result <- fs_count(X, index, col)
  return(result)
}


#' @name fs_count
#' @export
fs_count.sf <- function(X, index=NULL, col=NULL){
  if(!is.null(col)){
    if(!col %in% names(X)){
      message("Error: column name not found.")
      stop()
    } else{
        names(X)[which(names(X)==col)] <- "fs_count"
        result <- fs_count_calc(X, index)
    }
  } else{
      X[["fs_count"]] <- 1
      result <- fs_count_calc(X, index)
  }
  return(result)
}


fs_count_calc <- function(X, index){
  if(!"fs_count" %in% names(X)){
    X[["fs_count"]] <- 1
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
  
  colNam <- "fs_count"
  DT <- data.table::data.table(idxCol=index, 
                               area_calc=X[["fs_count"]])
  data.table::setnames(DT, "idxCol", indexCol)
  data.table::setkeyv(DT, indexCol)
  result <- DT[, setNames(.(sum(area_calc)), colNam), by=indexCol]
  
  return(result)
}
