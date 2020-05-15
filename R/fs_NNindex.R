#' Building Nearest Neighbour Index (NNI)
#' 
#' @description Calculate and summarise selected metrics of building footprint
#'   polygons within zones.
#' @param X Spatial object with building footprints or their centroid locations.
#' @param index A spatial polygon object of \code{sf} or \code{sp} type. If
#'   omitted all observations in \code{X} are assumed to be within one zone and
#'   the area of the bounding box is used for the nearest neighbour index.
#' @param unit character or \code{units} object to define distance. Default is
#'   NULL
#' @param col column name or index within \code{X} with pre-calculated distance
#'   measures.
#' @return \code{data.table} of zonal indices and values
#'
#' @details The nearest neighbour index (NNI) is a measure of clustering. It
#'   compares the observed mean neighbour distances with a hypothetical maximum
#'   of dispersed observations given the area of the zone. Note that NNI is
#'   sensitive to changes in the zone area.
#'   
#'   \deqn{ NNI_z = \frac{\bar{NND_z}}{(0.5 * \sqrt{\frac{A_z}{n_z}}})}, 
#'   where z is the zone, A is the area, NND is the mean nearest neighbour
#'   distance, and n is the count.
#' 
#' @author Chris Jochem
#' 
#' @import data.table
#' 
#' @aliases fs_NNindex
#' @rdname fs_NNindex
#' 
#' @export 
fs_NNindex <- function(X, index=NULL, unit=NULL, col=NULL) UseMethod("fs_NNindex")


#' @name fs_NNindex
#' @export
fs_NNindex.sp <- function(X, index=NULL, unit=NULL, col=NULL){
  X <- sf::st_as_sf(X)
  
  result <- fs_NNindex(X, index, unit, col)
  return(result)
}


#' @name fs_NNindex
#' @export
fs_NNindex.sf <- function(X, index=NULL, unit=NULL, col=NULL){
  if(!is.null(col)){
    if(!col %in% names(X)){
      message("Error: column name not found.")
      stop()
    } else{
      names(X)[which(names(X)==col)] <- "fs_NNdist"
      result <- fs_NNindex_calc(X, index, unit)
    }
  } else{  # distance column not supplied
    # if(any(!sf::st_geometry_type(X) %in% c("POLYGON", "MULTIPOLYGON") )){
    #   message("Area requires polygon shapes.")
    #   stop()
    # }
    if(is.na(sf::st_crs(X))){
      warning("Footprints have no spatial projection. Units ignored.")
      unit <- NULL
    } else{
      if(is.null(unit)){
        unit <- "m"
      }
    }
    
    X[["fs_NNdist"]] <- fs_NNdist(X, unit=unit)
    result <- fs_NNindex_calc(X, index, unit)
  }
  return(result)
}


fs_NNindex_calc <- function(X, index, unit=NULL){
  if(!"fs_NNdist" %in% names(X)){
    X[["fs_NNdist"]] <- fs_NNdist(X, unit)
  }
  
  if(is.null(index)){
    warning("No index found, treating as one group.")
    index <- rep(1, nrow(X))
    indexZones <- sf::st_sf(index=1, 
                            geometry=sf::st_as_sfc(sf::st_bbox(X)), 
                            crs=sf::st_crs(X))
    X[['index']] <- 1
    zonalArea <- data.table::data.table(index=indexZones$index, 
                                        zoneArea=fs_area(indexZones, unit="ha"))
  } else{
    if(!inherits(index, "sf")){
      if(inherits(index, "Spatial")){
        index <- sf::st_as_sf(index)
      } else{
        message("Invalid index. Spatial units requried.")
        stop()
      }
    }
    index$index <- 1:nrow(index)
    
    X <- zonalIndex(X, index, returnObject=TRUE)
    
    zonalArea <- data.table::data.table(index=index$index, 
                                        zoneArea=fs_area(index, unit="m^2"))
  }
  
  meanDT <- fs_NNdist_mean(X, index=X$zoneID, unit=unit, col="fs_NNdist")
  countDT <- fs_count(X, index=X$zoneID,)
  
  mCol <- paste0("fs_NNdist_", unit, "_mean")
  cCol <- "fs_count"
  
  DT <- merge(meanDT, countDT, by="index")
  DT <- merge(DT, zonalArea, by="index")
  
  DT[, fs_NNindex := get(mCol) / (0.5 * sqrt(zoneArea / get(cCol))), by=index]
  units(nniDT$fs_NNindex) <- NULL
  
  return(DT[, list(index, fs_NNindex)])
}
