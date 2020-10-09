#' Building Nearest Neighbour Index (NNI)
#' 
#' @description Calculate and summarise selected metrics of building footprint
#'   polygons within zones.
#' @param X Spatial object with building footprints or their centroid locations.
#' @param index A spatial polygon object of \code{sf} or \code{sp} type. If
#'   omitted all observations in \code{X} are assumed to be within one zone and
#'   the area of the minimum bounding circle is used for the nearest neighbour
#'   index.
#' @param zoneField (Optional) Column name of unique identifiers in \code{zone}
#'   to use. If omitted, the 'zoneID' will be numbered \code{1:nrow(zone)}.
#' @param unit character or \code{units} object to define distance. Default is
#'   NULL
#' @return \code{data.table} of zonal indices and values
#'
#' @details The nearest neighbour index (NNI) is a measure of spatial
#'   clustering. It compares the observed mean neighbour distances with a
#'   hypothetical maximum of dispersed observations given the area of the zone.
#'   Note that NNI is sensitive to changes in the zone area.
#'   
#'   \deqn{ NNI_z = \frac{\bar{NND_z}}{(0.5 * \sqrt{\frac{A_z}{n_z}}})}, where z
#'   is the zone, A is the area, NND is the mean nearest neighbour distance, and
#'   n is the count. The value of NNI can range from 0 (fully disperse) to 2.15
#'   (clustered), with values of 1 indicating spatial randomness.
#'   
#'   The function uses \code{fs_nndist} to calculate the distance between
#'   centroids of the building footprints within the same spatial zone indicated
#'   by \code{index}.
#' 
#' @import data.table
#' 
#' @aliases fs_nnindex
#' @rdname fs_nnindex
#' 
#' @export 
fs_nnindex <- function(X, index=NULL, 
                       zoneField=NULL, unit=NULL) UseMethod("fs_nnindex")


#' @name fs_nnindex
#' @export
fs_nnindex.sp <- function(X, index=NULL, 
                          zoneField=NULL, unit=NULL){
  X <- sf::st_as_sf(X)
  
  result <- fs_nnindex(X, index, zoneField, unit)
  return(result)
}


#' @name fs_nnindex
#' @export
fs_nnindex.sf <- function(X, index=NULL, 
                          zoneField=NULL, unit=NULL){

  if(is.na(sf::st_crs(X))){
    warning("Footprints have no spatial projection. Units ignored.")
    unit <- NULL
  } else{
    if(is.null(unit)){
      unit <- "m"
    }
  }

  result <- fs_nnindex_calc(X, index, zoneField, unit)
  return(result)
}


fs_nnindex_calc <- function(X, index, zoneField=NULL, unit=NULL){
  if(is.null(zoneField)){
    zoneField <- "zoneID"
  }
  # need spatial zones
  if(is.null(index)){
    warning("No index found, treating as one group.")
    index <- rep(1, nrow(X))
    indexZones <- sf::st_sf(index=1, 
                            geometry=sf::st_geometry(lwgeom::st_minimum_bounding_circle(X)),
                            crs=sf::st_crs(X))
    X[[zoneField]] <- 1
    zonalArea <- data.table::data.table(index=indexZones$index, 
                                        zoneArea=fs_area(indexZones, 
                                                         unit=paste0(unit, "^2")))
  } else{
    if(!inherits(index, "sf")){
      if(inherits(index, "Spatial")){
        index <- sf::st_as_sf(index)
      } else{
        stop("Invalid index. Spatial units required")
      }
    }
    if(!zoneField %in% colnames(index)){
      index[[zoneField]] <- 1:nrow(index)
    }
    
    X <- zonalIndex(X, zone=index, zoneField=zoneField, 
                    method="centroid", returnObject=TRUE)
    
    zonalArea <- data.table::data.table(idx=index[[zoneField]], 
                                        zoneArea=fs_area(index, 
                                                         unit=paste0(unit, "^2")))
  }
  data.table::setnames(zonalArea, "idx", zoneField)
  data.table::setkeyv(zonalArea, zoneField)
  
  # get NN distance only within each zone
  # use centroid points rather than polygon edge distances
  xDT <- data.table::data.table(X)
  meanDT <- xDT[, list(dist=mean(fs_nndist(sf::st_as_sf(.SD), 
                                           maxSearch=NULL,
                                           method='centroid',
                                           unit=unit)) ), 
                by=zoneField]
  
  mCol <- paste0("fs_nndist_", unit, "_mean")
  data.table::setnames(meanDT, "dist", mCol) 
  
  data.table::setkeyv(meanDT, zoneField)

  countDT <- fs_count(X, index=zoneField)
  data.table::setkeyv(countDT, zoneField)
  cCol <- "fs_count"
  
  DT <- meanDT[countDT]
  n <- setdiff(names(zonalArea), key(zonalArea))
  DT[zonalArea, (n) := mget(paste0("i.", n))]
  
  DT[, fs_nnindex := get(mCol) / (0.5 * sqrt(zoneArea / get(cCol))), by=zoneField]
  units(DT$fs_nnindex) <- NULL
  
  keep <- c(zoneField, "fs_nnindex")
  return(DT[, ..keep])
  # return(DT[, list(index, fs_nnindex)])
}
