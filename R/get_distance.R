
suggestUTMzone <- function(pt){
  lon <- pt[,"X"]
  lat <- pt[,"Y"]
  
  zone <- (floor((lon + 180)/6) %% 60) + 1

  if (lat >= 0) {
    zone <- paste0('326', zone)
  } else {
    zone <- paste0('327', zone)
  }
  return(as.numeric(zone))
}

#' @title Nearest neighbour distance calculation
#'
#' @description Helper function to provide a distance calculation between
#'   spatial objects. The distance to the first nearest neighbour found
#'   (optionally within a maximum search radius) is returned.
#'
#' @param X Spatial object of \code{sf} type, typically polygons or points.
#' @param Y (Optional) Spatial object to measure distances to.
#' @param maxSearch Maximum radius around \code{X} to search. Distance in
#'   meters. Default is 100. To ignore the search limit, set to `NULL`.
#' @param method Either \code{'poly'} or \code{'centroid'} to assign a geometry
#'   method. See details. Default is 'poly'.
#' @param unit Character abbreviation for the units to return from the distance
#'   calculation.

#' @details If \code{Y} is omitted the nearest neighbour distances are found
#'   within \code{X}. Otherwise, the distance for each object in \code{X} to its
#'   nearest neighbour in \code{Y} is returned. 
#'   
#'   Use \code{method} to adjust which geometry of \code{X} and \code{Y} is used
#'   for distance calculations. When \code{method='poly'} distances are measured
#'   between polygon edges. When \code{method='centroid'}, the centroids of
#'   building footprints are used instead. Centroid-based distance calculations
#'   are faster.
#'   
#'   Providing a maximum search radius is strongly advised to speed up the
#'   calculation.
#'   
#' @examples 
#' data("kampala", package="foot")
#' 
#' # get sample of buildings
#' buildings <- kampala$buildings
#' buildings <- buildings[sample(1:nrow(buildings), size=100, replace=F),]
#' clusters <- kampala$clusters
#' 
#' # calculate distance between buildings in m
#' fs_nndist(buildings, unit="m")
#' 
#' # calculate unrestricted distance 
#' # between buildings and another set of points
#' fs_nndist(buildings, sf::st_centroid(clusters), maxSearch=NULL)
#' 
#' # use footprint centroids
#' fs_nndist(buildings, method='centroid', unit='m')
#' 
#' @name fs_nndist
#' @export
fs_nndist <- function(X, Y, maxSearch=100, method='poly', unit="m"){
  if(missing(X)) stop("Must provide building footrpints")
  # row index
  uid <- 1:nrow(X)
  
  if(method[1]=='centroid'){
    suppressWarnings(X <- sf::st_centroid(X))
  }
  
  if(missing(Y)){
    searchObj <- X
  } else{
    searchObj <- Y
    
    if(method[1]=='centroid'){
      suppressWarnins(searchObj <- sf::st_centroid(searchObj))
    }
  }
  
  if(sf::st_crs(X) != sf::st_crs(searchObj)){
    stop("Coordinate reference systems do not match in distance calculation.")
  }
  
  # initial search of intersecting objects (distance = 0)
  intersects1 <- suppressMessages(sf::st_intersects(X, searchObj))
  DT <- data.table::data.table(uid=uid, 
                               intersects=lapply(intersects1, function(i) uid[i]))
  
  DT[which(lengths(intersects)>1), dist := 0]
  data.table::setindex(DT, uid)
  
  # buffer search
  if(!is.null(maxSearch)){
    if(sf::st_is_longlat(searchObj)){
      zn <- suggestUTMzone(
                          suppressWarnings(sf::st_coordinates(
                            sf::st_centroid(sf::st_as_sfc(sf::st_bbox(X)))))
                          )
      
      searchBuffer <- sf::st_transform(searchObj, zn)
      searchBuffer <- sf::st_buffer(searchBuffer, maxSearch)
      searchBuffer <- sf::st_transform(searchBuffer, sf::st_crs(X))
    } else{
      searchBuffer <- sf::st_buffer(searchObj, maxSearch)
    }
    # get objects within search buffer
    intersects2 <- suppressMessages(sf::st_intersects(X, searchBuffer))
    # restricted distance calculation
    DT[, intersects := lapply(intersects2, function(i) i)]
    
    DT[is.na(dist), 
       dist := sort(sf::st_distance(sf::st_geometry(X)[uid], 
                                    sf::st_geometry(searchObj)[unlist(intersects)]))[2], #, tolerance=1
       by = uid]

  } else{  # unrestricted distance search (slower)
    DT[is.na(dist), 
       dist := sort(sf::st_distance(sf::st_geometry(X)[uid], sf::st_geometry(searchObj)))[2],
       by = uid]
  }

  if(!is.null(unit)){
    DT[, dist := units::set_units(dist, unit, mode="standard")]
  }
  
  return(DT[["dist"]])  # return as vector
}


