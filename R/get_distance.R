
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
#'   spatial ojects. The distance to the first nearest neighbour found
#'   (optionally within a maximum search radisu) is returned.
#'
#' @param X Spatial object of \code{sf} type, typically polygons or points.
#' @param Y (Optional) Spatial object to measure distances to.
#' @param maxSearch Maximum search radius around \code{X} to search. Distance in
#'   meters. Default is 1000.
#' @param unit Character abbreviation for the units to return from the distance
#'   calculation.
#'
#' @details If \code{Y} is omitted the nearest neighbour distances are found
#'   within \code{X}. Otherwise, the distance for each object in \code{X} to its
#'   nearest neighbour in \code{Y} is returned. Providing a maximum search
#'   radius is strongly advised to speed up the calcluation.
#' 
#' @name fs_NNdist
#' @export
fs_NNdist <- function(X, Y, maxSearch=100, unit="m"){
  # row index
  uid <- 1:nrow(X)
  
  if(missing(Y)){
    searchObj <- X
  } else{
    searchObj <- Y
  }
  
  if(sf::st_crs(X) != sf::st_crs(searchObj)){
    stop("Coordinate reference systems do not match for zone indexing.")
  }
  
  # initial search of intersecting objects (distance = 0)
  intersects1 <- sf::st_intersects(X, searchObj)
  DT <- data.table::data.table(uid=uid, 
                               intersects=lapply(intersects1, function(i) uid[i]))
  
  DT[which(lengths(intersects)>1), dist := 0]
  data.table::setindex(DT, uid)
  
  # buffer search
  if(!is.null(maxSearch)){
    if(sf::st_is_longlat(searchObj)){
      zn <- suggestUTMzone(sf::st_coordinates(sf::st_centroid(sf::st_as_sfc(sf::st_bbox(X)))))
      
      searchBuffer <- sf::st_transform(searchObj, zn)
      searchBuffer <- sf::st_buffer(searchBuffer, maxSearch)
      searchBuffer <- sf::st_transform(searchBuffer, sf::st_crs(X))
    } else{
      searchBuffer <- sf::st_buffer(searchObj, maxSearch)
    }
    # get objects within search buffer
    intersects2 <- sf::st_intersects(X, searchBuffer)
    # restricted distance calculation
    DT[, intersects := lapply(intersects2, function(i) i)]
    
    DT[is.na(dist), 
       # dist := sort(sf::st_distance(X$geometry[uid], searchObj$geometry[unlist(intersects)]))[2], #, tolerance=1
       dist := sort(sf::st_distance(sf::st_geometry(X)[uid], sf::st_geometry(searchObj)[unlist(intersects)]))[2], #, tolerance=1
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


