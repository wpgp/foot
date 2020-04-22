
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


#' @name fs_NNdist
#' @export
fs_NNdist <- function(X, Y, maxSearch=1000, unit="m"){
  
  if(!is.null(maxSearch) & maxSearch > 0){
    if(missing(Y)){
      searchBuffer <- X
    } else{
      searchBuffer <- Y
    }
    
    if(sf::st_crs(X) != sf::st_crs(searchBuffer)){
      stop("Coordinate reference systems do not match for zone indexing.")
    }
    
    if(sf::st_is_longlat(searchBuffer)){
      zn <- suggestUTMzone(sf::st_coordinates(sf::st_centroid(sf::st_as_sfc(sf::st_bbox(X)))))
      
      searchBuffer <- sf::st_transform(searchBuffer, zn)
      searchBuffer <- sf::st_buffer(searchBuffer, maxSearch)
      searchBuffer <- sf::st_transform(searchBuffer, sf::st_crs(X))
    } else{
      searchBuffer <- sf::st_buffer(searchBuffer, maxSearch)
    }
    # get objects within search buffer
    intersects <- sf::st_intersects(searchBuffer, X)
    # restricted distance calculation
    distList <- lapply(seq_along(1:nrow(X)),
                                     FUN=function(i){ sort(sf::st_distance(X[i,], 
                                                                           X[intersects[[i]],]))[2] })
    
  } else{
    # unrestricted distance search (slower)
    distList <- lapply(seq_along(1:nrow(X)), 
                          FUN=function(i){ sort(sf::st_distance(X[i,], X[-i,]))[1] })
  }
  
  result <- do.call(c, distList)
  
  if(!is.null(unit)){
    units::set_units(result, unit, mode="standard")
  }
  
  return(result)
}


