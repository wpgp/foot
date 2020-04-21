
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


fs_NNdist <- function(X, maxSearch=1000, unit="m"){
  
  if(!is.null(maxSearch) & maxSearch > 0){
    if(sf::st_is_longlat(X)){
      zn <- suggestUTMzone(sf::st_coordinates(sf::st_centroid(sf::st_as_sfc(sf::st_bbox(X)))))
      
      searchBuffer <- sf::st_transform(X, zn)
      searchBuffer <- sf::st_buffer(searchBuffer, maxSearch)
      searchBuffer <- sf::st_transform(searchBuffer, sf::st_crs(X))
    } else{
      searchBuffer <- sf::st_buffer(X, maxSearch)
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
  
  result <- do.call(rbind, distList)
  
  if(!is.null(unit)){
    units::set_units(result, unit, mode="standard")
  }
  
  return(result)
}


