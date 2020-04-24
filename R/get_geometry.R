
fs_area <- function(X, unit=NULL){
  area_calc <- sf::st_area(X)
  
  if(!is.null(unit)){
    area_calc <- units::set_units(area_calc, unit, mode="standard")
  }
  
  return(area_calc)
}


fs_perimeter <- function(X, unit=NULL){
  perim_calc <- lwgeom::st_perimeter(X)
  
  if(!is.null(unit)){
    perim_calc <- units::set_units(perim_calc, unit, mode="standard")
  }
  
  return(perim_calc)
}


# Based on: https://gis.stackexchange.com/questions/22895/finding-minimum-area-rectangle-for-given-points
fs_mbr <- function(X, returnShape=FALSE){
  if(any(class(X) == "POLYGON") | any(class(X) == "MULTIPOLYGON")){
    p <- sf::st_coordinates(X)[,1:2]
  } else if(class(X) != "Matrix"){
    stop("Invalid coordinates.")
  }

  # Analyze the convex hull edges     
  a <- chull(p)                                   # Indexes of extremal points
  a <- c(a, a[1])                                 # Close the loop
  e <- p[a[-1],] - p[a[-length(a)], ]             # Edge directions
  norms <- sqrt(rowSums(e^2))                     # Edge lengths
  v <- e / norms                                  # Unit edge directions
  w <- cbind(-v[,2], v[,1])                       # Normal directions to the edges

  # Find the MBR
  vertices <- p[a, ]                              # Convex hull vertices
  x <- apply(vertices %*% t(v), 2, range)         # Extremes along edges
  y <- apply(vertices %*% t(w), 2, range)         # Extremes normal to edges
  areas <- (y[1,]-y[2,])*(x[1,]-x[2,])            # Areas
  k <- which.min(areas)                           # Index of the best edge (smallest area)

  # Form a rectangle from the extremes of the best edge
  R <- rbind(v[k,], w[k,])
  # print((atan2(R[2,1], R[1,1]) * 180/pi) %% 360)
  mbr <- cbind(x[c(1,2,2,1,1),k], y[c(1,1,2,2,1),k]) %*% R

  if(returnShape){
    return(sf::st_polygon(list(mbr)))
    
  } else{
    angle <- (atan2(R[2,1], R[1,1]) * 180/pi) %% 360
    return(angle)
  }
}
