
fs_area <- function(X, unit=NULL){
  area_calc <- sf::st_area(X)
  
  if(!is.null(unit)){
    area_calc <- units::set_units(area_calc, unit, mode="standard")
  }
  
  return(area_calc)
}


fs_perimeter <- function(X, unit=NULL){
  if(st_is_longlat(X)){
    perim_calc <- sf::st_length(sf::st_cast(sf::st_geometry(X), "LINESTRING"))
  } else{
    perim_calc <- sf::st_length(X)
  }
  # perim_calc <- lwgeom::st_perimeter(X)
  
  if(!is.null(unit)){
    perim_calc <- units::set_units(perim_calc, unit, mode="standard")
  }
  
  return(perim_calc)
}


#' @title Rotated minimum bounding rectangle
#'
#' @description Helper function to provide the minimum rotated bounding rectange
#'   of a polygon or set of points. Optionally, the function will return the
#'   bearing angle in degrees from vertical in a clockwise direction.
#'
#' @param X matrix of point coordinates in two columns or a polygon of \code{sf}
#'   type.
#' @param returnShape logical. Should the function return the \code{sf} polygon
#'   of the rotated bounding rectange or should it retun the angle (in degrees).
#'
#' @details This function is currently not vectorized and processing is limited
#'   to one shape.
#'
#' @source
#'   \link[https://gis.stackexchange.com/questions/22895/finding-minimum-area-rectangle-for-given-points]{https://gis.stackexchange.com/questions/22895/finding-minimum-area-rectangle-for-given-points}
#'
#'   
#' @name fs_mbr
#' @export
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
