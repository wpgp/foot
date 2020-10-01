#' @title Area
#' @description Helper geometry function to measure the area of building
#'   footprint polygons.
#' @param X polygons of building footprints of type \code{sf}.
#' @param unit string indicating unit of measure. Passed to
#'   \code{units::set_units}.
#' @return numeric vector of area measured for each item in \code{X}.
#' 
#' @import units
#'
#' @name fs_area
#' @export
fs_area <- function(X, unit=NULL){
  area_calc <- sf::st_area(X)
  
  if(!is.null(unit)){
    area_calc <- units::set_units(area_calc, unit, mode="standard")
  }
  
  return(area_calc)
}


#' @title Perimeter
#' @description Helper geometry function to measure the perimeter of building
#'   footprint polygons.
#' @param X polygons of building footprints of type \code{sf}.
#' @param unit string indicating unit of measure. Passed to
#'   \code{units::set_units}.
#' @return numeric vector of perimeter measured for each item in \code{X}.
#'
#' @name fs_perimeter
#' @export
fs_perimeter <- function(X, unit=NULL){
  if(st_is_longlat(X)){
    # perim_calc <- sf::st_length(sf::st_cast(sf::st_geometry(X), "LINESTRING"))
    perim_calc <- lwgeom::st_geod_length(X)
  } else{
    perim_calc <- lwgeom::st_perimeter(X)
  }

  if(!is.null(unit)){
    perim_calc <- units::set_units(perim_calc, unit, mode="standard")
  }
  
  return(perim_calc)
}


#' @title Compactness index
#' 
#' @description Calculates an approximate measure of shape "compactness".
#' @param X polygons of building footprints of type \code{sf}.
#' @return numeric vector of compactness values ranging from 0 to 1 for each
#'   item in \code{X}.
#' 
#' @details The compactness measure is the Polsby-Popper test based on the
#'   perimeter and area of each footprint.
#'   
#'   \deqn{ PP_i = \frac{4$\pi$ * A_z}{P_i^2} },
#'   where P and A are the perimeter and area of footprint 'i', respectively.
#'   Values closer to 1 indicate more compact shapes.
#'   
#' @source Polsby, Daniel D., and Robert D. Popper. 1991. “The Third Criterion:
#'   Compactness as a procedural safeguard against partisan gerrymandering.”
#'   Yale Law & Policy Review 9 (2): 301–353.
#' 
#' @name fs_shape
#' @export
fs_compact <- function(X){
  if(!inherits(X, 'sf')){
    X <- sf::st_as_sf(X)
  }
  
  if(!"fs_area" %in% names(X)){
    if(any(!sf::st_geometry_type(X) %in% c("POLYGON", "MULTIPOLYGON"))){
      stop("Area requires polygons")
    } else{
      X[["fs_area"]] <- fs_area(X, unit="m^2")
    }
  }
  
  if(!"fs_perimeter" %in% names(X)){
    if(any(!sf::st_geometry_type(X) %in% c("POLYGON", "MULTIPOLYGON"))){
      stop("Perimeter requires polygons.")
    } else{
      X[["fs_perimeter"]] <- fs_perimeter(X, unit="m")
    }
  }
  
  compactness <- (4 * pi * X[["fs_area"]]) / (X[["fs_perimeter"]]^2)
  units(compactness) <- NULL
  
  return(compactness)
} 


#' @title Shape index
#' 
#' @description Calculates a measure of shape or "roundness".
#' @param X polygons of building footprints of type \code{sf}.
#' @return numeric vector with shape index values ranging from 0 to 1 or each
#'   item in \code{X}.
#' 
#' @details The shape index is calculated as the ratio of footprint polygon's
#'   area to the area of minimum bounding circle. The mbc is calculated using
#'   \code{fs_mbc} and \code{lwgeom}. Values closer 1 suggest more rounded
#'   shapes.
#' 
#' The function first looks for pre-calculated values of area in a field called
#' \code{fs_area}. If not present, the areas are calculated in m^2.
#' 
#' @name fs_shape
#' @export
fs_shape <- function(X){
  if(!inherits(X, "sf")){
    X <- sf::st_as_sf(X)
  }
  
  if(any(!sf::st_geometry_type(X) %in% c("POLYGON", "MULTIPOLYGON"))){
    stop("Shape index requires polygons")
  } 
  
  if(!"fs_area" %in% names(X)){
    X[["fs_area"]] <- fs_area(X, unit="m^2")
  } else{
    units(X$fs_area) <- units::as_units("m^2")
  }
  
  DT <- data.table::data.table(sf::st_drop_geometry(X),
                               geom=sf::st_geometry(X))
  
  DT[, mbc := fs_mbc(geom)]
  DT[, mbcA := fs_area(mbc, unit="m^2")]
  # calculate index
  DT[, shapeIdx := fs_area / mbcA]
  units(DT$shapeIdx) <- NULL
  
  return(DT[["shapeIdx"]])
}


#' @title Rotated minimum bounding rectangle
#'
#' @description Helper function to provide the minimum rotated bounding
#'   rectangle of a polygon or set of points. Optionally, the function will
#'   return the bearing angle in degrees from vertical in a clockwise direction.
#'
#' @param X polygons of building footprints in type \code{sf}.
#' @param returnShape logical. Should the function return the \code{sf} polygon
#'   of the rotated bounding rectangle or should it return the angle (in degrees).
#' @return a numeric angle from 0 to 360 degrees or the rotated rectangle as a
#'   polygon of type \code{sf}.
#' @details This function is currently not vectorized and processing is limited
#'   to one shape.
#'
#' @source
#' \link[https://gis.stackexchange.com/questions/22895/finding-minimum-area-rectangle-for-given-points]{https://gis.stackexchange.com/questions/22895/finding-minimum-area-rectangle-for-given-points}
#'
#' @name fs_mbr
#' @export
# Based on: https://gis.stackexchange.com/questions/22895/finding-minimum-area-rectangle-for-given-points
fs_mbr <- function(X, returnShape=FALSE){
  if(any(!sf::st_geometry_type(X) %in% c("POLYGON", "MULTIPOLYGON"))){
    stop("Bounding rectangle requires polygons.")
  }
  
  mbr <- function(g, returnShape){
    p <- sf::st_coordinates(g)[, 1:2]
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
  
  resList <- lapply(sf::st_geometry(X), FUN=mbr, returnShape)
  if(returnShape){
    return(sf::st_as_sfc(resList))
  } else{
    return(unlist(resList))
  }
  
  # if(sf::st_geometry_type(X) %in% c("POLYGON", "MULTIPOLYGON", "POINT", "MULTIPOINT")){
  #   p <- sf::st_coordinates(X)[,1:2]
  # } else if(class(X) != "Matrix"){
  #   stop("Invalid coordinates.")
  # }  
  # 
  # # Analyze the convex hull edges     
  # a <- chull(p)                                   # Indexes of extremal points
  # a <- c(a, a[1])                                 # Close the loop
  # e <- p[a[-1],] - p[a[-length(a)], ]             # Edge directions
  # norms <- sqrt(rowSums(e^2))                     # Edge lengths
  # v <- e / norms                                  # Unit edge directions
  # w <- cbind(-v[,2], v[,1])                       # Normal directions to the edges
  # 
  # # Find the MBR
  # vertices <- p[a, ]                              # Convex hull vertices
  # x <- apply(vertices %*% t(v), 2, range)         # Extremes along edges
  # y <- apply(vertices %*% t(w), 2, range)         # Extremes normal to edges
  # areas <- (y[1,]-y[2,])*(x[1,]-x[2,])            # Areas
  # k <- which.min(areas)                           # Index of the best edge (smallest area)
  # 
  # # Form a rectangle from the extremes of the best edge
  # R <- rbind(v[k,], w[k,])
  # # print((atan2(R[2,1], R[1,1]) * 180/pi) %% 360)
  # mbr <- cbind(x[c(1,2,2,1,1),k], y[c(1,1,2,2,1),k]) %*% R
  # 
  # if(returnShape){
  #   return(sf::st_polygon(list(mbr)))
  #   
  # } else{
  #   angle <- (atan2(R[2,1], R[1,1]) * 180/pi) %% 360
  #   return(angle)
  # }
}


#' @title Minimum bounding circle
#' 
#' @description Helper function to calculate the minimum circle that bounds the
#'   polygon features.
#' @param X Building footprint polygons in \code{sf} or \code{sp} or other valid
#'   spatial types.
#' @return Object of the same class as \code{X} 
#' @details \code{fs_mbc} uses \code{lwgeom::st_minimum_bounding_circle} to
#'   calculate the minimum bounding circle.
#' 
#' @import lwgeom
#' 
#' @name fs_mbc
#' @export
fs_mbc <- function(X){
  if(any(!sf::st_geometry_type(X) %in% c("POLYGON", "MULTIPOLYGON"))){
    stop("Bounding circle requires polygons.")
  } else{
    mbc <- lwgeom::st_minimum_bounding_circle(X)
  }
  
  return(mbc)
}
