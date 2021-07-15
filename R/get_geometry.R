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


#' @title Length-width ratio
#' @description Helper geometry function to measure calculate the ratio between
#'   the length and width.
#' @param X polygons of building footprints of type \code{sf}.
#' @return numeric vector of length / width measured for each item in \code{X}.
#' @details The length is defined as the longest side of the rotated minimum
#'   bounding rectangle and the width is the shorter side.
#'
#' @name fs_lwratio
#' @export
fs_lwratio <- function(X){
  if(!inherits(X, "sf")){
    X <- sf::st_as_sf(X)
  }
  
  if(any(!sf::st_geometry_type(X) %in% c("POLYGON", "MULTIPOLYGON"))){
    stop("Shape index requires polygons")
  } 
  
  mbr <- fs_mbr(X, returnShape = TRUE)
  calc <- calc_lw(mbr)

  return(calc[,1] / calc[,2])
}


#' @title Length to equivalent-width ratio
#' @description Helper geometry function to measure calculate the ratio between
#'   the length and the equivalent width.
#' @param X polygons of building footprints of type \code{sf}.
#' @param unit string indicating unit of measure. Passed to
#'   \code{units::set_units}.
#' @return numeric vector of length / width measured for each item in \code{X}.
#' @details The length is defined as the longest side of the rotated minimum
#'   bounding rectangle. The equivalent width is defined as the area of the
#'   footprint divided by its length.
#'
#' @rdname fs_lwratio
#' @export
fs_leqwratio <- function(X, unit=NULL){
  if(!inherits(X, "sf")){
    X <- sf::st_as_sf(X)
  }
  
  if(any(!sf::st_geometry_type(X) %in% c("POLYGON", "MULTIPOLYGON"))){
    stop("Shape index requires polygons")
  } 
  
  if(is.null(unit)){
    unit <- "m^2"
  }
  
  if(!"fs_area" %in% names(X)){
    X[["fs_area"]] <- fs_area(X, unit=unit)
  } else{
    units(X$fs_area) <- units::as_units(unit)
  }

  mbr <- fs_mbr(X, returnShape = TRUE)
  calc <- calc_lw(mbr)
  ww <- units::drop_units(X$fs_area / calc[,1])
  
  return(calc[,1] / ww)
}


#' @title Length and width calculator
#' @description Internal helper geometry function.
#' @param X minimum bounding rectangles of building footprints of type \code{sf}.
#' @return numeric vectors of length and width measured for each item in \code{X}.
#'
#' @keywords internal
calc_lw <- function(X){
  # find edges
  gpts <- sf::st_cast(X, 'POINT')
  lagpts <- c(gpts[-1], gpts[1])
  # get edge lengths
  alld <- sf::st_distance(gpts, lagpts, by_element = T)
  d <- alld[-seq(5, length(alld), by = 5)]
  ids <- rep(1:length(X), each = 4)
  
  # get length of mbr
  mx <- aggregate(list('maxID' = d), by = list('id' = ids), which.max)
  mx$did <- (mx$id - 1) * 4 + mx$maxID
  ll <- d[mx$did]
  
  # get width of mbr
  mn <- aggregate(list('minID' = d), by = list('id' = ids), which.min)
  mn$did <- (mn$id - 1) * 4 + mn$minID
  ww <- d[mn$did]
  
  return(cbind(ll, ww))
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

  if(any(!sf::st_geometry_type(X) %in% c("POLYGON", "MULTIPOLYGON"))){
      stop("Area requires polygons")
  } else{
      X[["fs_area"]] <- fs_area(X, unit="m^2")
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
#' @param unit string indicating unit of measure. Passed to
#'   \code{units::set_units}.
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
#' @examples 
#' data("kampala", package="foot")
#' buildings = kampala$buildings
#' 
#' fs_shape(buildings)
#' 
#' # how the calculation is done - first observation
#' fs_area(buildings[1,]) / fs_area(fs_mbc(buildings[1,]))
#' 
#' @name fs_shape
#' @export
fs_shape <- function(X, unit=NULL){
  if(!inherits(X, "sf")){
    X <- sf::st_as_sf(X)
  }
  
  if(any(!sf::st_geometry_type(X) %in% c("POLYGON", "MULTIPOLYGON"))){
    stop("Shape index requires polygons")
  } 
  
  if(is.null(unit)){
    unit <- "m^2"
  }
  
  if(!"fs_area" %in% names(X)){
    X[["fs_area"]] <- fs_area(X, unit=unit)
  } else{
    units(X$fs_area) <- units::as_units(unit)
  }
  
  DT <- data.table::data.table(sf::st_drop_geometry(X),
                               geom=sf::st_geometry(X))
  
  DT[, mbc := fs_mbc(geom)]
  DT[, mbcA := fs_area(mbc, unit=unit)]
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
#'
#' @name fs_mbr
#' @export   
fs_mbr <- function(X, returnShape=FALSE){
  if(any(!sf::st_geometry_type(X) %in% c("POLYGON", "MULTIPOLYGON"))){
    stop("Bounding rectangle requires polygons.")
  }
  
  mbr <- sf::st_as_sfc(geos::geos_minimum_rotated_rectangle(X))
  
  if(returnShape){
    return(mbr)
  } else{
    # find edges
    gpts <- sf::st_cast(mbr, 'POINT')
    lagpts <- c(gpts[-1], gpts[1])
    # get edge lengths
    alld <- sf::st_distance(gpts, lagpts, by_element = T)
    d <- alld[-seq(5, length(alld), by = 5)]
    ids <- rep(1:length(mbr), each = 4)
    
    mx <- aggregate(list('maxID' = d), by = list('id' = ids), which.max)
    mx$did <- (mx$id - 1) * 4 + mx$maxID

    if(st_is_longlat(b)){
      bears <- geosphere::bearing(sf::st_coordinates(gpts[mx$did]), 
                                  sf::st_coordinates(lagpts[mx$did]))
    } else{
      dif <- sf::st_coordinates(gpts[mx$did]) - sf::st_coordinates(lagpts[mx$did])
      bears <- atan2(dif[,1], dif[,2]) * 180/pi
    }
    return((bears + 360) %% 360)
  }
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
