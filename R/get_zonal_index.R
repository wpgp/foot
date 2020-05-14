#' Index buildings footprints to 'zones'
#' 
#' @description Find the area, grid cells, or other zone that a building polygon
#'   overlaps or is located in
#' @param X Spatial data (or path to file) with building footprint polygons
#' @param zone Spatial data (or path to file) with polygon zones or a spatial
#'   grid ("raster")
#' @param zoneField (Optional) Unique identifier for each zone
#' @param returnObject Logical of whether to return an sf object of X with zonal
#'   information. Default TRUE.
#' @param clip Logical of whether polygons of X which span multiple zones should
#'   be clipped to the zone. If not clipped, then whole building footprints are
#'   linked to each zone. Default FALSE.
#' @return 'sf' object with attributes of X plus the unique zone ID or a
#'   data.table with the ID to the records in \code{X} and the zone IDs.
#' @author Chris Jochem
#' 
#' @import data.table
#' @aliases zonalIndex
#' @rdname zonalIndex
#' 


#' @name zonalIndex
#' @export
zonalIndex <- function(X, zone, zoneField=NULL, returnObject=TRUE, clip=FALSE) UseMethod("zonalIndex", zone)


#' @name zonalIndex
#' @export
zonalIndex.sf <- function(X, zone, zoneField=NULL, returnObject=TRUE, clip=FALSE){
  if(any(class(X) == "sp") | any(class(X) == "stars")){
    # convert to sf
    X <- sf::st_as_sf(X)
  } else if (all(class(X) != "sf")) {
    stop("Object format not valid.")
  }
  
  if(clip==TRUE & returnObject==FALSE){
    clip <- FALSE
    warning("Clipping only valid when returning a spatial object.")
  }
  
  result <- get_zonal_index(X, zone, zoneField, returnObject, clip)
  return(result)
}


#' @name zonalIndex
#' @export
zonalIndex.sfc <- function(X, zone, zoneField=NULL, returnObject=TRUE, clip=FALSE){
  if(any(class(X) == "sp") | any(class(X) == "stars")){
    # convert to sf
    X <- sf::st_as_sf(X)
  } else if (all(class(X) != "sf")) {
    stop("Object format not valid.")
  }
  
  if(clip==TRUE & returnObject==FALSE){
    clip <- FALSE
    warning("Clipping only valid when returning a spatial object.")
  }
  
  result <- get_zonal_index(X, zone, zoneField, returnObject, clip)
  return(result)
}


#' @name zonalIndex
#' @export
zonalIndex.sp <- function(X, zone, zoneField=NULL, returnObject=TRUE, clip=FALSE){
  zone <- sf::st_as_sf(zone)
  result <- zonalIndex(X, zone, zoneField, returnObject, clip)
  return(result)
}


#' @name zonalIndex
#' @export
zonalIndex.stars <- function(X, zone, zoneField=NULL, returnObject=TRUE, clip=FALSE){
  zone <- sf::st_as_sf(zone)
  result <- zonalIndex(X, zone, zoneField, returnObject, clip)
  return(result)
}


#' @name zonalIndex
#' @export
zonalIndex.raster <- function(X, zone, zoneField=NULL, returnObject=TRUE, clip=FALSE){
  zone <- stars::st_as_stars(zone)
  result <- zonalIndex(X, zone, zoneField, returnObject, clip)
  return(result)
}


#' @name zonalIndex
#' @export
zonalIndex.character <- function(X, zone, zoneField=NULL, returnObject=TRUE, clip=FALSE){
  zone <- sf::st_read(zone)
  result <- zonalIndex(X, zone, zoneField, returnObject, clip)
  return(result)
}


get_zonal_index <- function(X, zone, zoneField=NULL, returnObject=TRUE, clip=FALSE){
  if(missing(X)){
    stop("Missing footprint dataset")
  } else if (all(!class(X) %in% c("sf","sfc"))){
    X <- sf::st_as_sf(X)
  }
  
  if(is.na(sf::st_crs(X)) | is.na(sf::st_crs(zone))){
    stop("Missing CRS.")
  }
  
  if(sf::st_crs(X) != sf::st_crs(zone)){
    stop("Coordinate systems do not match")
  }
  
  if(any(sf::st_geometry_type(X) == "MULTIPOLYGON")){
    X <- sf::st_cast(X, "POLYGON")
  }
  
  if(any(sf::st_geometry_type(zone) == "MULTIPOLYGON")){
    zone <- sf::st_cast(sf::st_cast(sf::st_make_valid(zone), "MULTIPOLYGON"), "POLYGON")
  }
  
  if(is.null(zoneField)){
    zone[["zoneID"]] <- 1:length(sf::st_geometry(zone))
    zoneField <- "zoneID"
  } else if(!zoneField %in% names(zone)){
    stop("Field identifying zones not found")
  }
  
  i <- sf::st_intersects(zone, X, sparse=TRUE)
  hits <- which(lengths(i)>0)
  # subset the objects to limit search space
  i <- i[hits]
  zone <- zone[hits,]
  # get the name of the geometry column
  zoneGeo <- attr(zone, "sf_column") 
  
  if(returnObject){
    if(clip){
      intList <- suppressMessages(suppressWarnings( lapply(seq(hits),  # TO-DO move to parallel
                        FUN=function(j){ 
                          ints <- sf::st_intersection(zone[j, c(zoneField, zoneGeo)], 
                                                      X[i[[j]],] )
                          if(any(sf::st_geometry_type(ints)=="MULTIPOLYGON")){
                            ints <- sf::st_cast(
                              sf::st_cast(
                                sf::st_make_valid(ints), "MULTIPOLYGON"), "POLYGON")
                          }
                          return(ints)
                        }) ))
      DT <- data.table::rbindlist(intList)
      data.table::setkeyv(DT, zoneField)
      data.table::setcolorder(DT, neworder=c(names(X), zoneField))
      result <- sf::st_as_sf(DT)
      
    } else{
      DT <- data.table::data.table(X[unlist(i),],
                                   zoneID=rep(zone[[zoneField]], lengths(i)))
      data.table::setkey(DT, zoneID)
      result <- sf::st_as_sf(DT)
    }
  } else{
    result <- data.table::data.table(xID=unlist(i), 
                                     zoneID=rep(zone[[zoneField]], lengths(i)))
    data.table::setkey(result, zoneID)
  }
  
  return(result)
}
