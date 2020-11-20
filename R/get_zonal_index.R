#' Index buildings footprints to 'zones'
#' 
#' @description Find the area, grid cells, or other zone that a building polygon
#'   overlaps or is located in. Implements an efficient spatial join by
#'   intersection.
#' @param X Spatial data (or path to file) with building footprint polygons
#' @param zone Spatial data (or path to file) with polygon zones or a spatial
#'   grid (i.e. "raster")
#' @param zoneField (Optional) Column name of unique identifiers in \code{zone}
#'   to use. If omitted, the 'zoneID' will be numbered \code{1:nrow(zone)}.
#' @param method One of \code{'centroid', 'intersect', 'clip'} to determine how
#'   footprints are allocated to zones. See details. Default is
#'   \code{"centroid"}.
#' @param returnObject Logical of whether to return an sf object of X with zonal
#'   information. Default is \code{TRUE} which is generally preferred.
#' @return 'sf' object with attributes of \code{X} plus the unique zone ID or a
#'   \code{data.table} with the row number to the record in \code{X} matched to
#'   the zone IDs.
#'   
#' @details Zone assignments for building footprints can be done using three
#'   possible methods, set by the \code{method=} parameter. Defining by
#'   'centroid' allocates and entire building and its characteristics to the
#'   zone(s) which its centroid intersects. Centroids are defined by
#'   \code{\link[sf]{st_centroid}} which could be outside the polygon shape.
#'   This is the default mode. Defining zones by 'intersect' uses the geometric
#'   binary predicate from \code{\link[sf]{st_intersects}}. This method will
#'   include a whole building and its characteristics into all zones that it
#'   intersects. Therefore a building could appear to be "counted" twice. The
#'   final approach, 'clip', uses \code{\link[sf]{st_intersection}} to split
#'   footprints so that only that area of the polygon intersecting the zone is
#'   include. This method is more time consuming because the geometries are
#'   modified.
#'   
#'   When \code{zone} is a multi-layer \code{stars} or \code{Raster*},
#'   \code{zoneField} can be used to select a specific layer to define the
#'   areas, or \code{NULL} to use each pixel as a unique zone.
#'   
#' @examples 
#' data("kampala", package="foot")
#' 
#' buildings <- kampala$buildings
#' clusters <- kampala$clusters
#' 
#' # assign zones and return a new 'sf' object
#' zonalIndex(buildings, clusters)
#' 
#' # assign all intersecting zones
#' zonalIndex(buildings, clusters, method="intersect")
#' 
#' # return only a table of indices - note column names
#' zonalIndex(buildings, clusters, zoneField="Id", returnObject=FALSE)
#' 
#' @import data.table
#' @importFrom purrr map2
#' @aliases zonalIndex
#' @rdname zonalIndex
#' @name zonalIndex
#' @export
zonalIndex <- function(X, 
                       zone, 
                       zoneField=NULL, 
                       method='centroid',
                       returnObject=TRUE) UseMethod("zonalIndex", zone)


#' @name zonalIndex
#' @export
zonalIndex.sf <- function(X, zone, 
                          zoneField=NULL, method='centroid', 
                          returnObject=TRUE){
  if(is.list(method) & length(method) > 1){
    message("Using the first element of argument 'method'")
    method <- method[[1]]
  }
  
  if(!method %in% c("centroid","intersect","clip")){
    stop("Method must be one of 'centroid', 'intersect', or 'clip'")
  }

  if(method=="clip" & returnObject==FALSE){
    warning("Clipping only valid when returning a spatial object.")
  }

  result <- get_zonal_index(X, zone, zoneField, method, returnObject)
  return(result)
}


#' @name zonalIndex
#' @export
zonalIndex.sfc <- function(X, zone, 
                           zoneField=NULL, method='centroid', 
                           returnObject=TRUE){
  if(any(class(X) == "sp") | any(class(X) == "stars")){
    # convert to sf
    X <- sf::st_as_sf(X)
  } else if (all(class(X) != "sf")) {
    stop("Object format not valid.")
  }
  
  if(is.list(method) & length(method) > 1){
    message("Using the first element of argument 'method'")
    method <- method[[1]]
  }
  
  if(!method %in% c("centroid","intersect","clip")){
    stop("Method must be one of 'centroid', 'intersect', or 'clip'")
  }
  
  if(method=="clip" & returnObject==FALSE){
    warning("Clipping only valid when returning a spatial object.")
  }

  result <- get_zonal_index(X, zone, zoneField, method, returnObject)
  return(result)
}


#' @name zonalIndex
#' @export
zonalIndex.sp <- function(X, zone, 
                          zoneField=NULL, method='centroid', 
                          returnObject=TRUE){
  zone <- sf::st_as_sf(zone)
  result <- zonalIndex(X, zone, zoneField, method, returnObject)
  return(result)
}


#' @name zonalIndex
#' @export
zonalIndex.stars <- function(X, zone, 
                             zoneField=NULL, method='centroid', 
                             returnObject=TRUE){
  zone <- sf::st_as_sf(zone)
  
  if(!is.null(zoneField)){
    # create zones by merging grid cells (polygons)
    zsplits <- split(zone, f=factor(zone[[zoneField]], 
                                    levels=unique(zone[[zoneField]])))
    zunion <- lapply(zsplits, sf::st_union)
    zgeom <- do.call(c, zunion)
    
    zone <- sf::st_sf(unique(zone[[zoneField]]), geometry=zgeom)
    names(zone)[1] <- zoneField
  }
  result <- zonalIndex(X, zone, zoneField, method, returnObject)
  return(result)
}


#' @name zonalIndex
#' @export
zonalIndex.Raster <- function(X, zone, 
                              zoneField=NULL, method='centroid', 
                              returnObject=TRUE){
  if(!is.null(zoneField)){
    zone <- zone[[zoneField]]
  }
  
  zone <- stars::st_as_stars(zone)
  result <- zonalIndex(X, zone, zoneField, method, returnObject)
  return(result)
}


#' @name zonalIndex
#' @export
zonalIndex.character <- function(X, zone, 
                                 zoneField=NULL, method='centroid', 
                                 returnObject=TRUE){
  zone <- sf::st_read(zone)
  result <- zonalIndex(X, zone, zoneField, method, returnObject)
  return(result)
}


get_zonal_index <- function(X, zone, 
                            zoneField=NULL, method='centroid', 
                            returnObject=TRUE){
  # clean up
  on.exit({ rm(list=ls()); gc() })
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
    suppressWarnings(X <- sf::st_cast(X, "POLYGON"))
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
  
  if(method=='centroid'){
    # keep backup
    polyGeo <- sf::st_geometry(X)
    suppressWarnings(X <- sf::st_centroid(X))
    X$polyGeo <- polyGeo
  }

  # intersects - binary predicate
  suppressMessages(ints <- sf::st_intersects(zone, X))
  hits <- which(lengths(ints)>0)
  ll <- lengths(ints)
  
  if(length(hits) > 0){
    intDT <- data.table::data.table("zID" = rep(hits, ll[hits]), 
                                    "xID" = unlist(ints, use.names=F))
    
    # geomDT <- list(xID=ints[hits])
    # data.table::setDT(geomDT)
    # # geomDT <- data.table::data.table(xID=ints[hits])
    # # geomDT[, (zoneField) := hits]
    # geomDT[, zID := hits]
    # data.table::setkey(geomDT, zID) # geomDT
    data.table::setkey(intDT, zID) # geomDT
    
    # get zone ID values
    zDT <- data.table::data.table(zone)
    zDT[, zID := 1:.N]
    data.table::setkey(zDT, zID)
    intDT[zDT, (zoneField) := mget(zoneField)]
    intDT[, zID := NULL]

    # # expand list of intersected features
    # intDT <- geomDT[, setNames(c(xID), "xID"), by=zID]
    # data.table::setcolorder(intDT, c("xID", zoneField))
    # data.table::setkey(intDT, xID)
    
    if(returnObject){
      # join
      Xdt <- data.table::data.table(X)
      Xdt[, xID := 1:.N]
      data.table::setkey(Xdt, xID)
      
      intDT[Xdt, on='xID', colnames(X) := mget(colnames(X))]
      # drop xID col
      intDT[, xID := NULL]
      data.table::setcolorder(intDT, neworder=colnames(X))
      
      if(method=='clip'){
        xGeo <- attr(X, "sf_column") # get variable name
        # match up geometries
        intDT[, zGeom := sf::st_geometry(zone)[get(zoneField)]]
        # loop over
        suppressMessages(clipGeom <- purrr::map2(intDT[[xGeo]],
                                intDT[["zGeom"]],
                                sf::st_intersection))
        # update geometry field
        intDT[, (xGeo) := sf::st_sfc(clipGeom, crs=sf::st_crs(X))]
        
        # build sf object
        result <- sf::st_as_sf(intDT[, c(colnames(X), zoneField), with=F])
      } else{
        result <- sf::st_as_sf(intDT)
        
        if(method=='centroid'){
          # sf::st_geometry(result) <- polyGeo
          sf::st_geometry(result) <- result$polyGeo
          result <- result[,!names(result) %in% "polyGeo"]
        }
      }
      
    } else{
      result <- intDT
    }
  } else{ # no intersections
    result <- NULL
    warning("No intersections found between footprints and zones.")
  } 
  return(result)
}
