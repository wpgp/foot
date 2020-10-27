#' calculate_footstats
#' 
#' @title calculate_footstats: Feature statistics of building footprints
#' @description Calculate groups of metrics for building footprint datasets
#' @param X object with building footprint polygons. This argument can take
#'   multiple spatial types, including \code{sf} and \code{sp}, or a filepath
#'   string to a file.
#' @param zone A spatial polygon file defining areas. Or a string identifying a
#'   column within \code{X} which provides a zonal index for summarising values.
#'   Alternatively a vector of indices (with \code{length(index)==nrow(X)}) can
#'   be provided. If omitted, all observations in \code{X} are assumed to be
#'   within one zone.
#' @param what list of strings naming the columns or built-in geometry measures to
#'   calculate for each footprint. Other options include \code{'all'} or
#'   \code{'nodist'} to calculate all available characteristics and all except
#'   nearest-neighbour distance metrics.
#' @param how list of strings naming functions to be used to calculate summary
#'   statistics. The functions can be built-in functions (e.g. "mean","sd"), or
#'   user-defined function names.
#' @param controlZone (optional) named list. Setting controls passed on to 
#'   \code{\link[foot]{zonalIndex}}. Elements can include \code{zoneName} and
#'   \code{method}.
#' @param controlUnits (optional) named list. Elements can include
#'   \code{areaUnit}, \code{perimUnit}, and \code{distUnit}. The values for
#'   these items should be strings that can be coerced into a \code{units}
#'   object.
#' @param controlDist (optional) named list to override default
#'   options for distance calculations. Elements can include \code{maxSearch}
#'   and \code{method}. Ignored if \code{metrics} does not include a distance
#'   calculation. See \code{\link[foot]{fs_nndist}}. 
#' @param filter (optional) named list with \code{minArea} and \code{maxArea}.
#'   These are numeric values to filter footprints prior to processing. Default
#'   values are \code{NULL} and do not filter any records.
#' @param verbose logical. Should progress messages be printed. 
#' Default \code{TRUE}.
#' 
#' @details \code{calculate_footstats} is a wrapper function combining several
#'   internal functions from \code{foot}. It can calculate various geometric
#'   measures for each footprint polygon, including area, perimeter,
#'   compactness, shape, angle of rotation, and nearest neighbour distance. To
#'   find the list of built-in characteristics and summary metrics, use
#'   \code{list_fs()}.
#'   
#'   The \code{what} and \code{how} arguments are lists specifying the
#'   characteristics and the summary metrics to calculated, respectively. Each
#'   "how" function will be applied to each "what" characteristic. To apply a
#'   metric to only a subset of characteristics, nested lists, with groups of
#'   characteristics and functions can be supplied. See examples. 
#'   
#'   The \code{control} arguments are sets of lists with named arguments that
#'   pass on these parameters to other \code{foot} functions. These are
#'   optional.
#' 
#' @return a \code{data.table} with an index column identifying the zones and
#'   named columns for each footprint summary statistic.
#'   
#' @examples 
#' data("kampala", package="foot")
#' buildings <- kampala$buildings
#' adminzones <- kampala$adminZones
#' 
#' # no summary statistics, just geometry calculations
#' calculate_footstats(buildings, adminzones, what=list("area","perimeter"))
#' 
#' # average building footprint area
#' calculate_footstats(buildings, 
#'                     zone=adminzones, 
#'                     what="area", how="mean")
#'                     
#' # calculate multiple metrics - nested lists to group arguments
#' calculate_footstats(buildings, adminzones, 
#'                     what=list(list("area"), list("perimeter")), 
#'                     how=list(list("mean","sum"), list("sd","cv")))
#'                     
#' @seealso \link[foot]{zonalIndex}, \link[foot]{fs_nndist}, \link[foot]{list_fs}
#' 
#' @aliases calculate_footstats
#' @rdname calculate_footstats
#' 
#' @export
calculate_footstats <- function(X, zone=NULL, what='all', how=NULL,
                                controlZone=list(zoneName="zoneID", 
                                                 method="centroid"), 
                                controlUnits=list(areaUnit="m^2", 
                                                  perimUnit="m", 
                                                  distUnit="m"), 
                                controlDist=list(maxSearch=100, 
                                                 method="centroid", 
                                                 unit=controlUnits$distUnit),
                                filter=list(minArea=NULL, 
                                            maxArea=NULL),
                                verbose=TRUE) UseMethod("calculate_footstats") 

#' @name calculate_footstats
#' @export
calculate_footstats.sf <- function(X, zone=NULL, what='all', how=NULL,
                                   controlZone=list(zoneName="zoneID", 
                                                    method="centroid"), 
                                   controlUnits=list(areaUnit="m^2", 
                                                     perimUnit="m", 
                                                     distUnit="m"), 
                                   controlDist=list(maxSearch=100, 
                                                    method="centroid", 
                                                    unit=controlUnits$distUnit),
                                   filter=list(minArea=NULL, 
                                               maxArea=NULL),
                                   verbose=TRUE){
  
  if(any(!sf::st_geometry_type(X) %in% c("POLYGON", "MULTIPOLYGON") )){
    message("Footprint statistics require polygon shapes.")
    stop()
  }
  
  result <- calc_fs_internal(X, zone, what, how,
                             controlZone, controlUnits, controlDist,
                             filter, verbose)
  
  return(result)
}


#' @name calculate_footstats
#' @export
calculate_footstats.sfc <- function(X, zone=NULL, what='all', how=NULL,
                                    controlZone=list(zoneName="zoneID", 
                                                     method="centroid"), 
                                    controlUnits=list(areaUnit="m^2", 
                                                      perimUnit="m", 
                                                      distUnit="m"), 
                                    controlDist=list(maxSearch=100, 
                                                     method="centroid", 
                                                     unit=controlUnits$distUnit),
                                    filter=list(minArea=NULL, 
                                                maxArea=NULL),
                                    verbose=TRUE){
  # cast to sf for consistency
  X <- sf::st_as_sf(X)
  
  result <- calculate_footstats(X, zone=zone, what=what, how=how, 
                                controlZone=controlZone,
                                controlUnits=controlUnits,
                                controlDist=controlDist,
                                filter=filter,
                                verbose=verbose)
  return(result)
}


#' @name calculate_footstats
#' @export
calculate_footstats.sp <- function(X, zone=NULL, what='all', how=NULL,
                                   controlZone=list(zoneName="zoneID", 
                                                    method="centroid"), 
                                   controlUnits=list(areaUnit="m^2", 
                                                     perimUnit="m", 
                                                     distUnit="m"), 
                                   controlDist=list(maxSearch=100, 
                                                    method="centroid", 
                                                    unit=controlUnits$distUnit),
                                   filter=list(minArea=NULL, 
                                               maxArea=NULL),
                                   verbose=TRUE){
  # convert to sf
  X <- sf::st_as_sf(X)
  
  result <- calculate_footstats(X, zone, what, how,
                                controlZone, controlUnits, controlDist,
                                filter, verbose)
  return(result)
}


#' @name calculate_footstats
#' @export
calculate_footstats.character <- function(X, zone=NULL, what='all', how=NULL,
                                          controlZone=list(zoneName="zoneID", 
                                                           method="centroid"), 
                                          controlUnits=list(areaUnit="m^2", 
                                                            perimUnit="m", 
                                                            distUnit="m"), 
                                          controlDist=list(maxSearch=100, 
                                                           method="centroid", 
                                                           unit=controlUnits$distUnit),
                                          filter=list(minArea=NULL, 
                                                      maxArea=NULL),
                                          verbose=TRUE){
  # attempt to read in file
  X <- sf::st_read(X)
  
  result <- calculate_footstats(X, zone, what, how,
                                controlZone, controlUnits, controlDist,
                                filter, verbose)
  return(result)
}


# internal processing function
calc_fs_internal <- function(X, zone, what, how,
                             controlZone, controlUnits, controlDist,
                             filter, verbose){
  
  if(is.na(st_crs(X))){
    stop("Footprints must have a spatial reference.")
  }
 
  # get characteristics and metrics
  if(verbose){ cat(paste0("Selecting metrics \n")) }
  if("all" %in% what){ 
    argsDF <- list_fs(what="all", how=how) 
  } else if("nodist" %in% what){ 
    argsDF <- list_fs(what="nodist", how=how) 
  } else if(!is.null(how)){
    argsX <- crossargs(what, how)
    argsDF <- do.call(rbind, argsX)
    argsDF <- unique(argsDF)
  } 
  
  if(is.null(how)){
    argsDF <- list_fs(what, how=NULL)
    uchars <- unlist(unique(argsDF))
    if("nndist" %in% argsDF){ 
      calcD <- TRUE
    } else{
      calcD <- FALSE
    }
  } else{
    uchars <- unlist(unique(argsDF$cols))
    calcD <- ifelse(nrow(argsDF[argsDF$cols=="nndist" & 
                                  argsDF$funs != "nnindex",])>0, 
                    TRUE, FALSE)
  }
  
  # set defaults for controls
  if(verbose){ cat("Setting control values. \n") }
  # defaults for zonal indexing
  providedZone <- controlZone
  controlZone <- list(zoneName="zoneID", method="centroid")
  controlZone <- controlZone[order(names(controlZone))]
  # update with provide values
  if(!is.null(providedZone)){
    providedZone <- providedZone[order(names(providedZone))]
    controlZone[names(controlZone) %in% names(providedZone)] <- providedZone
  }
  
  # get full set of units - sort alphabetically to make matches
  providedUnits <- controlUnits
  controlUnits <- list(areaUnit="m^2", perimUnit="m", distUnit="m")
  controlUnits <- controlUnits[order(names(controlUnits))]
  # update with provide values
  if(!is.null(providedUnits)){
    providedUnits <- providedUnits[order(names(providedUnits))]
    controlUnits[names(controlUnits) %in% names(providedUnits)] <- providedUnits
  }
  
  # defaults for distance measures
  providedDist <- controlUnits
  controlDist=list(maxSearch=100, 
                   method="centroid", 
                   unit=controlUnits$distUnit)
  controlDist <- controlDist[order(names(controlDist))]
  # update with provide values
  if(!is.null(providedDist)){
    providedDist <- providedDist[order(names(providedDist))]
    controlDist[names(controlDist) %in% names(providedDist)] <- providedDist
  }
  
  # create zonal index
  if(!is.null(zone)){
    if(verbose){ cat("Creating zonal index \n") }
    if(inherits(zone, "Spatial")){
      zone <- sf::st_as_sf(zone)
    }
    if(inherits(zone, "sf")){
      if(sf::st_crs(zone) != sf::st_crs(X)){
        stop("Spatial reference for footprints and zones does not match.")
      }
      if(all(sf::st_geometry_type(zone) %in% c("POLYGON","MULTIPOLYGON"))){
        if(!controlZone$zoneName %in% colnames(zone)){
          zone[[controlZone$zoneName]] <- 1:nrow(zone)
        }
        # create index
        X <- zonalIndex(X, 
                        zone=zone, 
                        zoneField=controlZone$zoneName, 
                        method=controlZone$method, 
                        returnObject=TRUE)
        # check for no intersecting
        if(is.null(X)){
          return(NULL)
        }
        # get the geometry attribute
        geomField <- attr(X, which="sf_column")
      } else{ # found 'sf' data but not polygons
        warning("Zone must be a spatial polygon. Ignoring input.")
        zone <- NULL
        X[[controlZone$zoneName]] <- rep(1, nrow(X))
      }
      # drop non-intersecting buildings
      # X <- subset(X, !is.na(controlZone$zoneName))
      X <- sf::st_as_sf(subset(data.table::setDT(X), !is.na(controlZone$zoneName)))
      # check for no intersecting
      if(nrow(X) == 0){
        if(verbose){ cat("No records found in zones. \n") }
        return(NULL)
      }
    } else if(FALSE){ #is raster or is stars){
      
    } else if(is.character(zone)){ # could be column or a vector of codes
      if(length(zone)==1){ # do we only have 1 footprint?
        if(nrow(X)>1){ # it must be a column name
          if(!zone %in% colnames(X)){
            stop("Index column not found in footprints.")
          } else{
            controlZone$zoneName <- zone
            zone <- NULL
          }
        } # potential issue if 1 row X and 1 column name - won't affect calcs
      } else if(length(zone != nrow(X))){
        stop("Invalid length of zonal index.")
      }
    } else if(is.numeric(zone)){
      if(length(zone) != nrow(X)){
        stop("Invalid length of zonal index.")
      } else{
        X[[controlZone$zoneName]] <- zone
        zone <- NULL
      }
    }
  } else{ # zone is null
    # if(verbose) cat("No zone index provided, treating as one group. \n")
    X[[controlZone$zoneName]] <- rep(1, nrow(X))
  }

  # check for empty zone intersection
  if(is.null(X) | nrow(X) == 0){
    return(NULL)
  }

  # pre-calculate 'whats'
  if('area' %in% uchars |
     !is.null(filter$minArea) | !is.null(filter$maxArea)){
    if(!'area' %in% colnames(X)){
      if(verbose){ cat("Pre-calculating areas \n") }
      X[['area']] <- fs_area(X, unit=controlUnits$areaUnit)
    } else{
      if(verbose){ cat("Area data column already exists \n") }
    }
  }

  # filter records
  if(!is.null(filter$minArea)){
    if(verbose) { cat(paste0(" Filtering features larger than ", 
                             filter$minArea," \n")) }
    X <- sf::st_as_sf(subset(data.table::setDT(X), 
                             area > units::set_units(filter$minArea, 
                                                     controlUnits$areaUnit,
                                                     mode="standard")))
  }
  if(!is.null(filter$maxArea)){
    if(verbose) { cat(paste0(" Filtering features smaller than ", 
                             filter$maxArea," \n")) }
    
    X <- sf::st_as_sf(subset(data.table::setDT(X), 
                             area < units::set_units(filter$maxArea, 
                                                     controlUnits$areaUnit,
                                                     mode="standard")))
  }
  if(nrow(X) == 0){ # filter removed all?
    if(verbose){ cat("No records found in zones. \n") }
    return(NULL)
  }
  
  if('perimeter' %in% uchars){
    if(!'perimeter' %in% colnames(X)){
      if(verbose){ cat("Pre-calculating perimeters \n") }
      X[['perimeter']] <- fs_perimeter(X, unit=controlUnits$perimUnit)
    } else{
      if(verbose){ cat("Perimeter data column already exists \n") }
    }
  }
  
  if('angle' %in% uchars){
    if(!'angle' %in% colnames(X)){
      if(verbose){ cat("Pre-calculating angles \n") }
      X[['angle']] <- fs_mbr(X, returnShape=FALSE)
    } else{
      if(verbose){ cat("Angle data column already exists \n") }
    }
  }
  
  if('shape' %in% uchars){
    if(!'shape' %in% colnames(X)){
      if(verbose){ cat("Pre-calculating shape \n") }
      X[['shape']] <- fs_shape(X, unit=controlUnits$areaUnit)
    } else{
      if(verbose){ cat("Shape data column already exists \n") }
    }
  }
  
  if('compact' %in% uchars){
    if(!'compact' %in% colnames(X)){
      if(verbose){ cat("Pre-calculating compactness \n") }
      X[['compact']] <- fs_compact(X)
    } else{
      if(verbose){ cat("Shape data column already exists \n") }
    }
  }
  
  if('settled' %in% uchars){
    if(!'settled' %in% colnames(X)){
      X[['settled']] <- 1
    } 
  }
  
  if('nndist' %in% uchars & calcD){
    if(!'dist' %in% colnames(X)){
      if(verbose){ cat("Pre-calculating nearest neighbour distances \n") }
        X[['nndist']] <- fs_nndist(X, 
                                   maxSearch=controlDist$maxSearch, 
                                   method=controlDist$method, 
                                   unit=controlUnits$distUnit)
    } else{
      if(verbose){ cat("NN distance data column already exists. \n") }
    }
  }
  
  if(is.null(how)){
    if(verbose){ cat("No summary functions found, returning metrics. \n\n") }
    return(data.table::data.table(sf::st_drop_geometry(X[, uchars])))
  }
  # check for invalid characteristic/function pairs
  # but need to exclude user-supplied columns
  # if(verbose){ cat("Checking for valid function parameter pairings. \n") }
  if(is.null(zone)){ 
    argsDF <- argsDF[!argsDF$funs == "nnindex", ]
  }
  # # update parameters
  # argsDF$params <- argsDF$cols
  # argsDF[argsDF$funs=="nnindex" & 
  #          argsDF$cols=="nndist", "params"] <- fs_varlist(geomField, zone)
  
  # convert to data.frame
  DT <- data.table::setDT(X)
  data.table::setkeyv(DT, controlZone$zoneName)
  
  # main processing loop -- working!
  if(verbose){ cat("\nCalculating ", nrow(argsDF)  ," metrics ... \n")}
  
  result <- lapply(1:nrow(argsDF), function(i){
    params <- unlist(argsDF[i, "cols"])
    calc_func <- unlist(argsDF[i, "funs"])
    if(verbose){ cat("  ", params, calc_func, " \n") }
    newNm <- paste(paste(params, collapse="_"), calc_func, sep="_")
    
    if(calc_func == "nnindex"){
      calc_func <- gen_nnindex(zone, controlZone$zoneName, controlUnits$distUnit)
      params <- geomField
    }  

    tryCatch(DT[, setNames(list(do.call(calc_func, unname(.SD))),
                           newNm),
                by=eval(controlZone$zoneName),
                .SDcols=params],
             error = function(e){
               message("")
               stop(e)
             }
    )
  })
  
  # result <- lapply(1:nrow(argsDF), function(i){
  #   params <- unlist(argsDF[i, "cols"])
  #   calc_func <- unlist(argsDF[i, "funs"])
  #   if(verbose){ cat("  ", params, " ", calc_func, " \n") }
  #   newNm <- paste(paste(params, collapse="_"), calc_func, sep="_")
  # 
  #   tryCatch(DT[, setNames(list(do.call(calc_func, unname(.SD))),
  #                          newNm),
  #               by=eval(controlZone$zoneName),
  #               .SDcols=params],
  #            error = function(e){
  #              message("")
  #              stop(e)
  #            }
  #   )
  # })
  
  # merge all
  merged_result <- Reduce(function(...) merge(...), result)
  
  if(verbose){ cat("Finished calculating metrics. \n") }
  
  return(merged_result)
}
  