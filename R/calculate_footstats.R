#' calculate_footstats
#' 
#' @title calculate_footstats: Feature statistics of building footprints
#' @description Calculate groups of metrics for building footprint datasets
#' @param X object with building footprint polygons. This argument can take
#'   multiple spatial types, including \code{sf} and \code{sp}, or a filepath
#'   string to a file, or a list where each member provides a spatial object or
#'   a filepath string.
#' @param index A spatial polygon file defining areas. Or a character or numeric
#'   value identifying a column within \code{X} which provides a zonal index for
#'   summarising values. Alternatively a vector of indices can be provided. If
#'   omitted all observations with \code{X} are assumed to be within one zone.
#' @param metrics character vector. Names of footprint statistics in the form of
#'   "fs_area_mean", etc. Other options include \code{ALL} or \code{NODIST} to
#'   calculate all available metrics and all except nearest neighbour distances,
#'   respectively.
#' @param minArea numeric. Minimum footprint area to filter \code{X}.
#' @param maxArea numeric. Maximum footprint area to filter \code{X}.
#' @param controlUnits (optional) named list. Elements can include
#'   \code{areaUnit}, \code{perimUnit}, and \code{distUnit}. The values for
#'   these items should be strings that can be coerced into a \code{units}
#'   object.
#' @param controlDist (Optional) named list to override default options for
#'   distance calculations. Elements can include \code{maxSearch} and
#'   \code{method}. Ignored if \code{metrics} does not include a distance
#'   calculation. See \code{\link[foot]{fs_nndist}}.
#' @param zoneMethod One of \code{'centroid', 'intersect', 'clip'}. How should
#'   footprints which span zones be clipped? Default is \code{'centroid'}.
#'   See \code{\link[foot]{zonalIndex}} for details.
#' @param verbose logical. Should progress messages be printed. 
#' Default \code{False}.
#' 
#' @return a \code{data.table} with an 'index' column identify the zone and
#'   named columns for each footprint summary statistic.
#'   
#' @examples 
#' data("kampala", package="foot")
#' buildings <- kampala$buildings
#' adminzones <- kampala$adminZones
#' 
#' calculate_footstats(buildings, 
#'                     index=adminzones, 
#'                     metrics=c("fs_area_mean","fs_area_cv"))    
#' 
#' @aliases calculate_footstats
#' @rdname calculate_footstats
#' 
#' @export
calculate_footstats <- function(X, 
                                index=NULL, 
                                metrics='all',
                                minArea=NULL,
                                maxArea=NULL,
                                controlUnits=NULL,
                                controlDist=NULL,
                                zoneMethod='centroid',
                                verbose=FALSE) UseMethod("calculate_footstats")

#' @name calculate_footstats
#' @export
calculate_footstats.sf <- function(X, index=NULL, metrics='all', 
                                   minArea=NULL,
                                   maxArea=NULL,
                                   controlUnits=NULL,
                                   controlDist=NULL,
                                   zoneMethod='centroid',
                                   verbose=FALSE){
  
  if(any(!sf::st_geometry_type(X) %in% c("POLYGON", "MULTIPOLYGON") )){
    message("Footprint statistics require polygon shapes.")
    stop()
  }
  
  result <- calc_fs_internal(X, index, metrics, minArea, maxArea,
                             controlUnits, controlDist, zoneMethod, verbose)
  
  return(result)
}


#' @name calculate_footstats
#' @export
calculate_footstats.sfc <- function(X, index=NULL, metrics='all', 
                                    minArea=NULL,
                                    maxArea=NULL,
                                    controlUnits=NULL,
                                    controlDist=NULL,
                                    zoneMethod='centroid',
                                    verbose=FALSE){
  # cast to sf for consistency
  X <- sf::st_as_sf(X)
  
  result <- calculate_footstats(X, index=index, metrics=metrics, 
                                minArea=minArea, maxArea=maxArea,
                                controlUnits=controlUnits,
                                controlDist=controlDist,
                                zoneMethod=zoneMethod,
                                verbose=verbose)
  return(result)
}


#' @name calculate_footstats
#' @export
calculate_footstats.sp <- function(X, index=NULL, metrics='all', 
                                   minArea=NULL,
                                   maxArea=NULL,
                                   controlUnits=NULL,
                                   controlDist=NULL,
                                   zoneMethod='centroid',
                                   verbose=FALSE){
  # convert to sf
  X <- sf::st_as_sf(X)
  
  result <- calculate_footstats(X, index=index, metrics=metrics, 
                                minArea=minArea, maxArea=maxArea,
                                controlUnits=controlUnits, 
                                controlDist=controlDist,
                                zoneMethod=zoneMethod,
                                verbose=verbose)
  return(result)
}


#' @name calculate_footstats
#' @export
calculate_footstats.character <- function(X, index=NULL, metrics='all',
                                          minArea=NULL,
                                          maxArea=NULL,
                                          controlUnits=NULL,
                                          controlDist=NULL,
                                          zoneMethod='centroid',
                                          verbose=FALSE){
  # attempt to read in file
  X <- sf::st_read(X)
  
  result <- calculate_footstats(X, index=index, metrics=metrics, 
                                minArea, maxArea,
                                controlUnits=controlUnits,
                                controlDist=controlDist,
                                zoneMethod=zoneMethod,
                                verbose=verbose)
  return(result)
}


#' @name calculate_footstats
#' @export
calculate_footstats.list <- function(X, index=NULL, metrics='all', 
                                     minArea=NULL,
                                     maxArea=NULL,
                                     controlUnits=NULL,
                                     controlDist=NULL,
                                     zoneMethod='centroid',
                                     verbose=FALSE){
  
  if(is.null(outputTag)){
    if(is.null(names(X))){
      outputTag <- seq_along(X)
    } else{
      outputTag <- names(X)
    }
  } else{
    if(length(outputTag) != length(X)){
      stop("Invalid output tag length.")
    } 
  }
  
  # expand other arguments - recycling values
  args <- list(index=index, metrics=metrics, 
               minArea=minArea, maxArea=maxArea,
               controlUnits=controlUnits, 
               controlDist=controlDist,
               zoneMethod=zoneMethod,
               verbose=verbose)
  maxL <- max(lengths(args), length(X))
  args <- lapply(args, rep, length.out=maxL)
  
  result <- lapply(seq_along(X), FUN=function(i){
    calculate_footstats(X[[i]], 
                        index=args$index[i], 
                        metrics=args$metrics[i],
                        minArea=args$minArea[i],
                        maxArea=args$maxArea[i],
                        controlUnits=args$controlUnits[i],
                        controlDist=args$controlDist[i],
                        zoneMethod=args$zoneMethod[i],
                        verbose=args$verbose[i])
  })
  
  return(result)  # should the list be simplified?
}


calc_fs_internal <- function(X, index, metrics, 
                             minArea, maxArea,
                             controlUnits, controlDist, zoneMethod,
                             verbose){

  if(is.na(st_crs(X))){
    stop("Polygons must have a spatial reference.")
  }
  
  if(verbose){ cat("Creating zonal index \n") }
  if(!is.null(index)){
    if(inherits(index, "Spatial")){
      index <- sf::st_as_sf(index)
    }
    if(inherits(index, "sf")){
      if(any(sf::st_geometry_type(index) %in% c("POLYGON","MULTIPOLYGON"))){
        indexZones <- index # make copy
        indexZones$index <- 1:nrow(indexZones)
        
        X <- zonalIndex(X, zone=index, method=zoneMethod, returnObject=TRUE)
        index <- "zoneID"
        # check for no intersecting
        if(is.null(X)){
          return(NULL)
        }
      } else{
        warning("Index must be a polygon or a column name. Ignoring input.")
        index <- NULL
      }
      # drop non-intersecting buildings
      X <- subset(X, !is.na(zoneID))
      # check for no intersecting
      if(nrow(X) == 0){
        return(NULL)
      }
    } else if(class(index) == "numeric" | class(index) == "character"){
      if(length(index) != nrow(X)){
        index <- colnames(X)[index[1]]
        if(!index %in% names(X)) stop("Index column not found in footprints.")
      }
    } else{
      warning("Index must be a polygon or a column name/index. Ignoring input.")
      index <- rep(1, nrow(X))
    }
  }
  # check for empty zone intersection
  if(is.null(X) | nrow(X) == 0){
    return(NULL)
  }
  
  # get full set of units - sort alphabetically to make matches
  providedUnits <- controlUnits
  
  controlUnits <- list(areaUnit=get_fs_units("fs_area_mean"),
                       perimUnit=get_fs_units("fs_perim_mean"),
                       distUnit=get_fs_units("fs_nndist_mean"))
  controlUnits <- controlUnits[order(names(controlUnits))]
  # update with provide values
  if(!is.null(providedUnits)){
    providedUnits <- providedUnits[order(names(providedUnits))]
    controlUnits[names(controlUnits) %in% names(providedUnits)] <- providedUnits
  }
  
  # get full set of distance calculation controls
  providedDist <- controlDist
  # set defaults
  controlDist <- list(maxSearch=100,
                      method='poly')
  controlDist <- controlDist[order(names(controlDist))]
  # update with provided control values
  if(!is.null(providedDist)){
    providedDist <- providedDist[order(names(providedDist))]
    controlDist[names(controlDist) %in% names(providedDist)] <- providedDist
  }
  
  # select and expand list of metrics
  if(verbose){ cat("Selecting metrics \n") }
  metrics <- get_fs_metrics(short_names=metrics, group=metrics)
  
  nnIndex <- FALSE
  if(any(grepl("nnindex", metrics, fixed=T))){
    # calculated after the main processing loop
    metrics <- metrics[!grepl("nnindex", metrics)]
    nnIndex <- TRUE
    # 
    if(!exists("indexZones")){
      warning("Nearest neighbour index requires zonal areas.")
      nnIndex <- FALSE
    }
  } 
  
  if(any(grepl("angle", metrics, fixed=T))){
    normalize <- TRUE
  }
  
  # pre-calculate unit geometry measures
  if(any(grepl("area", metrics, fixed=T)) |
     any(grepl("compact", metrics, fixed=T)) |
     !is.null(minArea) | !is.null(maxArea)){
    if(!"fs_area" %in% names(X)){
      if(verbose){ cat("Pre-calculating footprint areas \n") }
      X[["fs_area"]] <- fs_area(X, unit=controlUnits$areaUnit)
    }
  }
  
  if(any(grepl("perim", metrics, fixed=T)) |
     any(grepl("compact", metrics, fixed=T))){
    if(!"fs_perim" %in% names(X)){
      if(verbose){ cat("Pre-calculating footprint perimeters \n")}
      X[["fs_perim"]] <- fs_perimeter(X, unit=controlUnits$perimUnit)
    }
  }
  
  if(any(grepl("nndist", metrics, fixed=T))){
    if(!"fs_nndist" %in% names(X)){
      if(verbose){ cat("Pre-calculating nearest neighbour distances \n") }
      X[["fs_nndist"]] <- fs_nndist(X, 
                                    maxSearch=controlDist$maxSearch, 
                                    method=controlDist$method, 
                                    unit=controlUnits$distUnit)
    }
  }
  
  if(any(grepl("angle", metrics, fixed=T))){
    if(!"fs_angle" %in% names(X)){
      if(verbose){ cat("Pre-calculating angles \n") }
      X[["fs_angle"]] <- fs_mbr(X)
    }
  }
  
  # filter records
  if(!is.null(minArea)){
    if(verbose) { cat(paste0("Filtering features larger than ", minArea," \n")) }
    X <- subset(X, fs_area > units::as_units(minArea, 
                                             controlUnits$areaUnit))
  }
  
  if(!is.null(maxArea)){
    if(verbose) { cat(paste0("Filtering features smaller than ", maxArea," \n")) }
    X <- subset(X, fs_area < units::as_units(maxArea, 
                                             controlUnits$areaUnit))
  }
  if(nrow(X) == 0){ # filter removed all?
    return(NULL)
  }
  
  # creating the names of the functions to call
  metrics <- unique(metrics)
  metrics_calc <- paste0(metrics, "_calc")
  if(verbose){ cat("\nCalculating ", length(metrics_calc)  ," metrics ... \n")}
  
  result <- lapply(seq_along(metrics_calc), function(current_metric){
    if(verbose){ cat("  ", metrics[[current_metric]], " \n") }
    func <- get(metrics_calc[[current_metric]], mode="function")

    getUnit <- switch(get_fs_group(metrics[[current_metric]]), 
                      area = controlUnits$areaUnit,
                      perim = controlUnits$perimUnit,
                      dist = controlUnits$distUnit)
    
    assign("unit", value=getUnit, envir=parent.env(environment()))
    arguments <- names(formals(func))

    tryCatch(do.call(what=func,
                     args=mget(arguments, 
                               envir=parent.env(environment()), 
                               ifnotfound=list(NULL))
                    ),
             error = function(e){
               message("")
               stop(e)
             }
            )  
    })
  
  # merge all
  merged_result <- Reduce(function(...) merge(...), result)

  if(nnIndex){
    if(verbose){ cat("  Calculating nearest neighbour index... \n")}
    nniDT <- fs_nnindex(X, index=indexZones, unit=controlUnits$distUnit)
    merged_result <- merge(merged_result, 
                           nniDT[, list(index, fs_nnindex)], 
                           by="index")
  }
  if(verbose){ cat("Finished calculating metrics. \n") }

  return(merged_result)
}

