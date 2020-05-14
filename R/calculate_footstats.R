#' calculate_footstats
#' 
#' @title calculate_footstats: Feature statistics of building footprints
#' @description Calculate groups of metrics for building footprint datasets
#' @param X object with building footprint polygons. This argument can take
#'   multiple spatial types, including \code{sf} and \code{sp}, or a fileparth
#'   string to a building footprint files, or a list where each list member
#'   provides a spatial object or a filepath.
#' @param index A character or numeric value identifying a column within
#'   \code{X} which provides a zonal index for summarising values. Alternatively
#'   a vector of indices can be provided. If omitted all observations with
#'   \code{X} are assumed to be within one zone.
#' @param metrics character vector. Names of footprint statistics in the form of
#'   "fs_area_mean", etc. Other options include \code{ALL} or \code{NODIST} to
#'   calculate all available metrics and all except nearest neighbour distances,
#'   respectively.
#' @param template (optional). When creating a gridded output, a supplied
#'   \code{stars} or \code{raster} dataset to align the data.
#' @param outputPath (optional). When creating a gridded output, a path for the
#'   location of the output.
#' @param outputTag (optional). A character string that will be added tagged to
#'   the beginning of the output gridded files. If \code{X} is a \code{list},
#'   then the names of the list items will be used or a vector of tags of the
#'   same length as \code{X} should be supplied.
#' @param driver character. Currently supports geotiff ("GTiff").
#' @param verbose logical. Should progress messages be printed. 
#' Default \code{False}.
#' 
#' @return a \code{data.table} with an 'index' column and named columns for each
#'   footprint statistic. Alternatively, geoTiffs of 
#'   
#' @author Chris Jochem
#' 
#' @aliases calculate_footstats
#' @rdname calculate_footstats
#' 
#' @export
calculate_footstats <- function(X, 
                                index=NULL, 
                                metrics='all',
                                gridded=TRUE, 
                                template=NULL,
                                outputPath=NULL,
                                outputTag=NULL,
                                driver="GTiff",
                                verbose=FALSE) UseMethod("calculate_footstats")

#' @name calculate_footstats
#' @export
calculate_footstats.sf <- function(X, index=NULL, metrics='all', 
                                   gridded=TRUE, template=NULL, 
                                   outputPath=NULL, 
                                   outputTag=NULL,
                                   driver="GTiff",
                                   verbose=FALSE){
  
  if(any(!st_geometry_type(X) %in% c("POLYGON", "MULTIPOLYGON") )){
    message("Footprint statistics require polygon shapes.")
    stop()
  }
  
  result <- calc_fs_internal(X, index, metrics, gridded, 
                             template, outputPath, outputTag, driver, verbose)
  
  return(result)
}


#' @name calculate_footstats
#' @export
calculate_footstats.sfc <- function(X, index=NULL, metrics='all', 
                                    gridded=TRUE, template=NULL, 
                                    outputPath=NULL, 
                                    outputTag=NULL,
                                    driver="GTiff",
                                    verbose=FALSE){
  # cast to sf for consistency
  X <- sf::st_as_sf(X)
  
  result <- calculate_footstats(X, index=index, metrics=metrics, 
                                gridded=gridded, template=template, 
                                outputPath=outputPath, 
                                outputTag=outputTag,
                                driver=driver,
                                verbose=verbose)
  return(result)
}


#' @name calculate_footstats
#' @export
calculate_footstats.sp <- function(X, index=NULL, metrics='all', 
                                   gridded=TRUE, template=NULL, 
                                   outputPath=NULL, 
                                   outputTag=NULL,
                                   driver="GTiff",
                                   verbose=FALSE){
  # convert to sf
  X <- sf::st_as_sf(X)
  
  result <- calculate_footstats(X, index=index, metrics=metrics, 
                                gridded=gridded, template=template, 
                                outputPath=outputPath, 
                                outputTag=outputTag,
                                driver=driver,
                                verbose=verbose)
  return(result)
}


#' @name calculate_footstats
#' @export
calculate_footstats.character <- function(X, index=NULL, metrics='all', 
                                          gridded=TRUE, template=NULL, 
                                          outputPath=NULL, 
                                          outputTag=NULL,
                                          driver="GTiff",
                                          verbose=FALSE){
  # attempt to read in file
  X <- sf::st_read(X)
  
  result <- calculate_footstats(X, index=index, metrics=metrics, 
                                gridded=gridded, template=template, 
                                outputPath=outputPath, 
                                outputTag=outputTag,
                                driver=driver,
                                verbose=verbose)
  return(result)
}


#' @name calculate_footstats
#' @export
calculate_footstats.list <- function(X, index=NULL, metrics='all', 
                                     gridded=TRUE, template=NULL, 
                                     outputPath=NULL, 
                                     outputTag=NULL,
                                     driver="GTiff",
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
  args <- list(index=index, metrics=metrics, gridded=gridded, 
               template=template, outputPath=outputPath, driver=driver, 
               verbose=verbose)
  maxL <- max(lengths(args), length(X))
  args <- lapply(args, rep, length.out=maxL)
  
  result <- lapply(seq_along(X), FUN=function(i){
    calculate_footstats(X[[i]], 
                        index=args$index[i], 
                        metrics=args$metrics[i],
                        gridded=args$gridded[i], 
                        template=args$template[i],
                        outputPath=args$outputPath[i], 
                        outputTag=outputTag[[i]],
                        driver=args$driver[i], 
                        verbose=args$verbose[i])
  })
  
  return(result)  # should the list be simplified?
}


calc_fs_internal <- function(X, index, metrics, 
                             gridded, template, 
                             outputPath, outputTag, driver, verbose){
  
  if(is.na(st_crs(X))){
    stop("Polygons must have a spatial reference.")
  }
  
  if(verbose){ cat("Creating index \n") }
  
  if(!is.null(index)){
    if(inherits(index, "sf")){
      if(any(sf::st_geometry_type(index) %in% c("POLYGON","MULTIPOLYGON"))){
        indexZones <- index # make copy
        indexZones$index <- 1:nrow(indexZones)
        
        X <- zonalIndex(X, index, returnObject=TRUE)
        index <- "zoneID"
      } else{
        warning("Index must be a polygon or a column name. Ignoring input.")
        index <- NULL
      }
    } else if(class(index) == "numeric"){
      if(length(index) != nrow(X)) stop("Index length does not match footprints.")
      
    } else if(class(index) == "character"){
      index <- index[1]
      
    } else{
      warning("Index must be a polygon or a column name. Ignoring input.")
      index <- NULL
    }
  }
  
  if(verbose){ cat("Selectinig metrics \n") }

  if(toupper(metrics[1]) == 'ALL'){
    metrics <- foot::fs_footprint_metrics$name
  }
  
  if(toupper(metrics[1]) == "NODIST"){
    # metrics <- foot::fs_footprint_metrics$name
    metrics <- metrcs[!grepl("NNdist", metrics)]
    metrics <- foot::fs_footprint_metrics[foot::fs_footprint_metrics$group != "NNdist", 
                                          "name"]
  }
  
  if(any(grepl("area_cv", metrics, fixed=T))){
    metrics <- c(metrics, "fs_area_mean", "fs_area_sd")
    metrics <- metrics[!grepl("area_cv", metrics)]
    area_cv <- TRUE
  } else{
    area_cv <- FALSE
  }
  
  if(any(grepl("perim_cv", metrics, fixed=T))){
    metrics <- c(metrics, "fs_perim_mean", "fs_perim_sd")
    metrics <- metrics[!grepl("perim_cv", metrics)]
    perim_cv <- TRUE
  } else{
    perim_cv <- FALSE
  }
  
  if(any(grepl("compact", metrics, fixed=T))){
    compact <- TRUE
  } else{
    compact <- FALSE
  }
  
  if(any(grepl("NNindex", metrics, fixed=T))){
    metrics <- c(metrics, "fs_NNdist_mean", "fs_count")
    metrics <- metrics[!grepl("NNindex", metrics)]
    nnIndex <- TRUE
    
    if(exists("indexZones")){
      zonalArea <- data.table::data.table(index=indexZones$index, 
                                          zoneArea=fs_area(indexZones, unit="m^2"))
    } else{
      warnings("Nearest neighbour index requires zonal areas.")
      nnIndex <- FALSE
      # zoneAreas <- fs_area(sf::st_as_sfc(sf::st_bbox(X), crs=sf::st_crs(X)))
      # zonalArea <- data.table(zoneID=1:length(zoneAreas), zoneArea=zoneAreas)
    }
    
  } else{
    nnIndex <- FALSE
  }
  
  if(any(grepl("angle", metrics, fixed=T))){
    normalize <- TRUE
  }
  
  # pre-calcluate unit geometry measures
  if(any(grepl("area", metrics, fixed=T)) | compact==TRUE){
    if(verbose){ cat("Pre-calculating footprint areas \n") }
    unit <- "ha"
    X[["fs_area"]] <- fs_area(X, unit)
  }
  
  if(any(grepl("perim", metrics, fixed=T)) | compact==TRUE){
    if(verbose){ cat("Pre-calculating footprint perimeters \n")}
    X[["fs_perim"]] <- fs_perimeter(X, 
                                    unit=fs_footprint_metrics[fs_footprint_metrics$name=="fs_perim_mean",
                                                              "default_units"])
  }
  
  if(any(grepl("NNdist", metrics, fixed=T))){
    if(verbose){ cat("Pre-calculating nearest neighbour distances \n") }
    X[["fs_NNdist"]] <- fs_NNdist(X, 
                                  unit=fs_footprint_metrics[fs_footprint_metrics$name=="fs_NNdist_mean",
                                                            "default_units"])
  }
  
  if(any(grepl("angle", metrics, fixed=T))){
    if(verbose){ cat("Pre-calculating angles \n") }
    X[["fs_angle"]] <- sapply(sf::st_geometry(X), fs_mbr)
  }
  
  # creating the names of the functions to call
  metrics <- unique(metrics)
  metrics_calc <- paste0(metrics, "_calc")
  if(verbose){ cat("\nCalculating ", length(metrics_calc)  ," metrics ... \n")}
  
  result <- lapply(seq_along(metrics_calc), function(current_metric){
    if(verbose){ cat("  ", metrics[[current_metric]], " \n") }
    func <- get(metrics_calc[[current_metric]], mode="function")
    
    getUnit <- fs_footprint_metrics$default_units[match(metrics[[current_metric]], fs_footprint_metrics$name)]
    assign("unit", value=getUnit, envir=parent.env(environment()))
    arguments <- names(formals(func))

    tryCatch(do.call(what=func,
                     args=mget(arguments, envir=parent.env(environment()), ifnotfound=list(NULL))
                    ),
             error = function(e){
               message("")
               stop(e)
             }
            )  
    })
  
  # merge all
  merged_result <- Reduce(function(...) merge(...), result)
  
  if(area_cv){
    merged_result[, fs_area_cv:=fs_area_ha_sd / fs_area_ha_mean]
  }
  
  if(perim_cv){
    merged_result[, fs_perim_cv:=fs_perim_m_sd / fs_perim_m_mean]
  }
  
  if(nnIndex){
    nniDT <- merged_result[, list(index, fs_NNdist_m_mean, fs_count)]
    nniDT <- merge(nniDT, zonalArea, by.x="index", by.y="zoneID")
    nniDT[, fs_NNindex := fs_NNdist_m_mean / (0.5 * sqrt(zoneArea / fs_count)), by=index]
    units(nniDT$fs_NNindex) <- NULL
    
    merged_result <- merge(merged_result, nniDT[, list(index, fs_NNindex)], by=index)
  }
  if(verbose){ cat("Finished calculating metrics. \n") }
  
  # output
  if(gridded==TRUE){
    if(verbose){ cat("\nCreating gridded datasets \n") }
    
    if(is.null(outputPath)){
      outputPath <- tempdir()
    } else{
      dir.create(outputPath)
    }
    
    # get template for aligning gridded output
    if(is.null(template)){
      template <- stars::st_as_stars(sf::st_bbox(X), values=NA_real_)  # default resolution
    } else if(inherits(template, "stars")){
      # no change?
    } else if(class(template) == "RasterLayer"){
      template <- stars::st_as_stars(template)
    } else if(class(template) == "character"){
      template <- stars::read_stars(template)[1]
    } else{
      stop("Error opening template dataset.")
    }
    
    template[!is.na(template)] <- NA
        
    if(exists("indexZones")){ # process buildings
      spatial_result <- merge(indexZones, merged_result, by.x="index", by.y="index")
      
    } else{
      if(sf::st_crs(template) != sf::st_crs(X)){
        stop("CRS for buildings and template raster not matching.")
      }
      
      if(!sf::st_geometry_type(X) %in% c("POINT")){
        X <- sf::st_centroid(X)
      } 
      spatial_result <- merge(X, merged_result, by.x=index, by.y="index")
    }
    
    if(verbose){ cat("Writing grids... \n") }
    for(val in names(merged_result)[!names(merged_result) %in% "index"]){
      if(is.null(outputTag)){
        outName <- paste(val, "tif", sep=".")
      } else{
        outName <- paste(outputTag, "_", val, "tif", sep=".")
      }
      
      if(verbose){ cat(" ", outName, "\n") }
      stars::st_rasterize(spatial_result[val], 
                          file=file.path(outputPath, outName), 
                          template=template,
                          driver=driver,
                          options="compress=LZW")
    }
  }
  
  if(verbose){ cat("Finished!\n") }
  return(merged_result)
}

