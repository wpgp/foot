#' calculate_bigfoot
#' 
#' @title calculate_bigfoot: Gridded feature statistics for large sets of
#'   building footprint polygons
#' @description Calculate selected metrics of building footprints for
#'   high-spatial resolution gridded outputs.
#' @param X object with building footprint polygons. This argument can take
#'   multiple spatial types, including \code{sf} and \code{sp}, or a filepath
#'   string to a file, or a list where each member provides a spatial object or
#'   a filepath string.
#' @param what list of strings naming the columns or built-in geometry measures to
#'   calculate for each footprint. Other options include \code{'all'} or
#'   \code{'nodist'} to calculate all available characteristics and all except
#'   nearest-neighbour distance metrics.
#' @param how list of strings naming functions to be used to calculate summary
#'   statistics. The functions can be built-in functions (e.g. "mean","sd"), or
#'   user-defined function names.
#' @param focalRadius numeric. Distance in meters for a buffer around each
#'   template pixel. Creates a focal processing window for metrics.
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
#' @param template (optional). When creating a gridded output, a supplied
#'   \code{stars} or \code{raster} dataset to align the data.
#' @param parallel logical. Should a parallel backend be used to process the
#'   tiles. Default is \code{TRUE}.
#' @param nCores number of CPU cores to use if \code{parallel} is \code{TRUE}.
#'   Default is 1 less than the available CPUs.
#' @param tileSize number of pixels per side of a tile. Can be a vector of
#'   length 2 (rows, column pixels). Default is \code{c(500, 500)}.
#' @param outputPath (optional). When creating a gridded output, a path for the
#'   location of the output. Default is the temp directory.
#' @param outputTag (optional). A character string that will be added to the
#'   beginning of the output name for the gridded files.
#' @param tries (optional). The number of attempts to write a tile to the output
#'   file. Default is 100.
#' @param restart (optional). A tile index (or vector of tile indices) from
#'   which to restart processing. Default is \code{NULL} to always process from
#'   the beginning.
#' @param verbose logical. Should progress messages be printed and a log of
#'   processed tiles be created. Default \code{TRUE}.
#' 
#' @details \code{calculate_bigfoot} provides a wrapper for a workflow to
#'   process vector polygons of structures to create a gridded output summary of
#'   morphology measures. The function wraps \code{calculate_footstats} along
#'   with other geometry functions of \code{foot} and read/writing functions
#'   from \code{stars} and \code{sf}.
#'   
#'   The suggested way of using this function is to supply character strings for
#'   \code{X} and \code{template} rather than objects. Using strings is more
#'   memory-efficient. This function processes based on 'tiles' or sub-regions
#'   of the template grid and will only read in the portion of the object needed
#'   for the calculations.
#' 
#' @return Invisible. Returns a vector of paths to the output files.
#' 
#' @examples 
#' data("kampala", package="foot")
#' buildings <- kampala$buildings
#' templateGrid <- kampala$mastergrid
#' 
#' calculate_bigfoot(X=buildings,
#'                   what=list(list("shape"), list("perimeter")),
#'                   how=list(list("mean"), list("sum")),
#'                   controlUnits=list(areaUnit="m^2"),
#'                   filter = list(minArea=50,  # footprints must be larger than 50 m^2
#'                                 maxArea=1000),  # footprints must be smaller than 1000 m^2
#'                   template=templateGrid, 
#'                   outputPath=tempdir(),  
#'                   outputTag="kampala",
#'                   parallel=FALSE,
#'                   verbose=TRUE)  
#'
#' # read one of the output files and plot as a raster layer
#' outGrid <- raster::raster(file.path(tempdir(), "kampala_count.tif"))
#' raster::plot(outGrid)
#' 
#' @seealso \link[foot]{calculate_footstats}
#' 
#' @import doParallel
#' @import parallel
#' @import foreach
#' @import filelock
#' @import sf
#' @import stars
#' @import iterators
#' 
#' @aliases calculate_bigfoot
#' @rdname calculate_bigfoot
#' 
#' @export
calculate_bigfoot <- function(X, 
                              what='all', how='all',
                              focalRadius=0,
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
                              template=NULL,
                              tileSize=c(500, 500),
                              parallel=TRUE,
                              nCores=max(1, parallel::detectCores()-1),
                              outputPath=tempdir(),
                              outputTag=NULL,
                              tries=100,
                              restart=NULL,
                              verbose=TRUE) UseMethod("calculate_bigfoot")


#' @name calculate_bigfoot
#' @export
calculate_bigfoot.sf <- function(X, 
                                 what='all', how='all',
                                 focalRadius=0,
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
                                 template=NULL,
                                 tileSize=c(500, 500),
                                 parallel=TRUE,
                                 nCores=max(1, parallel::detectCores()-1),
                                 outputPath=tempdir(),
                                 outputTag=NULL,
                                 tries=100,
                                 restart=NULL,
                                 verbose=TRUE){

  if(any(!sf::st_geometry_type(X) %in% c("POLYGON", "MULTIPOLYGON") )){
    message("Footprint statistics require polygon shapes.")
    stop()
  }
  
  result <- calc_fs_px_internal(X, what, how,
                                focalRadius, filter,
                                controlZone, controlUnits, controlDist,
                                template, tileSize,
                                parallel, nCores,
                                outputPath, outputTag,
                                tries, restart, verbose)
  
  invisible(result)
}


#' @name calculate_bigfoot
#' @export
calculate_bigfoot.sp <- function(X, 
                                 what='all', how='all',
                                 focalRadius=0,
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
                                 template=NULL,
                                 tileSize=c(500, 500),
                                 parallel=TRUE,
                                 nCores=max(1, parallel::detectCores()-1),
                                 outputPath=tempdir(),
                                 outputTag=NULL,
                                 tries=100,
                                 restart=NULL,
                                 verbose=TRUE){
  
  # convert to sf
  X <- sf::st_as_sf(X)
  
  if(any(!sf::st_geometry_type(X) %in% c("POLYGON", "MULTIPOLYGON") )){
    message("Footprint statistics require polygon shapes.")
    stop()
  }
  
  result <- calc_fs_px_internal(X, what, how,
                                focalRadius, filter,
                                controlZone, controlUnits, controlDist,
                                template, tileSize,
                                parallel, nCores,
                                outputPath, outputTag,
                                tries, restart, verbose)
  
  invisible(result)
}


#' @name calculate_bigfoot
#' @export
calculate_bigfoot.character <- function(X, 
                                        what='all', how='all',
                                        focalRadius=0,
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
                                        template=NULL,
                                        tileSize=c(500, 500),
                                        parallel=TRUE,
                                        nCores=max(1, parallel::detectCores()-1),
                                        outputPath=tempdir(),
                                        outputTag=NULL,
                                        tries=100,
                                        restart=NULL,
                                        verbose=TRUE){
        
  result <- calc_fs_px_internal(X, what, how,
                                focalRadius, filter,
                                controlZone, controlUnits, controlDist,
                                template, tileSize,
                                parallel, nCores,
                                outputPath, outputTag,
                                tries, restart, verbose)
  
  invisible(result)
}


# internal function for managing processes
calc_fs_px_internal <- function(X, what, how,
                                focalRadius, filter,
                                controlZone, controlUnits, controlDist,
                                template, tileSize,
                                parallel, nCores,
                                outputPath, outputTag,
                                tries, restart, verbose){

  if(is.null(template)){
    if(verbose) cat("Creating template grid \n")
    template <- stars::st_as_stars(sf::st_bbox(X), )
  } else if(is.character(template)){
    if(verbose) cat("Reading template grid \n")
    template <- stars::read_stars(template, proxy=TRUE)
  } else if(!inherits(template, "stars")){
    if(verbose) cat("Reading template grid \n")
    template <- stars::st_as_stars(template)
  }
  
  # check crs
  if(sf::st_crs(X) != sf::st_crs(template)){
    stop("Mismatched projection information between building footprints and template grid.", 
         call. = F)
  }
  
  if(!is.null(outputTag)){
    outputTag <- paste0(outputTag, "_")
  } else{
    outputTag <- ""
  }

  # get full list of metrics
  if(is.null(how)) stop("Please provide a summary function name.")
  if(verbose){ cat("Selecting metrics \n") }
  if("all" %in% what){ 
    argsDF <- list_fs(what="all", how=how) 
  } else if("nodist" %in% what){ 
    argsDF <- list_fs(what="nodist", how=how) 
  } else{
    argsX <- crossargs(what, how)
    argsDF <- do.call(rbind, argsX)
    argsDF <- unique(argsDF)
  } 
  
  # set defaults for controls
  if(verbose){ cat("Setting control values \n") }
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
  
  if(verbose){ cat("Creating output grids \n") }
  # create empty output grids to match template
  outTemplate <- stars::st_as_stars(matrix(NA_real_, 
                                           nrow=nrow(template), 
                                           ncol=ncol(template)), 
                                    dimensions=stars::st_dimensions(template))
  
  allOutPath <- vector("character", length=length(nrow(argsDF)))
  for(i in 1:nrow(argsDF)){
    params <- unlist(argsDF[i, "cols"])
    calc_func <- unlist(argsDF[i, "funs"])
    mTag <- paste(paste(params, collapse="_"), calc_func, sep="_")
    if(focalRadius > 0){
      mTag <- paste(mTag, focalRadius, sep="_")
    }
    
    outName <- file.path(outputPath, 
                         paste0(outputTag, mTag, ".tif"))
    if(is.null(restart)){
      tmp <- stars::write_stars(outTemplate, 
                                outName) # default is float32
      rm(tmp)
    }
    allOutPath[[i]] <- outName
  }
  rm(outTemplate)
  # print(allOutPath)

  # tiles for processing
  if(verbose){ cat("Creating list of processing tiles \n") }
  tiles <- gridTiles(template, px=tileSize)
  
  if(verbose){ 
    file.create(tile_log <- file.path(outputPath, "tile.log"))
  }
  
  if(focalRadius > 0){
    # convert focal radius to pixels for extraction (approximation)
    # assuming pixels are equal dimensions x,y
    if(sf::st_is_longlat(template)){ # approximate meters
      pxM <- 111111*abs(stars::st_dimensions(template)[[1]]$delta)
    } else{ # projected CRS
      pxM <- abs(stars::st_dimensions(template)[[1]]$delta)
    }
    pxLap <- ceiling(focalRadius / pxM)
    tilesBuff <- gridTiles(template, px=tileSize, overlap=pxLap)    
  } else{
    tilesBuff <- tiles
  }
  
  if(!is.null(restart)){
    if(verbose){ cat("Restarting tile processing \n") }
    if(length(restart) == 1){
      tiles <- tiles[tiles$tid >= restart, ]
      tilesBuff <- tilesBuff[tilesBuff$tid >= restart, ]
    }
    
    if(length(restart) > 1){
      tiles <- tiles[tiles$tid %in% restart, ]
      tilesBuff <- tilesBuff[tilesBuff$tid %in% restart, ]
    }
  }
  
  # processing loop
  if(parallel){
    # create cluster
    if(verbose){ cat("Setting up cluster...\n")}
    if(.Platform$OS.type == "unix"){
      cl <- parallel::makeCluster(spec=nCores, type="FORK")
    } else{
      cl <- parallel::makeCluster(spec=nCores, type="PSOCK")
      parallel::clusterExport(cl, 
                              varlist=c("X",
                                        "template",
                                        "what","how",
                                        "controlZone",
                                        "controlUnits",
                                        "controlDist",
                                        "focalRadius",
                                        "filter",
                                        "allOutPath",
                                        "tries",
                                        "verbose",
                                        "tile_log"),
                              envir=environment())
    }
    doParallel::registerDoParallel(cl)
    parallel::clusterEvalQ(cl, {library(foot); library(stars); 
      library(sf); library(filelock)})
    
    if(verbose){ cat(paste0("Begin parallel tile processing: ", 
                            Sys.time(), "\n"))}
    
    foreach::foreach(js=iterators::isplit(tiles, rep(1:nCores, length=nrow(tiles))),
                     jBs=iterators::isplit(tilesBuff, rep(1:nCores, length=nrow(tilesBuff))),
                     .inorder=FALSE
    ) %dopar%{
      on.exit({ rm(list=ls()); gc() })
      
      jobs <- js$value
      jobsBuff <- jBs$value
      
      for(i in 1:nrow(jobs)){
        job <- jobs[i,]
        jobBuff <- jobsBuff[i,]
        
        mgTile <- stars::st_as_stars(template[,job$xl:job$xu, job$yl:job$yu])
        mgBuffTile <- stars::st_as_stars(template[,jobBuff$xl:jobBuff$xu, 
                                                  jobBuff$yl:jobBuff$yu])
        process_tile(mgTile, mgBuffTile, 
                     X, what, how, 
                     focalRadius, 
                     controlZone, controlUnits, constrolDist,
                     allOutPath,
                     tries,
                     filter,
                     verbose=FALSE) 
        # logging completed tiles
        if(verbose){
          lck <- filelock::lock(file.path(tempdir(), "tile.log.lock"))
          write(job$tid, file=tile_log, append=TRUE)
          filelock::unlock(lck)
        }
      }
      NULL
    }

    parallel::stopCluster(cl)
    
  } else{
    for(i in 1:nrow(tiles)){
      job <- tiles[i,]
      jobBuff <- tilesBuff[i,]
      if(verbose){
        cat(paste0("\nTile: ", i, " of ", nrow(tiles), "\n"))
      }
      # create sub-datasets from template mastergrid
      mgTile <- stars::st_as_stars(template[,job$xl:job$xu, job$yl:job$yu])
      mgBuffTile <- stars::st_as_stars(template[,jobBuff$xl:jobBuff$xu, 
                                                jobBuff$yl:jobBuff$yu])
      
      process_tile(mgTile, mgBuffTile, 
                   X, what, how, 
                   focalRadius, 
                   controlZone, controlUnits, constrolDist,
                   allOutPath,
                   tries,
                   filter,
                   verbose)
      
      if(verbose){
        write(job$tid, file=tile_log, append=TRUE)
      }
    } # end for loop on tiles
  }
  if(verbose){ cat(paste0("\nFinished processing all tiles: ", 
                          Sys.time(), "\n")) }
  # return(result)
  return(allOutPath)
}


# helper function for processing tiles
process_tile <- function(mgTile, mgBuffTile, 
                         X, what, how, 
                         focalRadius, 
                         controlZone, controlUnits, constrolDist,
                         allOutPath,
                         tries,
                         filter,
                         verbose=FALSE){
  
  # clean-up
  on.exit({ rm(list=ls()); gc() })
  # blank tile for the results
  naTile <- stars::st_as_stars(matrix(NA_real_, 
                                      nrow=nrow(mgTile), 
                                      ncol=ncol(mgTile)), 
                               dimensions=stars::st_dimensions(mgTile))
  
  # set-up a spatial filter for file reading (with Buffer)
  bbox <- sf::st_as_sfc(sf::st_bbox(mgBuffTile))
  # TO-DO: add crs transform to match building footprints
  
  if(verbose){ cat("Reading footprints \n") }
  if(inherits(X, "sf")){
    Xsub <- X[bbox,]
  } else{
    wkt <- sf::st_as_text(bbox)
    # read in the footprints
    Xsub <- sf::st_read(X,
                        wkt_filter=wkt, 
                        quiet=!verbose)
    if(verbose){ cat("\n") }
  }

  # remove empty geometries
  Xsub <- Xsub[!sf::st_is_empty(Xsub), , drop=F]
  # simplify
  if(any(sf::st_geometry_type(Xsub) %in% c("MULTIPOLYGON"))){
    suppressWarnings(Xsub <- sf::st_cast(Xsub, "POLYGON"))
  }
  
  # processing
  if(nrow(Xsub) > 0){
    # read proxy to grid and convert to polygon object
    if(verbose){ cat("Reading template grid \n") }
    mgPoly <- sf::st_as_sf(stars::st_as_stars(mgTile))
    # check for valid processing locations
    if(nrow(mgPoly) > 0){ # NA pixels not converted
      if(!controlZone$zoneName %in% colnames(mgPoly)){
        mgPoly[[controlZone$zoneName]] <- 1:nrow(mgPoly)
      }
      # buffer for focal statistics
      if(focalRadius > 0){
        if(verbose){ cat("Buffering processing sites \n") }
        if(sf::st_is_longlat(mgPoly)){
          # find UTM zone of the tile's centroid
          aoi <- sf::st_as_sfc(sf::st_bbox(mgPoly))
          suppressWarnings(zn <- suggestUTMzone(sf::st_coordinates(sf::st_centroid(aoi))))
          mgPolyArea <- sf::st_transform(mgPoly, crs=zn)
          # circular buffer around centre point
          suppressWarnings(mgPolyArea <- sf::st_buffer(sf::st_centroid(mgPolyArea), 
                                                       dist=focalRadius))
          mgPolyArea <- sf::st_transform(mgPolyArea, crs=sf::st_crs(mgPoly))
        } else{
          mgPolyArea <- sf::st_buffer(mgPoly, dist=focalRadius)
        }
      } else{ # pixel resolution
        mgPolyArea <- mgPoly
      }
      
      # footprint statistics within the tile
      tileResults <- calculate_footstats(Xsub,
                                         zone=mgPolyArea,
                                         what=what,
                                         how=how,
                                         controlZone=controlZone,
                                         controlUnits=controlUnits,
                                         controlDist=controlDist,
                                         filter=filter,
                                         verbose=verbose)
      # clean-up
      rm(Xsub)

      # check for errors in the return
      if(is.null(tileResults)){
        return(NULL)
      }
      # store results tile calculations
      mgPoly <- merge(mgPoly, 
                      tileResults, 
                      by=controlZone$zoneName)
      # output loop
      if(verbose){ cat("Writing output tiles \n") }
      for(n in names(tileResults)[!names(tileResults) %in% controlZone$zoneName]){
        units(mgPoly[[n]]) <- NULL
        
        tmpName <- paste0("tempRas_", Sys.getpid(),".tif")
        resArea <- stars::st_rasterize(mgPoly[n], 
                                       template=naTile,
                                       file=file.path(tempdir(), 
                                                      tmpName))
        # update tile offset to nest in template
        d <- stars::st_dimensions(resArea)
        tD <- stars::st_dimensions(mgTile)
        d[["x"]]$from <- tD[["x"]]$from  # job$xl
        d[["x"]]$to <- tD[["x"]]$to  # job$xu
        d[["y"]]$from <- tD[["y"]]$from  # job$yl
        d[["y"]]$to <- tD[["y"]]$to  # job$yu
        
        resArea <- structure(resArea, dimensions=d)
        
        # get file name
        n <- sub("fs_", "", n, fixed=T)
        # nsplit <- strsplit(n, "_", fixed=T)[[1]]
        # n <- ifelse(length(nsplit)==3, 
        #             paste(nsplit[1], nsplit[3], sep="_"), 
        #             paste(nsplit, collapse="_"))
        # if(focalRadius > 0){
        #   n <- paste(n, focalRadius, sep="_")
        # }
        
        path <- which(grepl(n, allOutPath, fixed=T))
        # print(allOutPath[[path]])
        if(length(path)==1){
          lck <- filelock::lock(file.path(tempdir(), paste0(path, ".lock")))
          write_tile(outGrid=resArea, 
                     outName=allOutPath[[path]], 
                     tries=tries, 
                     update=TRUE)
          filelock::unlock(lck)
        }
      } # end output loop
      if(verbose){ cat("Finished writing grids\n") }
    } # end found mastergrid tiles
  } # end if buildings found after filter
} # end if buildings found in tile


# helper function for writing tiles
# based on {spatial.tools}
write_tile <- function(outGrid, outName, tries=100, update=FALSE){
  writeSuccess <- FALSE
  tryCount <- 1
  tryThreshold <- tries

  while(!writeSuccess & (tryCount <= tryThreshold))
  {
    writeSuccess <- TRUE
    tryCatch(stars::write_stars(outGrid, outName, update=update),
    error=function(err) writeSuccess <<- FALSE)	
    
    tryCount <- tryCount + 1
    Sys.sleep(1) 
  }
  if(tryCount > tryThreshold) stop(paste0("Writing failed after ", 
                                          tries, " tries, exiting."))
}
