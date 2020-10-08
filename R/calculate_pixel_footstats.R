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
#' @param metrics character vector. Names of footprint statistics in the form of
#'   "fs_area_mean", etc. Other options include \code{ALL} or \code{NODIST} to
#'   calculate all available metrics and all except nearest neighbour distances,
#'   respectively.
#' @param focalRadius numeric. Distance in meters for a buffer around each
#'   template pixel. Creates a focal processing window for metrics.
#' @param minArea numeric. Minimum footprint area to filter \code{X}.
#' @param maxArea numeric. Maximum footprint area to filter \code{X}.
#' @param controlUnits (Optional) named list to control the units used in the
#'   geometry functions. Elements can include \code{areaUnit}, \code{perimUnit},
#'   and \code{distUnit}. The values for these items should be strings that can
#'   be coerced into a \code{units} object.
#' @param controlDist (Optional) named list to override default options for
#'   distance calculations. Elements can include \code{maxSearch} and
#'   \code{method}. Ignored if \code{metrics} does not include a distance
#'   calculation. See \code{\link[foot]{fs_nndist}}.
#' @param zoneMethod One of \code{'centroid', 'intersect', 'clip'}. How should
#'   footprints which span pixel zones be allocated? Default is by its
#'   \code{'centroid'}. See \code{\link[foot]{zonalIndex}} for details.
#' @param template (optional). When creating a gridded output, a supplied
#'   \code{stars} or \code{raster} dataset to align the data.
#' @param parallel logical. Should a parallel backend be used to process the
#'   tiles.
#' @param nCores number of CPU cores to use if \code{parallel} is \code{TRUE}.
#'   Default is 1 less than the available CPUs.
#' @param tileSize number of pixels per side of a tile. Can be a vector of
#'   length 2 (rows, column pixels). Ignored if n provided. Default is 1000.
#' @param outputPath (optional). When creating a gridded output, a path for the
#'   location of the output. Default is the temp directory.
#' @param outputTag (optional). A character string that will be added to the
#'   beginning of the output name for the gridded files.
#' @param tries (optional). The number of attempts to write a tile to the output
#'   file. Default is 100.
#' @param verbose logical. Should progress messages be printed. Default
#'   \code{False}.
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
#'                   metrics=c("shape_mean",
#'                             "count",
#'                             "perim_total"),  
#'                   controlUnits=list(areaUnit="m^2"),
#'                   minArea=50,  # footprints must be larger than 50 m^2
#'                   maxArea=1000,  # footprints must be smaller than 1000 m^2
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
                              metrics='all',
                              focalRadius=0,
                              minArea=NULL,
                              maxArea=NULL,
                              controlUnits=NULL,
                              controlDist=NULL,
                              zoneMethod='centroid',
                              template=NULL,
                              tileSize=c(500, 500),
                              parallel=TRUE,
                              nCores=max(1, parallel::detectCores()-1),
                              outputPath=tempdir(),
                              outputTag=NULL,
                              tries=100,
                              verbose=FALSE) UseMethod("calculate_bigfoot")


#' @name calculate_bigfoot
#' @export
calculate_bigfoot.sf <- function(X, 
                                 metrics='all',
                                 focalRadius=0,
                                 minArea=NULL,
                                 maxArea=NULL,
                                 controlUnits=NULL,
                                 controlDist=NULL,
                                 zoneMethod='centroid',
                                 template=NULL,
                                 tileSize=c(500, 500),
                                 parallel=TRUE,
                                 nCores=max(1, parallel::detectCores()-1),
                                 outputPath=tempdir(),
                                 outputTag=NULL,
                                 tries=100,
                                 verbose=FALSE){

  if(any(!sf::st_geometry_type(X) %in% c("POLYGON", "MULTIPOLYGON") )){
    message("Footprint statistics require polygon shapes.")
    stop()
  }
  
  result <- calc_fs_px_internal(X, metrics, focalRadius, 
                                minArea, maxArea, 
                                controlUnits, controlDist, zoneMethod,
                                template, tileSize, parallel, nCores,
                                outputPath, outputTag, tries, verbose)
  
  invisible(result)
}


#' @name calculate_bigfoot
#' @export
calculate_bigfoot.sp <- function(X, 
                                 metrics='all',
                                 focalRadius=0,
                                 minArea=NULL,
                                 maxArea=NULL,
                                 controlUnits=NULL,
                                 controlDist=NULL,
                                 zoneMethod='centroid',
                                 template=NULL,
                                 tileSize=c(500, 500),
                                 parallel=TRUE,
                                 nCores=max(1, parallel::detectCores()-1),
                                 outputPath=tempdir(),
                                 outputTag=NULL,
                                 tries=100,
                                 verbose=FALSE){
  
  # convert to sf
  X <- sf::st_as_sf(X)
  
  if(any(!sf::st_geometry_type(X) %in% c("POLYGON", "MULTIPOLYGON") )){
    message("Footprint statistics require polygon shapes.")
    stop()
  }
  
  result <- calc_fs_px_internal(X, metrics, focalRadius, 
                                minArea, maxArea, 
                                controlUnits, controlDist, zoneMethod,
                                template, tileSize, parallel, nCores,
                                outputPath, outputTag, tries, verbose)
  
  invisible(result)
}


#' @name calculate_bigfoot
#' @export
calculate_bigfoot.character <- function(X, 
                                        metrics='all',
                                        focalRadius=0,
                                        minArea=NULL,
                                        maxArea=NULL,
                                        controlUnits=NULL,
                                        controlDist=NULL,
                                        zoneMethod='centroid',
                                        template=NULL,
                                        tileSize=c(500, 500),
                                        parallel=TRUE,
                                        nCores=max(1, parallel::detectCores()-1),
                                        outputPath=tempdir(),
                                        outputTag=NULL,
                                        tries=100,
                                        verbose=FALSE){
        
  result <- calc_fs_px_internal(X, metrics, focalRadius, 
                                minArea, maxArea, 
                                controlUnits, controlDist, zoneMethod,
                                template, tileSize, parallel, nCores,
                                outputPath, outputTag, tries, verbose)
  
  invisible(result)
}


# internal function for managing processes
calc_fs_px_internal <- function(X, 
                                metrics,
                                focalRadius,
                                minArea,
                                maxArea,
                                controlUnits,
                                controlDist,
                                zoneMethod,
                                template,
                                tileSize,
                                parallel,
                                nCores,
                                outputPath,
                                outputTag,
                                tries,
                                verbose){

  if(is.null(template)){
    stop("Template raster or grid required.")
  } else if(is.character(template)){
    template <- stars::read_stars(template, proxy=TRUE)
  } else if(!inherits(template, "stars")){
    template <- stars::st_as_stars(template)
  }
  
  if(!is.null(outputTag)){
    outputTag <- paste0(outputTag, "_")
  } else{
    outputTag <- ""
  }
  
  if(!zoneMethod %in% c("centroid","intersect","clip")){
    stop("Zone method must be one of 'centroid', 'intersect', or 'clip'")
  }
  
  # get full list of metrics
  metrics <- get_fs_metrics(short_names=metrics, group=metrics)
  
  # get full set of unit controls - sort alphabetically to make matches
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
  
  # create empty output grids to match template
  outTemplate <- stars::st_as_stars(matrix(NA, 
                                           nrow=nrow(template), 
                                           ncol=ncol(template)), 
                                    dimensions=stars::st_dimensions(template))
  
  allOutPath <- vector("character", length=length(metrics))
  for(i in seq_along(metrics)){
    m <- metrics[[i]]
    mTag <- sub("fs_", "", m, fixed=T) # drop function label
    if(focalRadius > 0){
      mTag <- paste(mTag, focalRadius, sep="_")
    }
    
    outName <- file.path(outputPath, 
                         paste0(outputTag, mTag, ".tif"))
    stars::write_stars(outTemplate, 
                       outName) # default is float32
    allOutPath[[i]] <- outName
  }
  # print(allOutPath)

  # tiles for processing
  tiles <- gridTiles(template, px=tileSize)
  
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
  
  # processing loop
  if(parallel){
    # create cluster
    if(verbose){ cat("Setting up cluster\n")}
    if(.Platform$OS.type == "unix"){
      cl <- parallel::makeCluster(spec=nCores, type="FORK")
    } else{
      cl <- parallel::makeCluster(spec=nCores, type="PSOCK")
      parallel::clusterExport(cl, 
                              varlist=c("X",
                                        "template",
                                        "metrics",
                                        "controlUnits",
                                        "controlDist",
                                        "focalRadius",
                                        "minArea",
                                        "maxArea",
                                        "zoneMethod",
                                        "allOutPath",
                                        "tries"),
                              envir=environment())
    }
    doParallel::registerDoParallel(cl)
    parallel::clusterEvalQ(cl, {library(foot); library(stars); 
      library(sf); library(filelock)})
    
    if(verbose){ cat(paste0("Begin parallel tile processing: ", 
                            Sys.time(), "\n"))}

    foreach::foreach(job=iterators::iter(tiles, by="row"),
                     jobBuff=iterators::iter(tilesBuff, by="row"),
                     .inorder=FALSE,
                     .export="process_tile"
                     ) %dopar% {
      mgTile <- stars::st_as_stars(template[,job$xl:job$xu, job$yl:job$yu])
      mgBuffTile <- stars::st_as_stars(template[,jobBuff$xl:jobBuff$xu, 
                                                jobBuff$yl:jobBuff$yu])
      process_tile(mgTile, mgBuffTile, 
                   X, metrics, 
                   focalRadius, minArea, maxArea,
                   controlUnits, controlDist,
                   zoneMethod,
                   allOutPath,
                   tries,
                   verbose=FALSE) 
      
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
                   X, metrics, 
                   focalRadius, 
                   minArea, maxArea,
                   controlUnits,
                   controlDist,
                   zoneMethod,
                   allOutPath, 
                   tries,
                   verbose)
    } # end for loop on tiles
  }
  if(verbose){ cat(paste0("\nFinished processing all tiles: ", 
                          Sys.time(), "\n")) }
  # return(result)
  return(allOutPath)
}


# helper function for processing tiles
process_tile <- function(mgTile, mgBuffTile, 
                         X, metrics, 
                         focalRadius, 
                         minArea, maxArea, 
                         controlUnits, constrolDist, zoneMethod,
                         allOutPath,
                         tries,
                         verbose=FALSE){
  
  # blank tile for the results
  naTile <- stars::st_as_stars(matrix(NA, 
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
    Xsub <- sf::st_cast(Xsub, "POLYGON")
  }
  
  # check for records
  if(nrow(Xsub) > 0){
    # if no clipping, speed up processing to avoid duplicated calculations after
    # zone index which can duplicate features.
    if(zoneMethod %in% c("centroid","intersects")){ 
      # pre-calculate unit geometry measures
      if(any(grepl("area", metrics, fixed=T)) |
         any(grepl("compact", metrics, fixed=T)) |
         !is.null(minArea) | !is.null(maxArea)){
        if(!"fs_area" %in% names(Xsub)){
          if(verbose){ cat("Pre-calculating footprint areas \n") }
          Xsub[["fs_area"]] <- fs_area(Xsub, unit=controlUnits$areaUnit)
        }
      }
  
      if(any(grepl("perim", metrics, fixed=T)) |
         any(grepl("compact", metrics, fixed=T))){
        if(!"fs_perim" %in% names(Xsub)){
          if(verbose){ cat("Pre-calculating footprint perimeters \n")}
          Xsub[["fs_perim"]] <- fs_perimeter(Xsub, unit=controlUnits$perimUnit)
        }
      }
  
      if(any(grepl("nndist", metrics, fixed=T))){
        if(!"fs_nndist" %in% names(Xsub)){
          if(verbose){ cat("Pre-calculating nearest neighbour distances \n") }
          Xsub[["fs_nndist"]] <- fs_nndist(Xsub, 
                                           maxSearch=controlDist$maxSearch, 
                                           method=controlDist$method, 
                                           unit=controlUnits$distUnit)
        }
      }
  
      if(any(grepl("angle", metrics, fixed=T))){
        if(!"fs_angle" %in% names(Xsub)){
          if(verbose){ cat("Pre-calculating angles \n") }
          Xsub[["fs_angle"]] <- fs_mbr(Xsub)
        }
      }
  
      # filter records
      if(!is.null(minArea)){
        if(verbose) { cat(paste0("Filtering features larger than ", minArea," \n")) }
        Xsub <- subset(Xsub, fs_area > units::as_units(minArea,
                                                       controlUnits$areaUnit))
      }
      if(!is.null(maxArea)){
        if(verbose) { cat(paste0("Filtering features smaller than ", maxArea," \n")) }
        Xsub <- subset(Xsub, fs_area < units::as_units(maxArea,
                                                       controlUnits$areaUnit))
      }
      # no need to re-filter if not clipping
      minArea <- maxArea <- NULL
    }
    
    if(nrow(Xsub) > 0){
      # read proxy to grid and convert to polygon object
      if(verbose){ cat("Reading template grid \n") }
      mgPoly <- sf::st_as_sf(stars::st_as_stars(mgTile))
      # check for valid processing locations
      if(nrow(mgPoly) > 0){ # NA pixels not converted
        mgPoly$id <- 1:nrow(mgPoly)
        # buffer for focal statistics
        if(focalRadius > 0){
          if(verbose){ cat("Buffering processing sites \n") }
          if(sf::st_is_longlat(mgPoly)){
            # find UTM zone of the tile's centroid
            aoi <- sf::st_as_sfc(sf::st_bbox(mgPoly))
            zn <- suggestUTMzone(sf::st_coordinates(sf::st_centroid(aoi)))
            mgPolyArea <- sf::st_transform(mgPoly, crs=zn)
            # circular buffer around centre point
            mgPolyArea <- sf::st_buffer(sf::st_centroid(mgPolyArea), 
                                        dist=focalRadius)
            mgPolyArea <- sf::st_transform(mgPolyArea, crs=sf::st_crs(mgPoly))
          } else{
            mgPolyArea <- sf::st_buffer(mgPoly, dist=focalRadius)
          }
        } else{ # pixel resolution
          mgPolyArea <- mgPoly
        }
        
        # footprint statistics within the tile
        tileResults <- calculate_footstats(Xsub,
                                           index=mgPolyArea,
                                           metrics=metrics,
                                           minArea=minArea,
                                           maxArea=maxArea,
                                           controlUnits=controlUnits,
                                           controlDist=controlDist,
                                           zoneMethod=zoneMethod,
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
                        by.x="id", by.y="index")
        # output loop
        if(verbose){ cat("Writing output tiles \n") }
        for(n in names(tileResults)[!names(tileResults) %in% "index"]){
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
          nsplit <- strsplit(n, "_", fixed=T)[[1]]
          n <- ifelse(length(nsplit)==3, 
                      paste(nsplit[1], nsplit[3], sep="_"), 
                      paste(nsplit, collapse="_"))
          if(focalRadius > 0){
            n <- paste(n, focalRadius, sep="_")
          }
          
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
}


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
  if(tryCount > tryThreshold) stop(paste0("Writing failed after ", tries, " tries, exiting."))
}
