#' calculate_footstats
#' 
#' @title calculate_footstats
#' @description Calculate selected metrics of building footprints
#' @param txt Text to append to "foot"
#' @return TBD.
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
                                file=NULL) UseMethod("calculate_footstats")

#' @name calculate_footstats
#' @export
calculate_footstats.sf <- function(X, index=NULL, metrics='all', 
                                   gridded=TRUE, template=NULL, file=NULL){
  if(any(!st_geometry_type(X) %in% c("POLYGON", "MULTIPOLYGON") )){
    message("Footprint statistics require polygon shapes.")
    stop()
  }
  
  result <- calc_fs_internal(X, index, metrics, gridded, template, file)
  
  return(result)
}


#' @name calculate_footstats
#' @export
calculate_footstats.sfc <- function(X, index=NULL, metrics='all', 
                                    gridded=TRUE, template=NULL, file=NULL){
  # cast to sf for consistency
  X <- sf::st_as_sf(X)
  
  result <- calculate_footstats(X, index=index, metrics=metrics, 
                                gridded=gridded, template=template, file=file)
  return(result)
}


#' @name calculate_footstats
#' @export
calculate_footstats.sp <- function(X, index=NULL, metrics='all', 
                                   gridded=TRUE, template=NULL, file=NULL){
  # convert to sf
  X <- sf::st_as_sf(X)
  
  result <- calculate_footstats(X, index=index, metrics=metrics, 
                                gridded=gridded, template=template, file=file)
  return(result)
}


#' @name calculate_footstats
#' @export
calculate_footstats.character <- function(X, index=NULL, metrics='all', 
                                          gridded=TRUE, template=NULL, file=NULL){
  # attempt to read in file
  X <- sf::st_read(X)
  
  result <- calculate_footstats(X, index=index, metrics=metrics, 
                                gridded=gridded, template=template, file=file)
  return(result)
}


#' @name calculate_footstats
#' @export
calculate_footstats.list <- function(X, index=NULL, metrics='all', 
                                     gridded=TRUE, template=NULL, file=NULL){
  result <- lapply(X, FUN=calculate_footstats(X, index=index, metrics=metrics, 
                                              gridded=gridded, template=template, file=file))
  
  return(result)  # should the list be simplified?
}


calc_fs_internal <- function(X, index, metrics, gridded, template, file){
  if(is.na(st_crs(X))){
    stop("Polygons must have a spatial reference.")
  }
  
  if(class(index) == "sf"){
    indexZones <- index # make copy
    X <- zonalIndex(X, index, returnObject=TRUE)
    index <- "zoneID"
  }
  
  if(toupper(metrics[1]) == 'ALL'){
    metrics <- foot::fs_footprint_metrics$name
  }
  
  if(toupper(metrics[1]) == "NODIST"){
    metrics <- foot::fs_footprint_metrics$name
    metrics <- metrcs[!grepl("NNdist", metrics)]
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
  
  if(any(grepl("NNindex", metrics, fixed=T))){
    metrics <- c(metrics, "fs_NNdist_mean", "fs_count")
    metrics <- metrics[!grepl("NNindex", metrics)]
    nnIndex <- TRUE
    
    if(exists("indexZones")){
      zonalArea <- data.table(index=X[["zoneID"]], zoneArea=fs_area(indexZones))
    } else{
      warnings("Nearest neighbour index requires zonal areas.")
      # nnIndex <- FALSE
      zoneAreas <- fs_area(sf::st_as_sfc(sf::st_bbox(X), crs=sf::st_crs(X)))
      zonalArea <- data.table(zoneID=1:length(zoneAreas), zoneArea=zoneAreas)
    }
    
  } else{
    nnIndex <- FALSE
  }
  
  if(any(grepl("angle", metrics, fixed=T))){
    normalize <- TRUE
  }
  
  # pre-calcluate unit geometry measures
  if(any(grepl("area", metrics, fixed=T))){
    unit <- "ha"
    X[["fs_area"]] <- fs_area(X, unit)
  }
  
  if(any(grepl("perim", metrics, fixed=T))){
    X[["fs_perim"]] <- fs_perimeter(X, 
                                    unit=fs_footprint_metrics[fs_footprint_metrics$name=="fs_perim_mean",
                                                              "default_units"])
  }
  
  if(any(grepl("NNdist", metrics, fixed=T))){
    X[["fs_NNdist"]] <- fs_NNdist(X, 
                                  unit=fs_footprint_metrics[fs_footprint_metrics$name=="fs_NNdist_mean",
                                                            "default_units"])
  }
  
  # creating the names of the functions to call
  metrics <- unique(metrics)
  metrics_calc <- paste0(metrics, "_calc")
  
  result <- lapply(seq_along(metrics_calc), function(current_metric){
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
  
  # output
  if(gridded==TRUE){
    if(is.null(file)){
      file <- tempdir()
    }
    
    if(is.null(template)){
      template <- stars::st_as_stars(sf::st_bbox(X), values=NA_real_)  # default resolution
    } 
    
    if(sf::st_crs(template) != sf::st_crs(X)){
      stop("CRS for buildings and template raster not matching.")
    }
    
    for(r in result){
      
    }
  }
  
  return(merged_result)
}

