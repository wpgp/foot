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
calculate_footstats.sf <- function(X, index=NULL, metrics='all', gridded=TRUE, template=NULL, file=NULL){
  if(any(!st_geometry_type(X) %in% c("POLYGON", "MULTIPOLYGON") )){
    message("Footprint statistics require polygon shapes.")
    stop()
  }
  
  result <- calc_fs_internal(X, index, metrics, gridded, template, file)
  
  return(result)
}


#' @name calculate_footstats
#' @export
calculate_footstats.sp <- function(X, index=NULL, metrics='all', gridded=TRUE, template=NULL, file=NULL){
  # attempt to read in file
  X <- sf::st_read(X)
  # convert to sf
  X <- sf::st_as_sf(X)
  
  result <- calculate_footstats(X, index=index, metrics=metrics, gridded=gridded, template=template, file=file)
  return(result)
}


#' @name calculate_footstats
#' @export
calculate_footstats.character <- function(X, index=NULL, metrics='all', gridded=TRUE, template=NULL, file=NULL){
  # attempt to read in file
  X <- sf::st_read(X)
  
  result <- calculate_footstats(X, index=index, metrics=metrics, gridded=gridded, template=template, file=file)
  return(result)
}


#' @name calculate_footstats
#' @export
calculate_footstats.list <- function(X, index=NULL, metrics='all', gridded=TRUE, template=NULL, file=NULL){
  result <- lapply(X, FUN=calculate_footstats(X, index=index, metrics=metrics, gridded=gridded, template=template, file=file))
  
  return(result)  # should the list be simplified?
}


calc_fs_internal <- function(X, index, metrics, gridded, template, file){
  if(is.na(st_crs(X))){
    stop("Polygons must have a spatial reference.")
  }
  
  if(metrics[1]=='all'){
    metrics <- foot::fs_footprint_metrics$name
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
  
  if(any(grepl("angle", metrics, fixed=T))){
    normalize <- TRUE
  }
  
  # pre-calcluate unit geometry measures
  if(any(grepl("area", metrics, fixed=T))){
    unit <- "ha"
    X[["fs_area"]] <- fs_area(X, unit)
  }
  
  if(any(grepl("perim", metrics, fixed=T))){
    
    X[["fs_perim"]] <- fs_perimeter(X, unit="m")
  }
  
  # creating the names of the functions to call
  metrics_calc <- paste0(unique(metrics), "_calc")
  # print(metrics_calc)
  #units_calc <- foot::fs_footprint_metrics[foot::fs_footprint_metrics$name %in% metrics, "unit"]
  
  result <- lapply(seq_along(metrics_calc), function(current_metric){
    func <- get(metrics_calc[[current_metric]], mode="function")
    # print(metrics_calc[[current_metric]])
    arguments <- names(formals(func))
    # print(mget(arguments, envir=parent.env(environment())))

    tryCatch(do.call(what=func,
                     args=mget(arguments, envir=parent.env(environment()))
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

