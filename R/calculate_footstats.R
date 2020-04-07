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
  
  result <- calc_fs_internal(X, index, metrics)
  
  return(result)
}


#' @name calculate_footstats
#' @export
calculate_footstats.sp <- function(X, index=NULL, metrics='all', gridded=TRUE, template=NULL, file=NULL){
  # attempt to read in file
  X <- sf::st_read(X)
  # convert to sf
  X <- sf::st_as_sf(X)
  
  result <- calculate_footstats(X, index=index, metrics=metrics)
  return(result)
}


#' @name calculate_footstats
#' @export
calculate_footstats.character <- function(X, index=NULL, metrics='all', gridded=TRUE, template=NULL, file=NULL){
  # attempt to read in file
  X <- sf::st_read(X)
  
  result <- calculate_footstats(X, index=index, metrics=metrics)
  return(result)
}


#' @name calculate_footstats
#' @export
calculate_footstats.list <- function(X, index=NULL, metrics='all', gridded=TRUE, template=NULL, file=NULL){
  result <- lapply(X, FUN=calculate_footstats(X, index=index, metrics=metrics))
  
  return(result)  # should the list be simplified?
}


calc_fs_internal <- function(X, index, metrics, gridded, template, file){
  
  if(metrics=='all'){
    metrics <- foot::fs_footprint_metrics$name
  }
  
  if(any(grepl("area", metrics, fixed=T))){
    unit <- "ha"
  }
  
  # creating the names of the functions to call
  metrics_calc <- paste0(metrics, "_calc")
  
  result <- lapply(seq_along(metrics_calc), function(current_metric){
    func <- get(metrics_calc[[current_metric]], mode="function")
    arguments <- names(formals(func))

    if(grepl("area", current_metric)){  # set default unit values
      unit <- "ha"
    }
    if(grepl("perim",current_metric)){
      unit <- "m"
    }
    
    tryCatch(do.call(what=func,
                     args=mget(arguments, envir=parent.env(environment()))
                     ),
             error = function(e){
               message("")
               stop(e)
               }
             )  
    })
  
  # output
  if(gridded==TRUE){
    if(is.null(file)){
      file <- tempdir()
    }
    
    if(is.null(template)){
      template <- starts::st_as_stars(sf::st_bbox(X), values=NA_real_)  # default resolution
      
    } else{
      tempProj <- sf::st_crs(template)$epsg
      xProj <- sf::st_crs(X)$epsg
      
    }
    
    for(r in result){
      
    }
    
  } 
  # merge all
  merged_result <- Reduce(function(...) merge(...), result)
  return(merged_result)
}

