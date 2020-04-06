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
    metrics <- list_fs()
  }
  
  if(grepl("area", metrics, fixed=T)){
    unit <- "ha"
  }
  
  # creating the names of the functions to call
  metrics_calc <- paste0(metrics, "_calc")
  
  result <- lapply(seq_along(metrics_calc), function(current_metric){
    func <- get(metrics_calc[[current_metric]], mode="function")
    arguments <- names(formals(func))
    
    tryCatch(do.call(what=func,
                     args=mget(arguments, envir=parent.env(environment()))
                     ),
             error = function(e){
               message("")
               stop(e)
               }
             )  
    })
  
  return(result)
}

