#' List footprint summary metrics
#'
#' @description Helper functions to list footprint metric names and units
#' @param short_names Character vector of short names to lookup \code{fs_}
#'   function names.
#' @param group Character vector to return a group of metrics (e.g. "area").
#' @param col_names Character vector to select columns from the table of
#'   metrics.
#' @param metrics Character vector of \code{fs_} function names to look up the
#'   default units.
#' 
#' @details These convenience function access the internal data in
#'   \code{foot::fs_footprint_metrics}. They provide easy look-up access for the
#'   function names, the default units of measurement, or to return columns.
#'   Arguments are optional and if omitted the full set of metrics will be
#'   returned.
#' @return Vector or data.frame with selected footprint metric function names
#'   and/or units.
#' @rdname get_fs_metrics
#' @export
get_fs_metrics <- function(short_names, group=NULL){
  if(missing(short_names) & is.null(group)){
    return(list_fs("name"))
  }
  
  which_rows <- NULL
  if(!missing(short_names)){
    metrics <- tolower(short_names)
    # clean back to short names
    metrics <- gsub("fs_", "", metrics, fixed=T)
    metrics <- gsub("_calc", "", metrics, fixed=T)
    
    if("all" %in% metrics) {
      return(list_fs("name"))
    }
    
    if("nodist" %in% metrics){
      rows <- list_fs(c("name", "group"))
      return(rows[rows$group != "dist", "name"])
    }
    
    allmetrics <- list_fs(c("name", "short_name", "group"))
    which_rows <- allmetrics[which(allmetrics$short_name %in% metrics), "name"]
  }
  
  if(!is.null(group)){
    group <- tolower(group)
    which_rows <- c(which_rows,
                    allmetrics[which(allmetrics$group %in% group), "name"])
  }
  
  which_rows <- unique(which_rows)
  if(length(which_rows) == 0){
    stop(paste0("Invalid metric names: ", metrics))
  } else{
    return(which_rows)
  }
}


#' @rdname get_fs_metrics
#' @export
list_fs <- function(col_names=NULL){
  if(is.null(col_names)){
    return(foot::fs_footprint_metrics)
  } else{
    if(!all(col_names %in% names(fs_footprint_metrics))){
      stop("Invalid column names.")
    } else{
      fs_footprint_metrics[, col_names]  
    }
  }
}


#' @rdname get_fs_metrics
#' @export
get_fs_units <- function(metrics=NULL){
  if(is.null(metrics)){
    return(list_fs("default_units"))
  } else{
    allunits <- list_fs(c("name","default_units"))
    return(allunits[which(allunits$name %in% metrics), "default_units"])
  }
}


#' @rdname get_fs_metrics
#' @export
get_fs_group <- function(metrics=NULL){
  if(is.null(metrics)){
    return(list_fs("group"))
  } else{
    allunits <- list_fs(c("name","group"))
    return(allunits[which(allunits$name %in% metrics), "group"])
  }
}



