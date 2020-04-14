#' List footprint summary metrics
#'
#' @description List footprint metrics
#'
#'

# TO-DO: add options to look up by abbreviation/shortened names or groups of metrics

#' @export
list_fs <- function(){
  metrics <- c("fs_settled","fs_area_mean", "fs_area_total")
  
  return(metrics)
}

