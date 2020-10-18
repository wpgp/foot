#' List footprint summary metrics
#'
#' @description Helper function to list footprint characteristic names and
#'   built-in summary functions.
#' @param what Character vector of names of characteristics to look up (e.g.
#'   "area"). Alternatively, list \code{'all'} or all characteristics except
#'   nearest neighbour distances (\code{'nodist'}).
#' @param how Character vector of summary functions to look up (e.g. "mean") or
#'   \code{'all'} available metrics.
#' 
#' @details Provides an easy look-up for the built-in function names, and
#'   basic geometric characteristics used by \code{foot}. Supplying a "what"
#'   characteristics finds all built-in functions. Conversely, supplying a "how"
#'   function name returns are characteristics for which that summary is
#'   available. Arguments are optional
#'   and if both are omitted the full set of metrics will be returned.
#' @return Vector or \code{data.frame} with selected footprint metric function
#'   names and/or units.
#' 
#' @examples
#' # get the full list of all available 
#' list_fs()
#' 
#' # get all summary functions for "area"
#' list_fs(what="area")
#' 
#' # get all characteristics relevant for entropy
#' list_fs(how="entropy")
#' 
#' @rdname list_fs
#' @export
list_fs <- function(what='all', how='all'){
  baseWhats <- c("area","perimeter")
  baseFuns <- c("sum","mean","median","sd","min","max","cv")
  shapeWhats <- c("shape","compact")
  shapeFuns <- c("mean","median","sd","min","max","cv")
  settWhats <- c("settled")
  settFuns <- c("binary","count")
  angleWhats <- c("angle")
  angleFuns <- c("mean","median","sd","min","max","cv","entropy")
  distWhats <- c("nndist")
  distFuns <- c("mean","median","sd","min","max","cv","nnindex")
  
  allMetrics <- c(
    crossargs(baseWhats, baseFuns),
    crossargs(shapeWhats, shapeFuns),
    crossargs(settWhats, settFuns),
    crossargs(angleWhats, angleFuns),
    crossargs(distWhats, distFuns)
  )
  allMetrics <- do.call(rbind, allMetrics)
  
  if(!'all' %in% tolower(what)){
    if('nodist' %in% tolower(what)){
      allMetrics <- allMetrics[allMetrics$cols != 'nndist', ]
    } else{
      allMetrics <- allMetrics[allMetrics$cols %in% what, ]
    }
  }
  
  if(is.null(how)){
    allMetrics <- allMetrics$cols
  } else{
    if(!'all' %in% tolower(how)){
      allMetrics <- allMetrics[allMetrics$funs %in% how,]
    }
    
    if(nrow(allMetrics)==0){
      message("No built-in 'foot' metrics found.")
      return(NULL)
    }
    allMetrics <- allMetrics[order(allMetrics$cols, allMetrics$funs),]
    rownames(allMetrics) <- 1:nrow(allMetrics)
  }
  
  return(unique(allMetrics))
}

