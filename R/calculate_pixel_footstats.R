#' calculate_bigfoot
#' 
#' @title calculate_bigfoot
#' @description Calculate selected metrics of building footprints for
#'   high-spatial resolution gridded outputs.
#' @param txt Text to append to "foot"
#' @return TBD.
#' 
#' @aliases calculate_bigfoot
#' @rdname calculate_bigfoot
#' 
#' @export
calculate_bigfoot <- function(X, 
                              metrics='all',
                              gridded=TRUE, 
                              template=NULL,
                              parallel=TRUE,
                              tileSize=c(1000, 1000),
                              overlap=0,
                              file=NULL) UseMethod("calculate_bigfoot")


#' @name calculate_bigfoot
#' @export
calculate_bigfoot.sf <- function(X, 
                                 metrics='all', 
                                 gridded=TRUE, template=NULL, 
                                 parallel=TRUE,
                                 tileSize=c(1000, 1000),
                                 overlap=0,
                                 file=NULL){

  if(any(!st_geometry_type(X) %in% c("POLYGON", "MULTIPOLYGON") )){
    message("Footprint statistics require polygon shapes.")
    stop()
  }
  
  result <- calc_fs_px_internal(X, metrics, gridded, 
                                template, parallel, tileSize, overlap, 
                                file)
  
  return(result)
}


#' @name calculate_bigfoot
#' @export
calculate_bigfoot.character <- function(X, 
                                        metrics='all', 
                                        gridded=TRUE, template=NULL, 
                                        parallel=TRUE,
                                        tileSize=c(1000, 1000),
                                        overlap=0,
                                        file=NULL){
  
}


calc_fs_px_internal <- function(X, metrics, gridded, 
                                template, parallel, tileSize, overlap, 
                                file){
  if(parallel){
    if(is.null(template)){
      stop("Template raster or grid required.")
    } else if(is.character(template)){
      template <- stars::read_stars(template, proxy=TRUE)
    } else{
      template <- stars::st_as_stars(template)
    }
    
    tiles <- gridTiles(template, px=tileSize)    
  } else{
    
  }
  
  return(result)
}

