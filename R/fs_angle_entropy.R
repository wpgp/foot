#' Building angle calculation
#' 
#' @description Calculate the entropy of rotation angles for building footprint
#' polygons within zones.
#' 
#' @inheritParams fs_area_mean
#' @param normalize A logical value indicating whether to normalize the entropy. 
#' Default is \code{TRUE}.
#' 
#' @details This measure uses the angle of the minimum rotated rectangle
#'   enclosing each footprint polygon. Entropy is an information criteria
#'   measure. When summarising the angles of footprints, higher entropy values
#'   may suggest less formally planned or zoned areas. The entropy calculation
#'   uses the common Shannon's Entropy. The normalization step produces an
#'   indicator for how much a zone departs from a grid. This metric is based on
#'   work by Boeing (2019).
#'
#' @return \code{data.table} of zonal indices and values.
#' 
#' @references Boeing, Geoff (2019). "Urban spatial order: Street network
#'   orientation, configuration, and entropy." Applied Network Science, 4(67),
#'   \url{https://doi.org/10.1007/s41109-019-0189-1}.
#' 
#' @examples 
#' data("kampala", package="foot")
#' b <- kampala$buildings
#' 
#' # assign random groups
#' idx <- sample(1:10, nrow(b), replace=T)
#' 
#' angles <- fs_angle_entropy(b, index=idx, normalize=F)
#' angle_norm <- fs_angle_entropy(b, index=idx, normalize=T)
#' 
#' @import data.table
#' 
#' @aliases fs_angle_entropy
#' @rdname fs_angle_entropy
#' 
#' @export 
fs_angle_entropy <- function(X, index=NULL, 
                             col=NULL, 
                             normalize=TRUE) UseMethod("fs_angle_entropy")


#' @name fs_angle_entropy
#' @export
fs_angle_entropy.sp <- function(X, index=NULL, col=NULL, normalize=TRUE){
  X <- sf::st_as_sf(X)
  
  result <- fs_angle_entropy(X, index, col, normalize)
  return(result)
}


#' @name fs_angle_entropy
#' @export
fs_angle_entropy.sf <- function(X, index=NULL, col=NULL, normalize=TRUE){
  if(any(!sf::st_geometry_type(X) %in% c("POLYGON", "MULTIPOLYGON") )){
    message("Angle requires polygon shapes.")
    stop()
  }
  
  if(!is.null(col)){
    if(!col %in% names(X)){
      message("Error: column name not found.")
      stop()
    } else{
        names(X)[which(names(X)==col)] <- "fs_angle"
        result <- fs_angle_entropy_calc(X, index, normalize)
    }
  } else{
      X[["fs_angle"]] <- sapply(sf::st_geometry(X), fs_mbr)
      result <- fs_angle_entropy_calc(X, index, normalize)
  }
  return(result)
}


fs_angle_entropy_calc <- function(X, index, normalize=TRUE){
  if(!"fs_angle" %in% names(X)){
    X[["fs_angle"]] <- sapply(sf::st_geometry(X), fs_mbr)
  }
  
  indexCol <- "index" # default
  
  if(is.null(index)){
    message("No index found, treating as one group.")
    index <- rep(1, nrow(X))
  } else{
    if(is.character(index)){ 
      if(length(index)==1){ 
        if(nrow(X)>1){ # it must be a column name
          if(!index %in% colnames(X)){
            stop("Index column not found in footprints.")
          } else{
            indexCol <- index
            index <- X[[indexCol]]
          }
        } # potential issue if 1 row X and 1 column name - won't affect calcs
      } else if(length(index != nrow(X))){
        stop("Invalid length of zonal index.")
      } 
    } else if(is.numeric(index)){
      if(length(index) != nrow(X)){
        stop("Invalid length of zonal index.")
      }
    }
  } 
  
  colNam <- "fs_angle_entropy"
  DT <- data.table::data.table(idxCol=c(index, index), 
                               area_calc=c(X[["fs_angle"]], (X[["fs_angle"]] + 180) %% 360)
                              )
  data.table::setnames(DT, "idxCol", indexCol)
  
  DT[, bin := cut(area_calc, seq(5, 355, 10), labels=F) + 1]
  DT[is.na(bin), bin := 1]
  
  data.table::setkeyv(DT, indexCol)
  result <- DT[, list(entropy = -1 * sum(prop.table(table(bin)) * 
                                         log(prop.table(table(bin)))) ), 
             by=indexCol]
  
  if(normalize){ # based on Boeing (2019)
    hmax <- 3.584
    hg <- 1.386
    result[, entropy := 1 - ((entropy-hg) / (hmax - hg))^2, by=index]
  } 
  data.table::setnames(result, "entropy", colNam)
  
  return(result[]) # [] trick needed to print on return because used ':='. known data.table bug
}
