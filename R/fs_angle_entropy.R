#' Building angle calculation
#' 
#' @description Calculate the entropy of rotation angles for building footprint
#' polygons within zones.
#' 
#' @inheritParams fs_area_mean
#' @param normalize A logical value indicating whether to normalize the entropy. 
#' Default is \code{TRUE}.
#' 
#' @details This measure uses the angle of the minimum rotated rectangle enclosing 
#' each footprint poygon. Entropy is an information criteria measure. When summarising
#' the angles of footprints, higher entropy values may suggest less formally planned 
#' or zoned areas. The entropy calculation uses the common Shannon's Entropy. The 
#' normalization step produces an indicator for how much a zone departs from a grid.
#'
#' @return \code{data.table} of zonal indices and values.
#' 
#' @author Chris Jochem
#' 
#' @import data.table
#' 
#' @aliases fs_angle_entropy
#' @rdname fs_angle_entropy
#' 
#' @export 
fs_angle_entropy <- function(X, index=NULL, col=NULL, normalize=TRUE) UseMethod("fs_angle_entropy")


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
  
  if(is.null(index)){
    warning("No index found, treating as one group.")
    index <- rep(1, nrow(X))
  } else{
    if(length(index)==1){
      if((is.numeric(index) & index <= ncol(X)) | 
         (is.character(index) & index %in% names(X))){
        index <- X[[index]]
      }
    } else if(length(index) != nrow(X)){
      message("Invalid index")
      stop()
    }
  } 
  
  # abins <- cut(0:360, seq(5, 355, 10), labels=F) + 1
  # abins[is.na(abins)] <- 1
  
  colNam <- "fs_angle_entropy"
  DT <- data.table::data.table(index=c(index, index), 
                               area_calc=c(X[["fs_angle"]], (X[["fs_angle"]] + 180) %% 360)
                              )
  DT[, bin := cut(area_calc, seq(5, 355, 10), labels=F) + 1]
  DT[is.na(bin), bin := 1]
  
  data.table::setkey(DT, index)
  result <- DT[, list(entropy = -1 * sum(prop.table(table(bin)) * 
                                         log(prop.table(table(bin)))) ), 
             by=index]
  
  if(normalize){ # based on Boeing (2019)
    hmax <- 3.584
    hg <- 1.386
    
    result[, entropy := 1 - ((entropy-hg) / (hmax - hg))^2, by=index]
  } 
  
  data.table::setnames(result, "entropy", colNam)
  
  return(result)
}
