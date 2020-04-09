#' Raster adjacencies
#' 
#' @description Find adjacent grid cells in a regular raster
#' @param txt Text to append to "foot"
#' @return TBD.
#' @author Chris Jochem
#' 
#' 
#' @aliases adjacentCells
#' @rdname adjacentCells
#' 


#' @name adjacentCells
#' @export
adjacentCells <- function(r, cells, directions=8, include=FALSE){
  if(directions==8 | directions=="queen"){
    w <- matrix(c(1,1,1, 1,0,1, 1,1,1),
                nrow=3, ncol=3)
    
  } else if(directions==4 | directions=="rook"){
    w <- matrix(c(NA, 1, NA, 1, 0, 1, NA, 1, NA), 
                nrow=3, ncol=3)
    
  } else if(is.matrix(directions)){
    w <- directions
    
  } else{
    stop("Invalid direction for adjacencies.")
  }
  
  ctr <- which(w==0, arr.ind=T)

  cNgb <- rep(1:ncol(w), each=nrow(w)) - ctr[,2]
  rNgb <- rep(nrow(w):1, ncol(w)) - (nrow(w)-ctr[,1]+1)

  cNgb <- cNgb * w
  rNgb <- rNgb * w

  # if no self-matching
  if(!include){
    cNgb[which(w != 1)] <- NA
    rNgb[which(w != 1)] <- NA
  }

  cNgb <- stats::na.omit(as.vector(cNgb))
  rNgb <- stats::na.omit(as.vector(rNgb))

  # construct neighbouring cell indices
  numNgb <- length(cNgb)
  fromCid <- rep(cells, each=numNgb)
  fromR <- trunc((fromCid-1) / ncol(r)) + 1
  fromC <- as.integer(fromCid - ((fromR-1) * ncol(r)))
  
  toC <- fromC + cNgb
  toR <- fromR + rNgb
  
  toCid <- toC + ((toR-1) * ncol(r))
  toCid[(toC<1 | toC>ncol(r)) | (toR<1 | toR>nrow(r))] <- NA
  
  adj <- cbind(fromCid, toCid)
  adj <- adj[!is.na(adj[,2]),]

  return(adj)  
}




