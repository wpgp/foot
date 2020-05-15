#' Raster adjacencies
#' 
#' @description Find adjacent grid cells in a regular raster dataset.
#' 
#' @param r A gridded, raster object on which to search.
#' @param cells A vector of cell numbers in \code{r} to search for adjacencies.
#' @param directions The dimension to define neighbouring cells by contiguity. 
#' Accepted options include: 8, 4, "queen", "bishop", "rook" or a matrix. See 
#' details. Default is 8 which is a queen's contiguity.
#' @param include logical. Should the cell number of interest should be considered 
#' adjacent to itself? Dfault is \code{FALSE}.
#' @param dataTable logical. Should the processing and return value use \code{data.table}?
#' @return A table with cells numbers ("from") and their adjacent ("to").
#' 
#' @details The \code{directions} parameter defines neighbouring cells. This parameter 
#' follows typical contiguity measures for lattice data (e.g. "queen" or "rook"), or 
#' more complex adjacencies can be found by supplying a matrix. To use a matrix to define 
#' adjacencies, the center value should be set to zero, all adjacent cells should be 1 and
#' any cells to ignore should be set to \code{NA}.
#' 
#' @seealso \code{\link{make_circular_filter}}
#' 
#' @import data.table
#' @aliases adjacentCells
#' @rdname adjacentCells
#' 


#' @name adjacentCells
#' @export
adjacentCells <- function(r, cells, directions=8, include=FALSE, dataTable=FALSE){
  if(missing(r) | missing(cells)){
    stop("Must provide a raster and cell numbers.")
  }
  
  if(is.matrix(directions)){
    w <- directions
    
  } else if(directions==8 | directions=="queen"){
    w <- matrix(c(1,1,1, 1,0,1, 1,1,1),
                nrow=3, ncol=3)
    
  } else if(directions==4 | directions=="rook"){
    w <- matrix(c(NA, 1, NA, 1, 0, 1, NA, 1, NA), 
                nrow=3, ncol=3)
    
  } else if(directions=="bishop"){
    w <- matrix(c(1, NA, 1, NA, 0, NA, 1, NA, 1),
                nrow=3, ncol=3)
    
  }else{
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
  
  if(numNgb*length(cells) > 1e7 | dataTable==TRUE){
    adj <- data.table::data.table(fromCid=rep(cells, each=numNgb))
    adj[, fromR:=trunc((fromCid-1) / ncol(r)) + 1]
    adj[, fromC:=as.integer(fromCid - ((fromR-1) * ncol(r)))]
    
    adj[, toC:=fromC + cNgb]
    adj[, toR:=fromR + rNgb]
    adj[, toCid:=toC + ((toR-1) * ncol(r))]
    adj[, toCid:=ifelse( (toC<1 | toC>ncol(r)) | (toR<1 | toR>nrow(r)), NA, toCid )]
    
    adj[, c("toC","toR","fromR","fromC"):=NULL]  # remove columns in place
    adj <- na.omit(adj)
    
    # # alternate for REALLY big lists of cells.
    # adj <- data.table::data.table(fromCid=cells)
    # adj[, fromR:=trunc((fromCid-1) / ncol(r)) + 1]
    # adj[, fromC:=as.integer(fromCid - ((fromR-1) * ncol(r)))]
    # 
    # for(i in 1:numNgb){
    #   cN <- cNgb[i]
    #   rN <- rNgb[i]
    #   
    #   adj[, toC:=fromC + cN]
    #   adj[, toR:=fromR + rN]
    #   
    #   adj[, toCid:=toC + ((toR-1) * ncol(r))]
    #   
    #   # set(adj, (toC<1 | toC>ncol(r)) | (toR<1 | toR>nrow(r)), toCid, NA)
    #   adj[, toCid:=ifelse( (toC<1 | toC>ncol(r)) | (toR<1 | toR>nrow(r)), NA, toCid )]
    #   adj[, toR:=ifelse( toR<1 | toR>nrow(r), NA, toR )]
    #   
    #   adj[, c("toC","toR","toCid"):=NULL]  # remove columns in place
    #   gc()
    # }
    
  } else{  # matrix
    fromCid <- rep(cells, each=numNgb)
    fromR <- trunc((fromCid-1) / ncol(r)) + 1
    fromC <- as.integer(fromCid - ((fromR-1) * ncol(r)))
    
    toC <- fromC + cNgb
    toR <- fromR + rNgb
    
    toCid <- toC + ((toR-1) * ncol(r))
    toCid[(toC<1 | toC>ncol(r)) | (toR<1 | toR>nrow(r))] <- NA
    
    adj <- cbind(fromCid, toCid)
    adj <- adj[!is.na(adj[,2]),]
  }

  return(adj)  
}




