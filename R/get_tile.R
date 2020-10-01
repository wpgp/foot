#' gridTiles
#' 
#' @title gridTiles
#' @description Split gridded data extents into subsets for parallel processing.
#' @param X The gridded (\code{raster} or \code{stars}) object to find tiles
#' @param n number of tiles. Can be a vector length 1 or of length 2 (rows,
#'   columns)
#' @param px number of pixels per side of a tile. Can be a vector of length 2
#'   (rows, column pixels). Ignored if n provided. Default is 1000.
#' @param overlap number of pixels of overlap between internal tiles
#'
#' @details \code{gridTiles} provides a convenient way for splitting a gridded
#'   dataset into sub-datasets for cropping or processing individually. The
#'   splitting uses the extent of the total grid, potentially including noData
#'   cells.
#'
#' @return \code{data.frame} where each row is a set of tile indices and sizes.
#'   \itemize{ 
#'   \item xl leftmost column index 
#'   \item yl lowest row index 
#'   \item xu rightmost column index 
#'   \item yu topmost row index 
#'   \item cropXsize number of columns in the tile 
#'   \item cropYsize number of rows in the tile 
#'   }
#'   
#'  @examples 
#'  data("kampala")
#'  # example dataset
#'  g <- kampala$mastergrid
#'  
#'  gridTiles(g)
#'  gridTiles(g, n=2)
#'  gridTiles(g, px=c(5, 25))
#' 
#' @aliases gridTiles
#' @rdname gridTiles
#' 
#' @export
gridTiles <- function(X, n=NULL, px=c(1000, 1000), overlap=0) UseMethod("gridTiles")


#' @name gridTiles
#' @export
gridTiles.stars <- function(X, n=NULL, px=c(1000, 1000), overlap=0){
  
  if(!is.null(n)){
    if(length(n) == 1){
      pxX <- pxY <- max(ceiling(stars::st_dimensions(X)$y$to / n),
                        ceiling(stars::st_dimensions(X)$x$to / n))
    } else{
      pxY <- ceiling(stars::st_dimensions(X)$y$to / n[1])
      pxX <- ceiling(stars::st_dimensions(X)$x$to / n[2])
    }
  } else{
    if(length(px) == 1){
      pxX <- pxY <- px
    } else{
      pxY <- px[1]
      pxX <- px[2]
    }
  }
  
  xfrom <- stars::st_dimensions(X)$x$from
  xto <- stars::st_dimensions(X)$x$to
  yfrom <- stars::st_dimensions(X)$y$from
  yto <- stars::st_dimensions(X)$y$to
  
  xl <- seq(xfrom, xto, pxX)
  xu <- xl - 1L + pxX
  yl <- seq(yfrom, yto, pxY)
  yu <- yl - 1L + pxY
  
  l <- expand.grid(xl=xl, yl=yl)
  u <- expand.grid(xu=xu, yu=yu)
  tiles <- cbind(l, u)
  
  if(overlap > 0){
    tiles$xl <- tiles$xl - overlap
    tiles$yl <- tiles$yl - overlap
    tiles$xu <- tiles$xu + overlap
    tiles$yu <- tiles$yu + overlap
  }
  
  # refit to bbox
  tiles[tiles$xl < xfrom, "xl"] <- xfrom
  tiles[tiles$yl < yfrom, "yl"] <- yfrom
  tiles[tiles$xu > xto, "xu"] <- xto
  tiles[tiles$yu > yto, "yu"] <- yto
  
  tiles$cropXsize <- tiles$xu - tiles$xl + 1L
  tiles$cropYsize <- tiles$yu - tiles$yl + 1L
  
  return(tiles)
}


#' @name gridTiles
#' @export
gridTiles.RasterLayer <- function(X, n=NULL, px=c(1000, 1000), overlap=0){
  
  return(gridTiles(stars::st_as_stars(X, proxy=TRUE), n, px, overlap))
}
