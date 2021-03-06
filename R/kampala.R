#' Kampala building footprint sample
#' 
#' @description Sample of building footprint polygons from a small region 
#' of Kampala, Uganda with 4840 structures represented. In addition the 
#' dataset contains a 3 arc-second grid to serve as a template for gridding 
#' values and polygons of "administrative boundaries" and "survey clusters."
#' 
#' The building data were produced by Microsoft Bing Maps. The mastergrid is 
#' aligned to gridded data produced by \href{https://www.world.org}{WorldPop}. 
#' The adminZones and cluster datasets are purely artificial and for demonstration 
#' purposes only.
#' 
#' @docType data
#'
#' @usage data(kampala)
#'
#' @format A list with four objects.
#' \itemize{
#' \item "buildings" - polygons of building footprints in \code{sf} format
#' \item "mastergrid" - geoTiff \code{RasterLayer}
#' \item "adminZones" - polygons in \code{sf} format for zonal statistics
#' \item "clusters" - 10 small polygons in \code{sf} format for sample sites
#' }
#'
#' @keywords datasets
#' 
#' @source \href{https://github.com/microsoft/Uganda-Tanzania-Building-Footprints}{Uganda-Tanzania Building Footprints}
#'
#'
"kampala"