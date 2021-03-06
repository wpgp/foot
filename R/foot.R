#' foot: An R package for calculating building footprint shape metrics
#' 
#' The \code{foot} package provides functions to calculate summary statistics of
#' geometric measurements of building footprint polygons. Footprint shapes
#' representing buildings are becoming more widely available by being detected
#' and extracted from very high resolution satellite imagery. Such datasets are
#' spatially detailed but often are unlabelled. However, the size, shape, and
#' distribution of buildings can suggest possible land uses or differences in
#' structure use, socio-economic status, etc.
#' 
#' The \code{foot} package is designed to provide a set of consistent and
#' flexible tools for processing 2D vector representations of buildings. The
#' functionality includes basic geometry and morphology measures, distance and
#' clustering metrics. These calculations are supported with helper functions
#' for spatial intersections and tiled reading/writing of data.
#' 
#' The measurements in \code{foot} include: area, perimeter, nearest-neighbour
#' distance, angle of rotation for a bounding rectangle, as well as a binary
#' indicator of structure presence and a count of structures.
#' 
#' These measures can be summarised as a mean, standard deviation, coefficient
#' of variation, the nearest neighbour index of clustering, or a (normalised)
#' entropy measure for the angles. The output can be formatted as a data table
#' or as a gridded dataset.
#' 
#' @section Helper functions: The \code{foot} package provides convenience
#'   functions (\code{\link[foot]{calculate_footstats}} and
#'   \code{\link[foot]{calculate_bigfoot}}) to wrap common analysis steps
#'   together, taking a list of measurements and parameters and returning a
#'   collected output.
#' 
#' In addition to bulk processing helper functions, there are additional 
#' utility functions supplied with the package to support identifying
#' nearest neighbours, adjacent raster cells, creating zonal indices for 
#' spatial data and providing efficient I/O and parallel processing.
#' 
#' @section Credits:
#' This work was undertaken by members of the WorldPop Research Group at the
#' University of Southampton(\url{https://www.worldpop.org/}).
#' 
#' @docType package
#' @name foot
NULL
