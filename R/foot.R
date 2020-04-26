#' foot: A package for calculating building footprint shape metrics
#' 
#' The foot package provides functions to calculate summary statistics
#' of geometric measurements of building footprint polygons. Footprints
#' shapes representing buildings are becoming more widely available by
#' being detected and extracted from very high resolution satellite. Such
#' datasets are spatially detailed but often are unlabelled. However,
#' the size, shape, and distribution of buildings can suggest possible 
#' land uses or differences in structure use, socieconomic status, etc.
#' 
#' The measurements in \code{foot} include: area, perimeter, nearest-neighbour 
#' distance, angle of rotation for a bounding rectange, as well as a binary 
#' indicator of structure presence and a count of structures.
#' 
#' These measures can be summarised as a mean, standard deivation, coefficient 
#' of variation, the nearest neighbour index of clustering, or a (normalised) 
#' entropy measure for the angles, and can be output as a table or as a 
#' gridded dataset.
#' 
#' @section Helper functions:
#' While each measurement function can be accessed as a standalone function,
#' the \code{foot} package provides convenience functions to wrap common 
#' analysis steps together in a helper function, taking a list of measures 
#' and parameters and returning a collected output.
#' 
#' In addition to bulk processing helper functions, there are additional 
#' utility functions supplied with the package to support identifying
#' nearest neighbours, adjacent raster cells, creating zonal indices for 
#' spatial data and providing efficient I/O and parallel processing.
#' 
#' @section Credits:
#' This work was undertaken by members of WorldPop at the University of Southampton.
#' 
#' @docType package
#' @name foot
NULL
