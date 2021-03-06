% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_geometry.R
\name{fs_perimeter}
\alias{fs_perimeter}
\title{Perimeter}
\usage{
fs_perimeter(X, unit = NULL)
}
\arguments{
\item{X}{polygons of building footprints of type \code{sf}.}

\item{unit}{string indicating unit of measure. Passed to
\code{units::set_units}.}
}
\value{
numeric vector of perimeter measured for each item in \code{X}.
}
\description{
Helper geometry function to measure the perimeter of building
  footprint polygons.
}
