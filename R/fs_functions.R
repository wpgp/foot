#' Built-in 'foot' functions
#' @description In addition to generic \code{R} functions (e.g. 'mean', 'max',
#'   etc.), these functions are designed to provide simple access to common
#'   calculations. 
#' @param x numeric vector of values to summarize.
#' @return numeric value
#' @details These functions are designed to be used within
#'   \code{calculate_footstats} which processes a \code{data.table} by group ID.
#'   Therefore all functions take a vector of values and return a single summary
#'   statistic. These functions are not likely to be used on their own.
#' @name fs_functions
NULL
#> NULL

#' @rdname fs_functions
binary <- function(x){
  return(ifelse(is.null(x), 0, 1))
}


#' @rdname fs_functions
count <- function(x){
  return(length(x))
}


#' @rdname fs_functions
cv <- function(x){
  return(sd(x) / mean(x))
}


#' @rdname fs_functions
entropy <- function(x){
  bins <- cut(x, seq(5, 355, 10), labels=F) + 1
  bins[is.na(bins)] <- 1
  calc_ent <- -1 * sum(prop.table(table(bins)) * log(prop.table(table(bins))))
  
  # normalizing step (see Boeing (2019))
  hmax <- 3.584
  hg <- 1.386
  
  calc_ent <- 1 - ((calc_ent-hg) / (hmax - hg))^2
  return(calc_ent)
}


#' @rdname fs_functions
majority <- function(x){
  return(names(which.max(table(x))))
}


# creating a function factory to use the pre-made function foot::fs_nnindex
#' Generate a nearest neighbour index function
#' @description Creates a new instance of the \code{fs_nnindex} function and
#'   initialises it with zone and unit information.
#' @param z A spatial polygon object of \code{sf} or \code{sp} type. If
#'   omitted all observations in \code{X} are assumed to be within one zone and
#'   the area of the minimum bounding circle is used for the nearest neighbour
#'   index.
#' @param zoneField (Optional) Column name of unique identifiers in \code{zone}
#'   to use. If omitted, the 'zoneID' will be numbered \code{1:nrow(zone)}.
#' @param unit character or \code{units} object to define distance. Default will
#'   attempt to coerce units to meters.
#' @details This is a function factory. It creates a partial function in order
#'   to allow \code{fs_nnindex} to be used by the internal loop of
#'   \code{calculate_footstats}. This function will generally not be used on its
#'   own.
#' @name gen_nnindex
gen_nnindex <- function(z, zoneField=NULL, unit="m"){
  force(z)
  force(zoneField)
  force(unit)
  
  function(x){
    if(length(x) == 1){
      return(0)
    } else{
      res <- fs_nnindex(sf::st_as_sf(x), z, zoneField, unit)
      return(res[["fs_nnindex"]])
    }
  }
}


