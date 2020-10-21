#' @title Select variables 
#' @description Functions to construct and check for lists of variables for
#'   \code{foot}.
#' @param ... Variables to include together as parameters for summary functions.
#' 
#' @details This helper function is used by \code{calculate_footstats} to select
#'   multiple columns or building characteristics to be used in a summary
#'   function. This facilitates using functions that take multiple arguments.
#'   
#' @seealso \code{[calculate_footstats]} for examples of summary functions.
#'
#' @name fs_varlist
#' @export
fs_varlist <- function(...){
  l <- list(...)
  class(l) <- "fs_varlist"
  return(l)
}

#' @param x object to be tested.
#' @rdname fs_varlist
#' @export
is.fs_varlist <- function(x){ inherits(x, "fs_varlist") }

