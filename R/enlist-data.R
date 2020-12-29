#' Create a named list from data
#'
#' @description  This is a convenience function serving as an
#' intermediate step in preparing the data for visualization.
#' The function builds a named list of the data elements.
#' The names will be used to group the data in subsequent
#' visualizations, so please ensure they are meaningful.
#' Each element will be coerced to a \code{tibble}
#' and row names are dropped, SORRY.
#' 
#' 
#' @param ... Comma-separated input of tabular-type data.  Any class
#'            that inherits from \code{data.frame} is permitted.  Group names
#'            are taken from these objects, unless \code{names} is specified
#' @param names Optional character vector of names corresponding to each data
#'              data element in \code{...}
#' @return A named list of \code{tibble}s
#' @examples
#' x <- data.frame(a = 1:3, b = letters[1:3])
#' y <- data.frame(a = 4:6, b = letters[4:6])
#'
#' # groups will be named "x" and "y"
#' enlist_data(x, y)
#' 
#' # groups will be named "old" and "new"
#' enlist_data(x, y, names = c("old", "new"))
#' 
#' @export

enlist_data <- function(..., names = NULL) {
  data_list <- list(...)
  check_elements_inherit_df(data_list)
  
  if(is.null(names)) names <- vapply(match.call()[-1], deparse, character(1))
  check_lengths(data_list, names)

  data_list[] <- lapply(data_list, tibble::as_tibble)
  list_out <- stats::setNames(data_list, names)

  list_out
}


check_lengths <- function(x, y) {
  if(length(x) != length(y)) {
    stop("Lengths of supplied arguments differ.")
  }
}