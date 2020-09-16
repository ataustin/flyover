#' Create a named list from data
#'
#' @description  This is a convenience function serving as an
#' intermediate step in preparing the data for visualization.
#' The function builds a named list
#' of data frames (or similar objects).  The names will be used to
#' refer to the data in subsequent visualizations, so ensure they
#' are meaningful.
#' 
#' 
#' @param ... Data frames, tibbles, or data.tables separated by commas
#' @param names Character vector of names corresponding to each data
#'              data element in \code{...}
#' @return A named list
#' @examples
#' x <- data.frame(a = 1:3, b = letters[1:3])
#' y <- data.frame(a = 4:6, b = letters[4:6])
#' enlist_data(x, y, names = c("old", "new"))
#' 
#' @export

enlist_data <- function(..., names) {
  data_list <- list(...)
  
  check_elements_are_tabular(data_list)
  check_lengths(data_list, names)
  
  setNames(data_list, names)
}


check_elements_are_tabular <- function(data_list) {
  inherits_df <- vapply(data_list, inherits, logical(1), what = "data.frame")
  if(any(!inherits_df)) {
    stop("Objects must be a tabular data structure (data.frame, tibble,
         or data.table).")
  }
}


check_lengths <- function(x, y) {
  if(length(x) != length(y)) {
    stop("Lengths of supplied arguments differ.")
  }
}