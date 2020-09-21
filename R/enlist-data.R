#' Create a named list from data
#'
#' @description  This is a convenience function serving as an
#' intermediate step in preparing the data for visualization.
#' The function builds a named list of \code{data.frame}s.
#' The names will be used to refer to the data in subsequent
#' visualizations, so ensure they are meaningful.
#' Each element will be coerced to a \code{data.frame}, SORRY.
#' 
#' 
#' @param ... Comma-separated input of tabular-type data.  Any class
#'            that inherits from \code{data.frame} is permitted.
#'            These are coerced to a \code{data.frame}.
#' @param names Character vector of names corresponding to each data
#'              data element in \code{...}
#' @return A named list of \code{data.frame}s
#' @examples
#' x <- data.frame(a = 1:3, b = letters[1:3])
#' y <- data.frame(a = 4:6, b = letters[4:6])
#' enlist_data(x, y, names = c("old", "new"))
#' 
#' @export

enlist_data <- function(..., names) {
  data_list <- list(...)
  
  check_elements_inherit_df(data_list)
  check_lengths(data_list, names)
  
  data_list[] <- lapply(data_list, as.data.frame, stringsAsFactors = FALSE)
  setNames(data_list, names)
}


check_lengths <- function(x, y) {
  if(length(x) != length(y)) {
    stop("Lengths of supplied arguments differ.")
  }
}