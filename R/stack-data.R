#' Stacks a *named* list of tabular data into a single data structure.
#'
#' @description  This is a convenience function serving as an
#' intermediate step in preparing the data for visualization.
#' It is a basic wrapper for \code{dplyr::bind_rows} that works
#' with the data preparation flow of \code{flyover}.  The input list
#' must be named to correctly identify the data sets after stacking.
#' 
#' @param data_list A *named* list of tabular data elements.
#'                  This is generally the ouptut of \code{enlist_data}.
#' @param drop_mismatches Logical.  Whether to drop columns across data
#'                        sets if they fail to appear in any one data
#'                        set in the list.
#' @return A single data set; the class of the result obeys the rules of
#'         \code{\link{dplyr::bind_rows}}.  This object will contain a
#'         new column \code{flyover_id} for identifying individual source
#'         data sets after stacking.
#' @examples
#' x <- list(old = data.frame(a = 1:3),
#'           new = data.frame(a = 4:6, b = 7:9))
#' stack_data(x)
#' stack_data(x, drop_mismatches = TRUE)
#' @export

stack_data <- function(data_list, drop_mismatches = FALSE) {
  check_names(data_list)
  
  if(drop_mismatches) {
    common_cols <- get_common_columns(data_list)
    data_list[] <- lapply(data_list, `[`, i = common_cols)
  }
  
  stacked_data <- dplyr::bind_rows(data_list, .id = "flyover_id")
  stacked_data
}


check_names <- function(data_list) {
  nm <- names(data_list)
  if(any(nm == "")) {
    non_named_index <- which(nm == "")
    stop(paste("In stack_data, the following list elements require a name.\n",
               paste(non_named_index, collapse = ", ")),
         call. = FALSE)
  }
}


get_common_columns <- function(data_list) {
  all_names_list <- lapply(data_list, names)
  common_cols <- Reduce(base::intersect, all_names_list)
  
  if(!length(common_cols)) {
    stop("In stack_data, there are no common columns shared by all data sets.",
         call. = FALSE)
  }
  
  common_cols
}