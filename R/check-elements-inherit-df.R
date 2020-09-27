check_elements_inherit_df <- function(data_list) {
  inherits_df <- vapply(data_list, inherits, logical(1), what = "data.frame")
  if(any(!inherits_df)) {
    stop("Tabular data must inherit from class data.frame
          (e.g. data.frame, tibble, or data.table).")
  }
}