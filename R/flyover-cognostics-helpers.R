split_data <- function(data, var, group_var) {
  target <- data[[var]]
  grouping <- data[[group_var]]
  split_out <- split(target, grouping, drop = FALSE)

  split_out
}


get_pct_change_min_to_max <- function(x) {
  if(all(is.na(x)) || length(stats::na.omit(x)) == 1) return(NA)
  if(all(x == 0, na.rm = TRUE)) return(0)

  max_val <- max(x, na.rm = TRUE)
  min_val <- min(x, na.rm = TRUE)

  if(all(c(max_val, min_val) < 0)) {
    old_max <- max_val
    max_val <- min_val
    min_val <- old_max
  }

  round(abs((max_val - min_val) / min_val) * 100, 2)
}


safe_stat <- function(stat) {
  switch(stat,
         "mean"   = function(x) if(all(is.na(x))) NA else mean(x, na.rm = TRUE),
         "median" = function(x) if(all(is.na(x))) NA else stats::median(x, na.rm = TRUE),
         "min"    = function(x) if(all(is.na(x))) NA else min(x, na.rm = TRUE),
         "max"    = function(x) if(all(is.na(x))) NA else max(x, na.rm = TRUE))
}


count_na <- function(x) {
  sum(is.na(x))
}


percent_na <- function(x) {
  100 * mean(is.na(x))
}


name_cognostics <- function(summary_names) {
  paste0("pct_change_", summary_names)
}