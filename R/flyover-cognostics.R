build_cognostics <- function(keep_type, plot_data, var, group_var) {
  cog_function <- switch(keep_type,
                         numeric = build_numeric_cognostics,
                         categorical = build_categorical_cognostics,
                         both = build_numeric_cognostics)

  data_split <- split_data(plot_data, var, group_var)
  cog_data   <- cog_function(data_split)
  
  tibble::tibble(cog_data)
}


build_numeric_cognostics <- function(data_split) {
  summaries        <- compute_numeric_summaries(data_split)
  pct_diffs        <- lapply(summaries, get_pct_change_min_to_max)
  names(pct_diffs) <- name_cognostics(names(pct_diffs))
  cog_data         <- as.data.frame(pct_diffs)

  cog_data
}


compute_numeric_summaries <- function(data_split) {
  means    <- vapply(data_split, safe_stat("mean"), numeric(1))
  medians  <- vapply(data_split, safe_stat("median"), numeric(1))
  maxes    <- vapply(data_split, safe_stat("max"), numeric(1))
  mins     <- vapply(data_split, safe_stat("min"), numeric(1))
  counts   <- vapply(data_split, length, integer(1))
  missings <- vapply(data_split, count_na, integer(1))
  
  cog_list <-  list(mean    = means,
                    median  = medians,
                    max     = maxes,
                    min     = mins,
                    count   = counts,
                    missing = missings)

  cog_list
}


build_categorical_cognostics <- function(data_split) {
  summaries        <- compute_categorical_summaries(data_split)
  diffable_list    <- summaries[c("count", "missing")]
  pct_diffs        <- lapply(diffable_list, get_pct_change_min_to_max)
  names(pct_diffs) <- name_cognostics(names(pct_diffs))
  
  cog_data <- as.data.frame(pct_diffs)
  cog_data$n_levels <- max(summaries$n_levels)

  cog_data
}


compute_categorical_summaries <- function(data_split) {
  counts   <- vapply(data_split, length, integer(1))
  missings <- vapply(data_split, count_na, integer(1))
  n_levels <- vapply(data_split, function(x) length(unique(x)), integer(1))

  cog_list <- list(count    = counts,
                   missing  = missings,
                   n_levels = n_levels)
}


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



name_cognostics <- function(summary_names) {
  paste0("pct_change_", summary_names)
}