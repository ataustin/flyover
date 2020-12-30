build_cognostics <- function(keep_type, plot_data, var, group_var) {
  cog_function <- switch(keep_type,
                         numeric = build_numeric_cognostics,
                         categorical = build_categorical_cognostics,
                         both = build_quality_cognostics)

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
  means     <- vapply(data_split, safe_stat("mean"), numeric(1))
  medians   <- vapply(data_split, safe_stat("median"), numeric(1))
  maxes     <- vapply(data_split, safe_stat("max"), numeric(1))
  mins      <- vapply(data_split, safe_stat("min"), numeric(1))
  n_missing <- vapply(data_split, count_na, integer(1))
  
  cog_list <-  list(mean      = means,
                    median    = medians,
                    max       = maxes,
                    min       = mins,
                    n_missing = n_missing)

  cog_list
}


build_quality_cognostics <- function(data_split) {
  missing_count   <- vapply(data_split, count_na, integer(1))
  missing_percent <- vapply(data_split, percent_na, numeric(1))
  summaries       <- list(n_missing   = missing_count,
                          pct_missing = missing_percent)

  pct_diffs        <- lapply(summaries, get_pct_change_min_to_max)
  names(pct_diffs) <- name_cognostics(names(pct_diffs))
  cog_data         <- as.data.frame(pct_diffs)

  cog_data
}


build_categorical_cognostics <- function(data_split) {
  cog_data   <- build_quality_cognostics(data_split)

  n_levels <- vapply(data_split, function(x) length(unique(x)), integer(1))
  cog_data$n_levels <- max(n_levels)

  cog_data
}