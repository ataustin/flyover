keep_cols_by_type <- function(df, type) {
  id_col <- df$flyover_id_
  df$flyover_id_ <- NULL
  
  keep_fun <- switch(type,
                     numeric = is.numeric,
                     categorical = function(x) is.character(x) | is.factor(x) | is.logical(x))
  
  df <- df[vapply(df, keep_fun, logical(1))]
  
  if(!length(df)) {
    stop(paste("In keep_cols_by_type, no columns of type", type),
         call. = FALSE)
  }
  
  df$flyover_id_ <- id_col
  df
}
