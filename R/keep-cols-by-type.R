keep_cols_by_type <- function(tb, type) {
  id_col <- tb$flyover_id_
  tb$flyover_id_ <- NULL
  
  keep_fun <- switch(type,
                     numeric = is.numeric,
                     categorical = function(x) is.character(x) | is.factor(x) | is.logical(x))
  
  tb <- tb[vapply(tb, keep_fun, logical(1))]
  
  if(!length(tb)) {
    stop(paste("In keep_cols_by_type, no columns of type", type),
         call. = FALSE)
  }
  
  tb$flyover_id_ <- id_col
  tb
}
