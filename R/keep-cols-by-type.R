keep_cols_by_type <- function(tbl, type) {
  if(!type %in% c("numeric", "categorical")) {
    stop("In keep_cols_by_type, type must be either 'numeric' or 'categorical'.",
         call. = FALSE)
  }
  
  keep_fun <- switch(type,
                     numeric = is.numeric,
                     categorical = function(x) is.character(x) | is.factor(x) | is.logical(x))
  
  tbl <- tbl[vapply(tbl, keep_fun, logical(1))]
  
  if(!length(tbl)) {
    stop(paste("In keep_cols_by_type, no columns of type", type),
         call. = FALSE)
  }
  
  tbl
}
