keep_cols_by_type <- function(tbl, type) {
  allowed_col_types <- unique(flyover:::get_flyover_type_lookup())
  if(!type %in% allowed_col_types) {
    types <- paste(paste0("'", allowed_col_types, "'"), collapse = ", ")
    err <- paste("In keep_cols_by_type, type must be one of", types)
    stop(err, call. = FALSE)
  }
  
  if(type == "both") return(tbl)

  keep_fun <- switch(type,
                     numeric = is_flyover_numeric,
                     categorical = is_flyover_categorical)
  
  tbl <- tbl[vapply(tbl, keep_fun, logical(1))]
  
  if(!length(tbl)) {
    stop(paste("In keep_cols_by_type, no columns of type", type),
         call. = FALSE)
  }
  
  tbl
}


is_flyover_numeric <- function(x) {
  is.numeric(x)
}


is_flyover_categorical <- function(x) {
  is.character(x) || is.factor(x) || is.logical(x)
}