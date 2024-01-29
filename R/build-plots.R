#' Create plot objects for the trelliscope display.
#'
#' @description This function allows the user to choose from a menu
#' of prepackaged plotting styles (or to pass your own) that are designed
#' to facilitate data comparison between groups.  Plot functions are
#' specific to one type of data (numeric or categorical) and will
#' only use those columns when building plots.  For example,
#' histograms are only useful for numeric data, so categorical
#' type data is ignored.
#' 
#' @param stack Tabular data that inherits from \code{data.frame}.
#'              This is recommended to be the output of
#'              \code{\link{stack_data}} but can be built by the user.
#'              It is coerced to a \code{tibble}. 
#' @param plot_fun The \code{flyover} plotting function to apply to the
#'                 data. The user may also supply a custom function,
#'                 but must be careful to match that function with
#'                 the appropriate \code{keep_type} argument.
#'                 Note that this function must be passed without parentheses.
#' @param ... Additional arguments to pass to the \code{geom} of the
#'            supplied \code{flyover} plot function.  Use this for further
#'            modifications to the plots if needed.
#' @param group_var Character string; the column name that represents the
#'                  source of the data.  It will be used as a grouping
#'                  variable in the subsequent plots. The default value
#'                  comes from the grouping column created by default in
#'                  \code{\link{stack_data}}.
#' @param keep_type Depending on the type of plot desired,
#'                  only numeric or categorical data can be used.
#'                  By default the column type is determined by the
#'                  \code{flyover} plotting function passed to the
#'                  \code{plot_fun} argument.  If a \code{flyover} plot
#'                  is passed, this argument is ignored. However, if the user
#'                  specifies a custom plotting function, the
#'                  \code{keep_type} argument must be set to one of
#'                  \code{"numeric"}, \code{"categorical"}, or \code{"both"}.
#'                  Passing \code{"categorical"} will keep character, factor,
#'                  and logical column types.
#' @param ncores Number of cores to use for processing plots. Plot creation can
#'               be parallelized by setting \code{ncores > 1}.  Note this uses
#'               the built-in \code{parallel} package which does not support
#'               Windows, so this argument is ignored for Windows systems.
#' @param plot_mods List containing additional layers to the \code{ggplot2} call
#'                  such as theme changes, different color scales, etc.
#'                  Each layer should be a separate list element.
#'                  See \code{?ggplot2::`+.gg`} for more details.
#' @return A \code{tibble} containing, for each relevant variable of the
#'         input data, a row with a plot object and data frame of
#'         cognostics for the trelliscope display. The tibble can
#'         then be passed to \code{build_display} to create the
#'         trelliscope output.
#'
#' @export

build_plots <- function(stack, plot_fun, ..., group_var = "flyover_id_",
                        keep_type = NULL, ncores = 1, plot_mods = NULL) {
  plot_fun_parse_tree <- as.character(substitute(plot_fun))
  plot_fun_call_char  <- plot_fun_parse_tree[length(plot_fun_parse_tree)]
  
  plot_type_lookup <- get_flyover_type_lookup()
  plot_is_flyover  <- plot_fun_call_char %in% names(plot_type_lookup)
  
  check_build_plot_args(stack, group_var, keep_type, plot_is_flyover)
  if(ncores > 1) {
    if(.Platform$OS.type == "windows") {
      warning("Parallelism not supported on Windows; setting ncores = 1.")
      ncores <- 1
    }
  }
  
  stack <- tibble::as_tibble(stack)
  stack[[group_var]] <- factor(stack[[group_var]], levels = unique(stack[[group_var]]))
  
  if(is.null(keep_type)) keep_type <- plot_type_lookup[plot_fun_call_char]
  stack_reduced <- keep_cols_by_type(stack[, setdiff(names(stack), group_var)], keep_type)

  plot_vars <- names(stack_reduced)
  stack_reduced[, group_var] <- stack[, group_var, drop = TRUE]
  
  plot_list <- parallel::mclapply(plot_vars, function(var) {
                      plot_data <- stack_reduced[, c(var, group_var)]  # reduce data size in gg object
                      plot_fun(plot_data, var, group_var, ...) + plot_mods},
                  mc.cores = ncores)
  cog_list  <- parallel::mclapply(plot_vars, function(var) {
                      build_cognostics(keep_type, stack_reduced, var, group_var)},
                  mc.cores = ncores)

  names(plot_list) <- plot_vars
  names(cog_list)  <- plot_vars 

  output <- tibble::tibble(variable = plot_vars,
                           plot     = plot_list,
                           cogs     = cog_list)
  output
}


get_flyover_type_lookup <- function() {
  c(flyover_histogram      = "numeric",
    flyover_density        = "numeric",
    flyover_binline_ridges = "numeric",
    flyover_density_ridges = "numeric",
    flyover_bar_dodge      = "categorical",
    flyover_bar_fill       = "categorical",
    flyover_na_percent     = "both",
    flyover_na_count       = "both")
}


check_build_plot_args <- function(stack, group_var, keep_type, plot_is_flyover) {
  check_elements_inherit_df(list(stack))
  
  if(!group_var %in% names(stack)) {
    stop(paste("In build_plots, the grouping variable", group_var,
               "is not found in the data passed to the stack argument."),
         call.= FALSE)
  }
  
  if((!plot_is_flyover) & is.null(keep_type)) {
    stop("In build_plots, you specified a custom plotting function
         (or misspelled one by mistake).  If you are using a custom
         plot function, please specify the 'keep_type' argument as
         either 'numeric' or 'categorical'.",
         call. = FALSE)
  } else {
    if(plot_is_flyover & !is.null(keep_type)) {
      warning("In build_plots, you specified a flyover plot, so
              the keep_type argument is ignored.",
              call. = FALSE)
    }
  }
}