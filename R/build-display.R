#' Build a trelliscope display of flyover plots.
#'
#' @description This function produces an interactive display for collecting,
#' organizing, and displaying the plots generated in \code{\link{build_plots}}.
#' The bulk of the work is done by \code{trelliscopejs::trelliscope}.
#' Please see the documentation of this package for additional details.
#' 
#' @param plot_data A \code{tibble} output from \code{build_plots}.
#' @param display_name Character string of the name to give the trelliscope display.
#' @param output_dir The file path of the directory to create in which the
#'                   outputs of the display will be written.  Caution: the function
#'                   will overwrite any files stored in this directory!
#' @param nrow The number of rows of plots to display.
#' @param ncol The number of columns of plots to display.
#' @param sort_var Character string of the cognostic to sort on.  Defaults to the
#'                 variable name, but can be any of the cognostics of interest.
#' @param sort_direction Character string specifying direction of sort;
#'                       can be "asc" for ascending or "desc" for descending.
#'                       Defaults to "asc".
#' @param self_contained Logical; whether to render the display as a
#'                       self-contained HTML document.  This should be set
#'                       to \code{TRUE} for including the display in an
#'                       R markdown HTML document, but generally can be left
#'                       as \code{FALSE} for producing an output directory
#'                       where plots and relevant files will be stored.
#' @param ... additional arguments passed to \code{trelliscopejs::trelliscope}.
#'
#' @export

build_display <- function(plot_data,
                          display_name,
                          output_dir,
                          nrow = 1,
                          ncol = 2,
                          sort_var = "variable",
                          sort_direction = "asc",
                          self_contained = FALSE,
                          ...) {
  
  if(!all((c("variable", "plot") %in% names(plot_data)))) {
    stop(paste("In build_display, column names of data do not match",
               "expected values.  Is plot_data the output of build_plots?"),
         call. = FALSE)
  }
  
  unlink(output_dir, recursive = TRUE, force = TRUE)
  
  trelliscopejs::trelliscope(x = plot_data,
                             name = display_name,
                             path = output_dir,
                             nrow = nrow,
                             ncol = ncol,
                             self_contained = self_contained,
                             panel_col = "plot",
                             state = list(sort = trelliscopejs::sort_spec(sort_var, sort_direction)),
                             ...)
}