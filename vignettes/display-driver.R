library(rprojroot)
library(flyover)
library(ggplot2)

root_dir <- find_root(has_dir(".git"))
vignette_dir <- file.path(root_dir, "docs", "articles")

set.seed(12345)

old <- data.frame(norm  = rnorm(n = 100, mean = 1),
                  exp   = rexp(n = 100, rate = 1),
                  chisq = rchisq(n = 100, df = 5),
                  lnorm = rlnorm(n = 100, meanlog = 0),
                  gamma = rgamma(n = 100, shape = 2),
                  alpha = sample(letters[1:4], size = 100, replace = TRUE, prob = c(1, 2, 3, 4)),
                  hilo  = sample(c("high", "low"), size = 100, replace = TRUE, prob = c(1, 2)),
                  tf    = sample(c(TRUE, FALSE), size = 100, replace = TRUE, prob = c(1, 1)),
                  fruit = sample(c("apple", "banana", "pear"), size = 100, replace = TRUE, prob = c(1, 2, 3)))


new <- data.frame(norm  = rnorm(n = 100, mean = 3),
                  exp   = rexp(n = 100, rate = 3),
                  chisq = rchisq(n = 100, df = 10),
                  lnorm = rlnorm(n = 100, meanlog = 2),
                  gamma = rgamma(n = 100, shape = 4),
                  alpha = sample(letters[1:4], size = 100, replace = TRUE, prob = c(4, 3, 2, 1)),
                  hilo  = sample(c("high", "low"), size = 100, replace = TRUE, prob = c(2, 1)),
                  tf    = sample(c(TRUE, FALSE), size = 100, replace = TRUE, prob = c(1, 3)),
                  fruit = sample(c("apple", "banana", "pear"), size = 100, replace = TRUE, prob = c(3, 2, 1)))


data_list  <- enlist_data(old, new, names = c("old data", "new data"))
data_stack <- stack_data(data_list, group_var = "source")


# building-displays.Rmd

f_binline <- build_plots(data_stack, flyover_binline_ridges, group_var = "fruit")
build_display(f_binline,
              display_name = "binline",
              output_dir = file.path(vignette_dir, "display-binline-fruit"))


# custom-plots.Rmd

custom_ecdf <- function(tbl, var, group_var, ...) {
  ggplot(tbl, aes_string(x = var, color = group_var)) +
    stat_ecdf(size = 2, ...) +
    theme_minimal(base_size = 14)
}

ecdf <- build_plots(data_stack, custom_ecdf, "source", keep_type = "numeric")
build_display(ecdf,
              display_name = "custom ECDF",
              output_dir   = file.path(vignette_dir, "display-ecdf"))


# gallery.Rmd

flyover_lookup <- flyover:::get_flyover_type_lookup()
flyover_fun_names <- names(flyover_lookup)
flyover_funs <- sapply(flyover_fun_names,
                       function(x) eval(parse(text = x)),  # yep I went there
                       USE.NAMES = TRUE,
                       simplify = "list")

fun_name_to_dir_name <- function(fun_name) {
  remove_flyover <- gsub("flyover_", "display-", fun_name)
  dir_name  <- gsub("_", "-", remove_flyover)
  dir_name
}


for(fun_name in flyover_fun_names) {
  this_plot <- build_plots(data_stack,
                           flyover_funs[[fun_name]],
                           group_var = "source",
                           keep_type = flyover_lookup[fun_name])
  
  # need to use print() to get each display's index.html to show up /shrug
  print(build_display(this_plot,
                      display_name = fun_name,
                      output_dir   = file.path(vignette_dir, fun_name_to_dir_name(fun_name))))
}