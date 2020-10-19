context("build_plots")

stack <- data.frame(grp  = rep(c("old", "new"), each = 10),
                    num1 = 1:20,
                    num2 = 21:40,
                    cat1 = letters[1:20],
                    cat2 = as.character(101:120),
                    cat3 = rep(TRUE, 20))


test_that("tibble output with numeric flyover function", {
  output <- build_plots(stack, flyover_histogram, group_var = "grp")
  expect_is(output, "tbl_df")
  expect_equal(nrow(output), 2)
  expect_equal(names(output), c("variable", "plot"))
  expect_is(output$plot, "list")
  expect_is(output$plot[[1]], "gg")
})


test_that("tibble output with categorical flyover function", {
  output <- build_plots(stack, flyover_bar_dodge, group_var = "grp")
  expect_is(output, "tbl_df")
  expect_equal(nrow(output), 3)
  expect_equal(names(output), c("variable", "plot"))
  expect_is(output$plot, "list")
  expect_is(output$plot[[1]], "gg")
})


test_that("tibble output with custom ggplot function", {
  custom_plot <- function(tbl, var, grp) {
    ggplot(tbl, aes_string(x = var, y = 1:nrow(tbl), color = grp)) +
      geom_point()
  }
  
  output <- build_plots(stack, custom_plot, "grp", keep_type = "numeric")
  expect_is(output, "tbl_df")
  expect_equal(nrow(output), 2)
  expect_equal(names(output), c("variable", "plot"))
  expect_is(output$plot, "list")
  expect_is(output$plot[[1]], "gg")
  
  output <- build_plots(stack, custom_plot, "grp", keep_type = "categorical")
  expect_is(output, "tbl_df")
  expect_equal(nrow(output), 3)
  expect_equal(names(output), c("variable", "plot"))
  expect_is(output$plot, "list")
  expect_is(output$plot[[1]], "gg")
  
})


test_that("plot changes do not produce errors", {
  # test plot_mots
  expect_silent(build_plots(stack, flyover_histogram, "grp",
                            plot_mods = xlab("A")))
  expect_silent(build_plots(stack, flyover_histogram, "grp",
                            plot_mods = list(xlab("A"))))
  expect_silent(build_plots(stack, flyover_histogram, "grp",
                            plot_mods = list(xlab("A"), theme_classic())))
  
  
  # test dots
  expect_silent(build_plots(stack, flyover_bar_dodge, "grp",
                            alpha = 0.5))
  expect_warning(build_plots(stack, flyover_histogram, "grp",
                             nonsense = 100),
                 regexp = "unknown parameters: nonsense")
  expect_warning(build_plots(stack, flyover_histogram, "grp",
                             alpha = 1),
                 regexp = "Duplicated aesthetics")
})


test_that("errors and warnings are issued", {
  expect_error(build_plots(letters, flyover_histogram, group_var = "grp"),
               regexp = "must inherit from class data.frame")
  
  expect_error(build_plots(stack, flyover_histogram, group_var = "nonsense"),
               regexp = "nonsense is not found")
  
  custom_plot <- function(a, b, c) return("custom")
  expect_error(build_plots(stack, custom_plot, group_var = "grp"),
               regexp = "specify the 'keep_type' argument")
  
  expect_warning(build_plots(stack, flyover_histogram, group_var = "grp", keep_type = "numeric"),
                 regexp = "keep_type argument is ignored")
})