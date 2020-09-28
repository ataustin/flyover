context("build_plots")

stack <- data.frame(grp  = rep(c("old", "new"), each = 10),
                    num1 = 1:20,
                    num2 = 21:40,
                    cat1 = letters[1:20],
                    cat2 = as.character(101:120))


test_that("tibble output with numeric flyover function", {
  output <- build_plots(stack, flyover_histogram, group_var = "grp")
  expect_is(output, "tbl_df")
  expect_equal(names(output), c("variable", "plot"))
  expect_is(output$plot, "list")
  expect_is(output$plot[[1]], "gg")
})

# TODO:  add tests for:
#           categorical flyover function
#           custom plotting function
#           
#   

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