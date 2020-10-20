context("build_display")

stack <- data.frame(grp  = rep(c("old", "new"), each = 10),
                    num1 = 1:20,
                    num2 = 21:40,
                    cat1 = letters[1:20],
                    cat2 = as.character(101:120),
                    cat3 = rep(TRUE, 20))

plots <- build_plots(stack, flyover_histogram, group_var = "grp")


test_that("display files are created", {
  # check that output directory is created
  build_display(plots, "test_name", "test_output")
  expect_true(file.exists("test_output"))
  
  # check that output directory is overwritten by new display
  old_files_id <- readLines(file.path("test_output", "appfiles", "id"), warn = FALSE)

  build_display(plots, "another_test", "test_output")
  new_files_id <- readLines(file.path("test_output", "appfiles", "id"), warn = FALSE)
  
  expect_false(old_files_id == new_files_id)
  
  # clean up
  unlink("test_output", recursive = TRUE, force = TRUE)
})


test_that("errors are handled", {
  names(plots)[1] <- "fiddlesticks"
  expect_error(build_display(plots, "test_name", "test_output"))
  
  names(plots) <- c("variable", "fiddlesticks")
  expect_error(build_display(plots, "test_name", "test_output"))
  
  expect_false(file.exists("test_output"))
})