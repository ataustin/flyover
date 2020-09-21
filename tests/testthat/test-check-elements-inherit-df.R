context("check_elements_inherit_df")

data1   <- data.frame(a = 1:4)
data2   <- tibble::tibble(a = 5:8)
not_tab <- letters

test_that("data inhering from data.frame is permitted", {
  expect_silent(check_elements_inherit_df(list(data1, data2)))
})

test_that("errors are handled correctly", {
  expect_error(check_elements_inherit_df(list(data1, data2, not_tab)),
               regexp = "must inherit from")
})