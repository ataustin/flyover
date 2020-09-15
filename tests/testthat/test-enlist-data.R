context("enlist_data")

test_that("enlist data creates expected list", {
  data1   <- data.frame(a = 1:4)
  data2   <- data.frame(a = 5:8)
  not_tab <- letters
  target  <- list(one = data1, two = data2)
  
  expect_equal(enlist_data(data1, data2, names = c("one", "two")),
               target)
  
  # test length mismatch error
  expect_error(enlist_data(data1, data2, data1, names = c("one", "two")))
  expect_error(enlist_data(data1, data2, names = c("one", "two", "three")))
  
  # test non-tabular-data error
  expect_error(enlist_data(data1, not_tab, names = c("one", "two")))
})