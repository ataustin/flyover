context("enlist_data")

data1   <- data.frame(a = 1:4)
data2   <- data.frame(a = 5:8)
not_tab <- letters

test_that("enlist_data creates expected list", {
  target<- list(one = data1, two = data2)
  expect_equal(enlist_data(data1, data2, names = c("one", "two")),
               target)
})


test_that("length errors are handled", {
  expect_error(enlist_data(data1, data2, data1, names = c("one", "two")))
  expect_error(enlist_data(data1, data2, names = c("one", "two", "three")))
})