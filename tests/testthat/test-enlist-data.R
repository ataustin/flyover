context("enlist_data")

data1   <- tibble::tibble(a = 1:4)
data2   <- tibble::tibble(a = 5:8)
data3   <- tibble::tibble(a = 9:12)
not_tab <- letters

test_that("enlist_data creates expected list using names", {
  target    <- list(one = data1, two = data2, three = data3)
  data_list <- enlist_data(data1,
                           as.data.frame(data2),
                           data.table::as.data.table(data3),
                           names = c("one", "two", "three"))
  expect_equivalent(data_list, target)             # ignore attributes
  expect_equal(names(data_list), names(target))
})


test_that("enlist_data created expected list without names", {
  target    <- list(data1 = data1, data2 = data2, data3 = data3)
  data_list <- enlist_data(data1, data2, data3)
  expect_equal(data_list, target)
})

test_that("length errors are handled", {
  expect_error(enlist_data(data1, data2, data3, names = c("one", "two")))
  expect_error(enlist_data(data1, data2, names = c("one", "two", "three")))
})