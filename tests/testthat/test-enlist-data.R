context("enlist_data")

data1   <- tibble::tibble(a = 1:4)
data2   <- tibble::tibble(a = 5:8)
data3   <- tibble::tibble(a = 9:12)
not_tab <- letters

test_that("enlist_data creates expected list", {
  target <- list(one = data1, two = data2, three = data3)
  expect_equivalent(enlist_data(data1,
                                as.data.frame(data2),
                                data.table::as.data.table(data3),
                                names = c("one", "two", "three")),
               target)
})


test_that("length errors are handled", {
  expect_error(enlist_data(data1, data2, data1, names = c("one", "two")))
  expect_error(enlist_data(data1, data2, names = c("one", "two", "three")))
})