context("cognostics helpers")

stack <- data.frame(grp  = rep(c("old", "mid", "new"), each = 5),
                    num1 = c(1:14, NA_integer_),
                    num2 = c(1:5, rep(NA_integer_, 5), 11:15),
                    cat1 = letters[1:15],
                    cat2 = as.character(101:115),
                    cat3 = rep(TRUE, 15))


test_that("helper functions count_na and percent_na return required data", {
  na_vec <- c(1, 2, 3, rep(NA, 3))
  expect_equal(count_na(na_vec), 3)
  expect_equal(percent_na(na_vec), 50)

  no_na  <- 1:10
  expect_equal(count_na(no_na), 0)
  expect_equal(percent_na(no_na), 0)

  all_na <- rep(NA, 10)
  expect_equal(count_na(all_na), length(all_na))
  expect_equal(percent_na(all_na), 100)
})


test_that("helper function get_pct_change_min_to_max returns required data", {
  length_2 <- c(1, 5)
  length_3 <- c(5, 10, 15)
  one_zero <- c(0, 1)
  all_zero <- c(0, 0)
  one_negative <- c(1, -1)
  two_negative <- c(-1, -2)
  missing_one  <- c(1, 2, NA)
  keep_one     <- c(1, NA, NA)
  missing_all  <- c(NA, NA)
  expect_equal(get_pct_change_min_to_max(length_2), 400)
  expect_equal(get_pct_change_min_to_max(length_3), 200)
  expect_equal(get_pct_change_min_to_max(one_zero), Inf)
  expect_equal(get_pct_change_min_to_max(all_zero), 0)
  expect_equal(get_pct_change_min_to_max(one_negative), 200)
  expect_equal(get_pct_change_min_to_max(two_negative), 100)
  expect_equal(get_pct_change_min_to_max(missing_one), 100)
  expect_equal(get_pct_change_min_to_max(keep_one), NA)
  expect_equal(get_pct_change_min_to_max(missing_all), NA)
})


test_that("helper function safe_stat returns required data", {
  inputs <- list(filled  = 1:5,
                 some_NA = c(1:5, NA, NA),
                 all_NA  = rep(NA, 5))
  targets <- list(mean = list(filled  = 3,
                              some_NA = 3,
                              all_NA  = NA),
                  median = list(filled  = 3,
                                some_NA = 3,
                                all_NA  = NA),
                  min = list(filled  = 1,
                             some_NA = 1,
                             all_NA  = NA),
                  max = list(filled  = 5,
                             some_NA = 5,
                             all_NA  = NA))

  for(fun in names(targets)) {
    for(scenario in names(inputs)) {
      expect_equal(safe_stat(fun)(inputs[[scenario]]), targets[[fun]][[scenario]])
    }
  }
})


test_that("helper function split_data returns required data", {
  target <- list(old = letters[1:5],
                 mid = letters[6:10],
                 new = letters[11:15])
  actual <- split_data(stack, "cat1", "grp")
  expect_identical(target[names(target)], actual[names(target)])
})


test_that("helper function name_cognostics returns required names", {
  chars <- c("a", "b")
  expect_equal(name_cognostics(chars), c("pct_change_a", "pct_change_b"))
})