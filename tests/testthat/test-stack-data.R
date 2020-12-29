context("stack_data")

flyover_id_ <- c("first", "second", "third")

input_unnamed <- list(data.table::data.table(a = 1),
                      tibble::tibble(a = 1, b = 2),
                      data.frame(a = 1, b = 2, c = 3))

input_named <- setNames(input_unnamed, flyover_id_)


test_that("data is correctly stacked", {
  target_drop_false <- tibble::tibble(flyover_id_ = flyover_id_,
                                      a = c(1, 1, 1),
                                      b = c(NA, 2, 2),
                                      c = c(NA, NA, 3))
  
  target_drop_true <- tibble::tibble(flyover_id_ = flyover_id_,
                                     a = c(1, 1, 1))
  
  expect_equivalent(stack_data(input_named),
                    target_drop_false)
  
  expect_equivalent(stack_data(input_named, drop_mismatches = TRUE),
                    target_drop_true)
})


test_that("name errors are handled", {
  expect_error(check_names(input_unnamed),
               regexp = "list has no names")
  
  names(input_unnamed)[1] <- "first"
  
  expect_error(check_names(input_unnamed),
               regexp = "2, 3")
  
  names(input_unnamed)[2] <- ""

  expect_error(check_names(input_unnamed),
               regexp = "2, 3")
})


test_that("common column identification errors are handled", {
  names(input_named$first) <- "z"
  
  expect_error(get_common_columns(input_named),
               regexp = "no common columns")
})