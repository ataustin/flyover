context("cognostics")

stack <- data.frame(grp  = rep(c("old", "mid", "new"), each = 5),
                    num1 = c(c(1:4, NA_integer_),
                             c(6:9, NA_integer_),
                             c(11:13, rep(NA_integer_, 2))),
                    num2 = c(1:5, rep(NA_integer_, 5), 11:15),
                    cat1 = letters[1:15],
                    cat2 = as.character(101:115),
                    cat3 = rep(TRUE, 15))


test_that("numeric summaries are correct", {
  data_split <- split(stack$num2, stack$grp)
  result     <- compute_numeric_summaries(data_split)

  expect_is(result, "list")
  expect_true(all(lengths(result) == length(unique(stack$grp))))
  expect_length(result[[1]], length(unique(stack$grp)))
  expect_true(all(vapply(result, is.numeric, logical(1))))
})


test_that("numeric cognostics are correct", {
  data_split <- split(stack$num2, stack$grp)
  summaries  <- compute_numeric_summaries(data_split)
  result     <- build_numeric_cognostics(data_split)

  expect_is(result, "data.frame")
  expect_length(result, length(summaries))
  expect_equal(names(result), name_cognostics(names(summaries)))
  expect_true(all(lengths(result) == 1))
})


test_that("quality cognostics are correct", {
  for(var in c("num1", "cat1")) {
    data_split <- split(stack[[var]], stack$grp)
    result     <- build_quality_cognostics(data_split)
    expect_is(result, "data.frame")
    expect_true(all(lengths(result) == 1))
  }
})


test_that("categorical cognostics are correct", {
  data_split <- split(stack$cat1, stack$grp)
  result     <- build_categorical_cognostics(data_split)

  expect_is(result, "data.frame")
  expect_gt(length(result), length(build_quality_cognostics(data_split)))
  expect_true(all(lengths(result) == 1))
})


test_that("build_cognostics produces desired output", {
  vars <- c("num2", "cat1", "num2")
  type <- c("numeric", "categorical", "both")

  for(i in 1:length(vars)) {
    expect_is(build_cognostics(type[i], stack, vars[i], "grp"),
              "tbl_df")
  }
})