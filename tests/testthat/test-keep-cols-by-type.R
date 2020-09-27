context("keep_cols_by_type")

df <- data.frame(a = 1:4,
                 b = c(1.1, 2.2, 3.3, 4.4),
                 c = letters[1:4],
                 d = factor(letters[1:4]),
                 e = c(TRUE, FALSE, TRUE, FALSE),
                 stringsAsFactors = FALSE)

test_that("correct columns are kept", {
  numeric_target <- data.frame(a = df$a,
                               b = df$b,
                               stringsAsFactors = FALSE)
  
  categorical_target <- data.frame(c = df$c,
                                   d = df$d,
                                   e = df$e,
                                   stringsAsFactors = FALSE)

  expect_identical(keep_cols_by_type(df, "numeric"),
                   numeric_target)
  
  expect_identical(keep_cols_by_type(df, "categorical"),
                   categorical_target)
})


test_that("errors are handled", {
  no_categories <- data.frame(a = df$a)
  
  no_numerics <- data.frame(c = df$cs)
  
  expect_error(keep_cols_by_type(no_categories, "categorical"),
               regex = "type categorical")
  expect_error(keep_cols_by_type(no_numerics, "numeric"),
               regex = "type numeric")
})