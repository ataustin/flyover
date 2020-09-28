context("flyover plotting functions")

# These tests merely ensure that the plotting functions don't inadvertantly
# something wrong by, say, forgetting a `+` in a line of code

stack <- data.frame(grp  = rep(c("old", "new"), each = 10),
                    num = 1:20,
                    cat1 = letters[1:20])

test_that("plots return the right object", {
  expect_is(flyover_histogram(stack, "num", "grp"), "gg")
  expect_is(flyover_histogram(stack, "num", "grp"), "gg")
  
  # TODO: add additional plotting functions
})