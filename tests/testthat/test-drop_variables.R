context("drop_variables()$run")

test_that("it correctly drops variables by numeric index", {
  iris2 <- drop_variables()$run(iris, 1)
  expect_equal(iris2, iris[, 2:5])
})

test_that("it correctly drops variables by logical index", {
  iris2 <- drop_variables()$run(iris, c(T,F))
  expect_equal(iris2, iris[, c(2,4)])
})

test_that("it correctly drops variables by character index", {
  iris2 <- drop_variables()$run(iris, c("Sepal.Length", "Sepal.Width"))
  expect_equal(iris2, iris[, 3:5])
})

test_that("drops down to a 1-column dataframe, not an atomic vector", {
  iris2 <- drop_variables()$run(iris, 2:5)
  expect_equal(iris2, iris[, 1, drop = FALSE])
})

