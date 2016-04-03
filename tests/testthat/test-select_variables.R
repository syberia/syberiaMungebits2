context("select_variables")

test_that("it correctly selects variables by numeric index", {
  iris2 <- select_variables()$run(iris, 1)
  expect_equal(iris2, iris[, 1, drop = FALSE])
})

test_that("it correctly selects variables by logical index", {
  iris2 <- select_variables()$run(iris, c(TRUE, FALSE))
  expect_equal(iris2, iris[, c(1,3,5)])
})

test_that("it correctly selects variables by character index", {
  iris2 <- select_variables()$run(iris, c("Sepal.Length", "Sepal.Width"))
  expect_equal(iris2, iris[, 1:2])
})

test_that("it preserves attributes after subsetting", {
  iris2 <- iris
  attr(iris2, "foo") <- "bar"
  iris2 <- select_variables()$run(iris2, c("Sepal.Length", "Sepal.Width"))
  expect_identical(attr(iris2, "foo"), "bar")
})

test_that("it cannot process a dataframe with duplicate columns #34", {
  iris2 <- iris
  colnames(iris2)[1:2] <- c("x", "x")
  expect_error(select_variables()$run(iris2, colnames(iris)[3:5]), "duplicate")
})


