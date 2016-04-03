context("replace_variable")

test_that("it can replace variable with a one argument function", {
  iris2 <- replace_variable()$run(iris, function(Sepal.Width) Sepal.Width + 1)
  expect_equal(iris2$Sepal.Width, iris$Sepal.Width + 1)
})

test_that("it can replace variable with a two argument function", {
  iris2 <- replace_variable()$run(iris, function(Sepal.Width, Sepal.Length) Sepal.Width + Sepal.Length)
  expect_equal(iris2$Sepal.Width, iris$Sepal.Width + iris$Sepal.Length)
})

