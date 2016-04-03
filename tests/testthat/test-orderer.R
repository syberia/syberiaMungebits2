context("orderer")

test_that("it orders a dataframe correctly", {
  mb <- orderer()
  iris2 <- mb$run(iris, "Sepal.Length")
  expect_equal(iris2, iris[order(iris$Sepal.Length), ])
  # Check that it works in predict.
  iris2 <- mb$run(iris, "Sepal.Length")
  expect_equal(iris2, iris[order(iris$Sepal.Length), ])
})

