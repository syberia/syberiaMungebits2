context("new_variable")

test_that("it can make a new column", {
  iris2 <- new_variable()$run(
    iris,
    function(Sepal.Width, Sepal.Length) Sepal.Width*2 + Sepal.Length*2,
    "Sepal.Perimeter"
  )
  expect_equal(
    iris2$Sepal.Perimeter,
    iris$Sepal.Width*2 + iris$Sepal.Length*2
  )
})

test_that("it can make a new column without a function", {
  iris2 <- new_variable()$run(iris, Sepal.Width*2, "Sepal.Width2", "Sepal.Width")
  expect_equal(iris2$Sepal.Width2, iris$Sepal.Width*2)
})

