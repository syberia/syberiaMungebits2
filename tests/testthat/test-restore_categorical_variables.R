context("restore_categorical_variables")

test_that("it can restore levels correctly in prediction", {
  mb <- restore_categorical_variables()
  mb$run(iris, 5)
  iris2 <- mb$run(data.frame(Species = "setosa"))
  expect_identical(levels(iris2$Species), levels(iris$Species))
})

