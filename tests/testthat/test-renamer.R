context("renamer")

test_that("it can rename zero columns", {
  iris2 <- renamer()$run(iris, NULL)
  expect_equal(colnames(iris2), colnames(iris))
})

test_that("it can rename one column", {
  iris2 <- renamer()$run(iris, list(Sepal.Length = 'seplen'))
  expect_equal(colnames(iris2), c('seplen', colnames(iris)[2:5]))
})

test_that("it can rename two columns", {
  iris2 <- renamer()$run(iris, list(Sepal.Length = 'seplen', Sepal.Width = 'sepwid'))
  expect_equal(colnames(iris2), c('seplen', 'sepwid', colnames(iris)[3:5]))
})

test_that("it can swap two column names", {
  iris2 <- renamer()$run(iris, list(Sepal.Length = 'Sepal.Width', Sepal.Width = 'Sepal.Length'))
  expect_equal(colnames(iris2), c(colnames(iris)[2:1], colnames(iris)[3:5]))
})

# mungebit tests

run_mungebit <- function(runner) {
  mb <- renamer()
  mb$run(iris, runner)
}

test_that("it can rename zero columns as a mungebit", {
  iris2 <- run_mungebit(NULL)
  expect_equal(colnames(iris2), colnames(iris))
})

test_that("it can rename one column as a mungebit", {
  iris2 <- run_mungebit(list(Sepal.Length = 'seplen'))
  expect_equal(colnames(iris2), c('seplen', colnames(iris)[2:5]))
})

test_that("it can rename two column as a mungebit", {
  iris2 <- run_mungebit(list(Sepal.Length = 'seplen', Sepal.Width = 'sepwid'))
  expect_equal(colnames(iris2), c('seplen', 'sepwid', colnames(iris)[3:5]))
})

test_that("it can swap two column names as a mungebit", {
  iris2 <- run_mungebit(list(Sepal.Length = 'Sepal.Width', Sepal.Width = 'Sepal.Length'))
  expect_equal(colnames(iris2), c(colnames(iris)[2:1], colnames(iris)[3:5]))
})

