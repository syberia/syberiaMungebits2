context("select_rows()$run")

test_that("it correctly selects rows by numeric index", {
  iris2 <- select_rows()$run(iris, 1:10)
  expect_equal(iris2, iris[1:10, ])
})

test_that("it correctly selects rows by logical index", {
  iris2 <- select_rows()$run(iris, c(T,F))
  expect_equal(iris2, iris[c(T,F), ])
})

test_that("it correctly selects rows by function with whole = TRUE", {
  iris2 <- iris
  iris2[1, 1] <- NA
  iris2 <- select_rows()$run(iris2, complete.cases, whole = TRUE)
  expect_equal(iris2, iris[-1, ])
})

test_that("it correctly selects rows by function", {
  dd <- data.frame(c(1,2,3), c(4,5,6), c(7,8,9)) 
  dd2 <- dd
  dd <- select_rows()$run(dd, function(x) sum(x) > 15)
  expect_equal(dd, dd2[3, ]) 
})

