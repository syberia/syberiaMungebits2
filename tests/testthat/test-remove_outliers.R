context("remove_outliers")


test_that("removes eggregious outliers ", {
  vec <- runif(min = 0, max = 1, n=20) 
  vec[10] <- 10 
  df <- data.frame(vec) 
  mb <- remove_outliers()
  df <- mb$run(df) 
  expect_that(is.na(df[10, ]), equals(TRUE))
})

test_that("works with negative values ", {
  vec <- runif(min = 0, max = 1, n = 20) 
  vec[10] <- -10 
  df <- data.frame(vec) 
  mb <- remove_outliers()
  df <- mb$run(df) 
  expect_that(is.na(df[10, ]), equals(TRUE))
})

test_that("will remove outliers based upon TRAIN mean & sd ", {
  vec <- runif(min = 0, max = 1, n = 20) 
  vec[10] <- 10 
  df <- data.frame(vec) 
  mb <- remove_outliers()
  df <- mb$run(df) 
  df2 <- data.frame(vec = c(33, 19, 44, 1))
  df2 <- mb$run(df2) 
  expect_that(sum(is.na(df2[, 1])), equals(3)) # all but last value is an outlier
})

test_that("threshold argument works ", {
  vec <- runif(min = 0, max = 1, n = 20) 
  vec[10] <- -10 
  df <- data.frame(vec) 
  mb <- remove_outliers()
  df <- mb$run(df, threshold = 0) # will remove all values as the absolute value of all z-scores > 0 
  expect_that(all(is.na(df[, 1])), equals(TRUE))
})

