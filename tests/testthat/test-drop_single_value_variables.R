run_test <- function(switch = 0) {
  if (switch == 0) {
    dataframe <- data.frame(cbind(rep(1, 1000), rep(c(1,2), 500)))
  } else {
    dataframe <- data.frame(cbind(rep(c(1,2), 500), rep(c(1,2), 500)))
  }

  mb <- drop_single_value_variables()
  newdata <- mb$run(dataframe)
  if (switch == 0) {
    expect_equal(newdata, dataframe[2])
  } else {
    expect_equal(newdata, dataframe)
  }

  dataframe <- data.frame(cbind(rep(c(1,2), 500), rep(c(1,2), 500)))
  newdata <- mb$run(dataframe)
  if (switch == 0) {
    expect_equal(newdata, dataframe[2])
  } else {
    expect_equal(newdata, dataframe)
  }
}

test_that("it correctly drops a 1-value column", run_test(0))
test_that("it correctly keeps a 2-value column", run_test(1))

