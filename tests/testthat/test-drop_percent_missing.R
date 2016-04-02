context("drop_percent_missing")

setup_mungebit <- function(num = 0.4 * 150) {
  eval.parent(substitute({
    df <- iris
    df[, 6] <- NA
    df[seq_len(num), 6] <- 1
    initial_df <- df
    mb <- drop_percent_missing()
    df <- mb$run(df, TRUE, 0.6)
  }))
}

test_that("it does not drop a column with less than 60% missing", {
  setup_mungebit(0.4 * 150 + 1)
  expect_true(mean(is.na(df[[6]])) < 0.6)
  expect_identical(df, initial_df)
})

test_that("it drops a column with 60% missing", {
  setup_mungebit()
  expect_identical(df, iris[, 1:5])
})

test_that("it does not drop a column with less than 60% missing during predict", {
  setup_mungebit(0.4 * 150 + 1)
  expect_identical(df, initial_df)
  df <- mb$run(initial_df, TRUE, 0.6)
  expect_identical(df, initial_df)
})

test_that("it drops a column with 60% missing", {
  setup_mungebit()
  expect_identical(df, iris[, 1:5])
  # We restore the column so it has no missing values -
  # that way we ensure we are replicating the munging
  # step that occurred during training.
  initial_df[[6]] <- 1
  df <- mb$run(initial_df, TRUE, 0.6)
  expect_identical(df, iris[, 1:5])
})

