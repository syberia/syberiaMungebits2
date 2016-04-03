context("imputer")

setup_imputation_mungebit <- function() {
  mb <- imputer()
  iris2 <- iris
  iris2[1, ] <- NA
  iris2 <- mb$run(iris2, c('Sepal.Length', 'Sepal.Width'))
  list(mb, iris2)
}

medians <- function(dataset) {
  unlist(lapply(dataset[2:150, 1:2], function(x) median(x, na.rm = TRUE)))
}

test_that("it imputes a column in a dataframe correctly", {
  x <- setup_imputation_mungebit()
  mb <- x[[1]]; iris2 <- x[[2]]
  expect_equal(medians(iris), unlist(iris2[1, 1:2]))
  # Ignore starred attributes for now
  expect_equal(length(grep("^[^*].*[^*]?", names(mb$.input))), 2,
    info = paste0("Expecting imputer mungebit to store inputs for 2 columns.",
                  " Did you set mutating = TRUE ",
                  " when defining the column_transformation?"))
})

test_that("it restores an imputed column correctly", {
  . <- setup_imputation_mungebit()
  mb <- .[[1]]; iris2 <- .[[2]]
  iris2[1, ] <- NA
  iris2 <- iris2[1, , drop = FALSE]
  iris2 <- mb$run(iris2, c("Sepal.Length", "Sepal.Width"))
  # make sure same medians get restored when predicting
  expect_equal(medians(iris), unlist(iris2[1, 1:2]),
    info = paste0("The imputer mungebit must be able to restore medians using ",
                  "the trained mungebit"))
})

test_that("it can handle imputation with a function column specifier",  {
 iris2 <- iris
  mb <- imputer()
  iris2[1, 1:2] <- NA
  iris2 <- mb$run(iris2, function(x) is.numeric(x) && sum(is.na(x)) > 0)
  iris2 <- iris
  iris2[1, ] <- NA
  out <- try(iris2 <- mb$run(iris2, function(x) is.numeric(x) && sum(is.na(x)) > 0))
  expect_false(is(out, "try-error"),  info = "There should not have been any warnings thrown")
  # make sure same medians get restored when predicting
  expect_equal(medians(iris), unlist(iris2[1, 1:2]),
    info = paste0("The imputer mungebit must be able to restore medians using ",
                  "the trained mungebit"))
  expect_identical(c(NA_real_, NA_real_), unname(unlist(iris2[1, 3:4])),
    info = paste0("The imputer mungebit must not restore inappropriate columns"))
})

test_that("it can impute factors (base case)", {
  
  # make a data.frame
  df <- data.frame(x=1:3, y=factor(c('A','B','B')))
  
  # train it
  mb <- imputer()
  df <- mb$run(df)
  
  # run it on a data.frame with a missing value
  df[1,2] <- NA
  df2 <- mb$run(df)
  
  # check that it works in the simplest case
  expect_identical(as.character(df2$y), c('B','B','B'), "Failed to impute")
})

test_that("for imputing factors it will take the first mode when there are more than one", {
  
  # make a data.frame
  df <- data.frame(x=1:3, y=factor(c('A','B','C')))
  
  mb <- imputer()
  # train it
  df <- mb$run(df)
  
  # run it on a data.frame with a missing value
  df[3,2] <- NA
  df <- mb$run(df)
  
  # check that it imputes 
  expect_identical(as.character(df$y), c('A','B','A'), "Fails when there are multiple modes")
})

test_that("it can impute new levels that the validation data.frame has not seen before", {
  
  # make a data.frame
  df <- data.frame(x=1:3, y=factor(c('A','B','A')))
  
  # train it
  mb <- imputer()
  df <- mb$run(df)
  
  # run it on a data.frame with a missing value
  df <- data.frame(x=1:3, y=factor(c(NA, NA, NA)))
  df <- mb$run(df)
  
  # check that it imputes 
  expect_identical(as.character(df$y), c('A','A','A'), 
                   "Fails when there is a new level in the factor to be imputed")
})


