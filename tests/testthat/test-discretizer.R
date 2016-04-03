context("discretizer")

test_that("it correctly discretizes iris data set", {
 mb <- discretizer()
 iris2 <- mb$run(iris, 1:4, mode_freq_threshold = 0.2)
 expect_equal(iris2, iris_discretized,
   info = paste0("The iris dataset must have been discretized correctly and ",
                 "match the values in the iris_discretized dataset."))
})

test_that("it correctly restores iris data set", {
 mb <- discretizer()
 # mode_freq_threshold = 0.15 actually fails to discretize...
 iris2 <- mb$run(iris, 1:4, mode_freq_threshold = 0.2)
 # prediction run
 one_row_discretized <- iris_discretized[1, , drop = FALSE]
 iris2 <- iris[1, , drop = FALSE]
 iris2 <- mb$run(iris2, 1:4)
 expect_equal(iris2, one_row_discretized,
   info = paste0("The discretizer must be able to restore levels using",
                 "the levels generated during the training run."))

 # for good measure, test multiple row datasets as well
 ten_rows_discretized <- iris_discretized[1:10, , drop = FALSE]
 iris2 <- iris[1:10, , drop = FALSE]
 iris2 <- mb$run(iris2, 1:4)
 expect_equal(iris2, ten_rows_discretized,
   info = paste0("The discretizer must be able to restore levels using",
                 "the levels generated during the training run."))
})

test_that("it does not discretize values with uniques below the lower bound", {
 mb <- discretizer()
 iris2 <- mb$run(iris, 1:4, mode_freq_threshold = 0.2,
                 lower_count_bound = 22)
 # Only the fourth column of iris has <= 22 uniques
 expect_equal(iris2[, -4], iris_discretized[, -4]);
 expect_equal(iris2[, 4], iris[, 4])

 # test prediction
 iris2 <- mb$run(iris, 1:4)
 expect_equal(iris2[, -4], iris_discretized[, -4]);
 expect_equal(iris2[, 4], iris[, 4])
})

test_that("it does not discretize values with uniques above the upper bnd", {
 mb <- discretizer()
 iris2 <- mb$run(iris, 1:4, mode_freq_threshold = 0.2,
                 upper_count_bound = 23)
 # Only the fourth column of iris has < 23 uniques
 expect_equal(iris2[, -4], iris[, -4]);
 expect_equal(iris2[, 4], iris_discretized[, 4])

 # test prediction
 iris2 <- mb$run(iris, 1:4)
 expect_equal(iris2[, -4], iris[, -4]);
 expect_equal(iris2[, 4], iris_discretized[, 4])
})

test_that("it correctly uses the missing_level argument", {
 df <- data.frame(first = 1:100); df[1, 1] <- NA
 mb <- discretizer()
 df <- mb$run(df, 1, missing_level = "Not here")

 expected_discretized_column <-
   factor(c('Not here', rep('[ 2, 35)', 33), rep('[35, 68)', 33), rep('[68,100]', 33)))
 expect_equal(df[[1]], expected_discretized_column)

 # test prediction
 df <- data.frame(first = 1:50); df[1, 1] <- NA
 df <- mb$run(df, 1, missing_level = "Not here")
 expect_equal(df[[1]], expected_discretized_column[1:50])
})

test_that("it correctly uses the missing_level argument if it is NULL", {
 df <- data.frame(first = 1:100); df[1, 1] <- NA
 mb <- discretizer()
 df <- mb$run(df, 1, missing_level = NULL)

 expected_discretized_column <-
   factor(c(NA, rep('[ 2, 35)', 33), rep('[35, 68)', 33), rep('[68,100]', 33)))
 expect_equal(df[[1]], expected_discretized_column)

 # test prediction
 df <- data.frame(first = 1:50); df[1, 1] <- NA
 df <- mb$run(df, 1, missing_level = NULL)
 expect_equal(df[[1]], expected_discretized_column[1:50])
})

test_that("it moves values that are not observed in the training set to closest bin", {
 df <- data.frame(first = 1:100)
 mb <- discretizer()
 df <- mb$run(df, 1)
 # test prediction
 expected_discretized_column <-
   factor(c(rep('[ 1, 35)', 34), rep('[35, 68)', 33), rep('[68,100]', 33)))
 df <- data.frame(first = -49:150)
 df <- mb$run(df, 1)
 expect_equal(df[[1]],
   factor(c(rep('[ 1, 35)', 50), as.character(expected_discretized_column), rep('[68,100]', 50)),
          levels = levels(df[[1]])))
})


test_that("it correctly assigns to infinity bins", {
 df <- data.frame(first = c(rep(-Inf,10),1:80,rep(Inf,10)))
 mb <- discretizer()
 df <- mb$run(df, 1, granularity = 3)

 # test prediction
 expected_discretized_column <-
   factor(c(rep(0, 34), rep(1, 33), rep(2, 33)),labels = c('[-Inf, 25)','[  25, 58)','[  58,Inf]'))

 df <- data.frame(first = c(-Inf,seq(-9999,-9991),1:80,seq(9991,9999),Inf))
 df <- mb$run(df, 1)
 expect_equal(df[[1]], expected_discretized_column)
})

test_that("it supports up to 8 digits in discretization levels", {
 df <- data.frame(first = 1:100 / 4000000)
 mb <- discretizer()
 df <- mb$run(df, 1)
 expected_discretized_column <-
   factor(c(rep("[0.00000025,0.00000875)", 34), rep("[0.00000875,0.00001700)", 33),
            rep("[0.00001700,0.00002500]", 33)))
 expect_equal(df[[1]], expected_discretized_column)

 # test prediction
 df <- data.frame(first = (1:100 / 4000000)[1:50])
 df <- mb$run(df, 1)
})

test_that("Discretizer Successfully Interacts with Truncators to Truncate Extreme values ", {
 xmin <- -83.892
 xmax <- 16.108

 x.train <- seq(xmin,xmax,length.out=100)
 df1 <- data.frame(x=x.train)

 x.predict <- c(xmin,mean(c(xmin,xmax)),xmax)
 df2 <- data.frame(x=x.predict)

 mbTrunc <- truncator()
 mbDisc  <- discretizer()

 df1t <- mbTrunc$run(df1)
 df1d <- mbDisc$run(df1, 1, lower_count_bound=0)

 df2t <- mbTrunc$run(df2)
 df2d <- mbDisc$run(df2, 1, lower_count_bound=0)

 expect_equal(sum(df2d[, 1] == "Missing"), 0)
})

test_that("Discretizer can handle unforseen Factor Levels", {
 xmin <- -83.892
 xmax <- 16.108

 x.train <- seq(xmin, xmax, length.out = 100)
 df1 <- data.frame(x = x.train)

 x.predict <- c(xmin, mean(c(xmin, xmax)), xmax)
 df2 <- data.frame(x = x.predict)

 mbDisc <- discretizer()

 df1 <- mbDisc$run(df1, 1, lower_count_bound=0)
 df2 <- mbDisc$run(df2, 1, lower_count_bound=0)


 # check that none of the outputs are missing in discretizer
 expect_equal(sum(df1[, 1] == "Missing"),0)
 expect_equal(sum(df2[, 1] == "Missing"),0)
})

test_that("Within Discretizer It Doesn't Contain Factor Gaps", {
 iris2 <- iris
 iris2[100:nrow(iris2), 1] <- 0

 mb <- discretizer()
 iris2 <- mb$run(iris2, 4, mode_freq_threshold = .1,granularity = 3)

 # test prediction
 iris2 <- mb$run(iris, 4)
 expect_equal(iris2[, -4], iris[, -4])
 expect_equal(iris2[, 4], iris_discretized[, 4])
})

