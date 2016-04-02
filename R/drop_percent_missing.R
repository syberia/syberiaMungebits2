#' @include mungebit_template.R
NULL

#' Drop columns with over a certain percent missing.
#'
#' @param x any. The column to screen.
#' @param threshold numeric. The percent (e.g., 0.5) below which to remove
#'   columns that have at least that percent missing. The default is 0.8.
#' @param missing_level character. A string representing a missing
#'   value in a factor. This will only be used when processing
#'   character and factor columns.
#' @name drop_percent_missing
#' @export
NULL

drop_percent_missing <- mungebit_template({
  column_transformation <- TRUE

  train <- function(x, threshold = 0.8, missing_level = "Missing") {
    if (is.character(x) || is.factor(x)) {
      percent_missing <- mean(is.na(x) | x == "Missing")
    } else {
      percent_missing <- mean(is.na(x))
    }

    if (percent_missing < threshold) {
      input$drop <- FALSE
      x
    } else {
      input$drop <- TRUE
      NULL
    }
  }

  predict <- function(x, ...) {
    if (input$drop) {
      NULL
    } else {
      x
    }
  }
})

