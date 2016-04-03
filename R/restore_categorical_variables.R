#' @include mungebit_template.R
NULL

#' Restores categorical variables, allowing you to predict.
#'
#' @name restore_categorical_variables
#' @param x. The column to restore.
#' @export
NULL

restore_categorical_variables <- mungebit_template({
  column_transformation <- TRUE

  train <- predict <- function(x) {
    if ("levels" %in% names(input)) {
      factor(x, levels = input$levels)
    } else {
      input$levels <- levels(x); x
    }
  }
})

