#' @include mungebit_template.R
NULL

#' Drop variables.
#'
#' @name drop_variables
#' @export
NULL

drop_variables <- mungebit_template({
  column_transformation <- TRUE

  # Drop variables from a dataset.
  train <- function(x) {
    input$dropped <- TRUE
    NULL
  }

  predict <- function(x) {
    if (isTRUE(input$dropped)) {
      NULL
    } else {
      x
    }
  }
})

