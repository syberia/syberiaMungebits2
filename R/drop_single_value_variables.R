#' @include mungebit_template.R
NULL

#' Drop features with only one non-NA value.
#'
#' @name drop_single_value_variables
#' @export
NULL

drop_single_value_variables <- mungebit_template({
  column_transformation <- TRUE

  # Drop variables with only one unique value.
  train <- function(x) {
    if (length(x) == 0 || (tmp <- length(unique(x))) == 1 ||
        (tmp == 2 && any(is.na(x)))) {
      input$dropped <<- TRUE
      NULL
    } else {
      input$dropped <<- FALSE
      x 
    }
  }

  predict <- function(x) {
    if (isTRUE(input$dropped)) {
      NULL 
    } else {
      x
    }
  }
})
 
