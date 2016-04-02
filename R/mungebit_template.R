#' Create a mungebit template from an expression.
#'
#' The format here is similar to the format required for specifying
#' a mungebit as a resource in a
#' \href{http://github.com/syberia/modeling.sy}{syberia modeling project}.
#' 
#' The returned mungebit template is a zero-argument function that
#' produces an untrained mungebit given by the expression.
#'
#' The expression passed in can specify local variables \code{train} or
#' \code{predict} indicating the train and predict function for the
#' mungebit, respectively, as well as set \code{column_transformation} 
#' to \code{TRUE} to specify they are column transformations.
#'
#' @param expression expression. An R expression containing potentially
#'   local variables \code{train} or \code{predict} indicating the train
#'   and predict function for the mungebit to be created.
#' @return A zero-argument function that produces the mungebit given by
#'   the expression.
mungebit_template <- function(expression) {
  inputs <- new.env(parent = baseenv())
  eval(substitute(expression), envir = inputs)
  
  train_function <- inputs$train
  environment(train_function) <- globalenv()
  predict_function <- inputs$predict
  environment(predict_function) <- globalenv()

  if (isTRUE(inputs$column_transformation)) {
    train_function   <- mungebits2::column_transformation(train_function)
    predict_function <- mungebits2::column_transformation(predict_function)
  }

  generator <- function() { }
  environment(generator) <- list2env(
    list(train_function = train_function, predict_function = predict_function),
    parent = globalenv()
  )
  body(generator) <- quote({
    mungebits2::mungebit$new(train_function, predict_function)
  })
  generator
}

