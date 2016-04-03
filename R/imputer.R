#' Impute a column by mean
#'
#' @name imputer
#' @param x an atomic vector.
#' @export
NULL

imputer <- mungebit_template({
  column_transformation <- TRUE

  train <- predict <- function(x) {
    if (!is.element("replacement", names(input))) {
      if (is.numeric(x)) {
        input$replacement <- median(x, na.rm=TRUE)
      } else {
        tt <- table(x)
        input$replacement <- names(tt)[which.max(tt)]
      }
      if (length(input$replacement) == 0) {
        input$replacement <- NA
      }
    }

    if (is.factor(x)) {
      levels(x) <- union(levels(x), input$replacement)
    }
    x[is.na(x)] <- input$replacement
    x
  }
})

