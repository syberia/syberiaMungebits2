#' @include mungebit_template.R
NULL

#' remove outliers based upon a z_score threshold z_score=> (x - mean(x) / sd(x) 
#'
#' @name remove_outliers
#' @param x an numeric vector
#' @param threshold a z_score threshold from which the absolute value of a z_score above this threshold will be set to NA 
#' @export
NULL

remove_outliers <- mungebit_template({
  column_transformation <- TRUE

  train <- predict <- function(x, threshold = 3) {
    if (!is.element("removed_outliers", names(input))) {
      input$mean <- mean(x, na.rm = TRUE)
      input$sd <- sd(x, na.rm = TRUE) 
      input$removed_outliers <- TRUE # not sure if another mungebit has a mean namespace 
    }
    x[abs((x - input$mean) / input$sd) > threshold] <- NA
    x
  }
})

