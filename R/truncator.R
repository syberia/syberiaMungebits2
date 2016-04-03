#' Constrain a numeric column to the range [min,max] of the training set 
#'
#' @name truncator
#' @param x an atomic vector
#' @param digits how many decimal places you would like your numeric number to contain after its been truncated
#' @export
NULL

truncator <- mungebit_template({
  column_transformation <- TRUE

  train <- predict <- function(x) {
    stopifnot(is.numeric(x))
    if (!("min" %in% names(input))) {
      input$min <- min(x, na.rm = TRUE)
      input$max <- max(x, na.rm = TRUE)
    } else { 
      x[x <= input$min] <- syberiaMungebits2:::trunc.dig(input$min,
        digits = syberiaMungebits2:::MAX_DISCRETIZATION_DIGITS - 1)
      x[x >= input$max] <- syberiaMungebits2:::trunc.dig(input$max,
        digits = syberiaMungebits2:::MAX_DISCRETIZATION_DIGITS - 1)
    }
    x
  }
})

# Truncates Decimals after specified numers of Digits. 
#' i.e trunc.dig(5.732 , digits = 1) => 5.7 
trunc.dig <- function(x, digits = 1) {
  trunc(x * 10 ^ digits) / 10 ^ digits
}

