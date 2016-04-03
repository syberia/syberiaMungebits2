#' @include mungebit_template.R
NULL

#' Order a dataframe by a column
#'
#' @name orderer
#' @param dataframe a data.frame to re-order
#' @param column_name the column by which to order
#' @export
NULL

orderer <- mungebit_template({
  train <- predict <- function(dataframe, column_name) {
    dataframe[order(dataframe[[column_name]]), ]
  }
})

