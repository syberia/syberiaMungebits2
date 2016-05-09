#' @include mungebit_template.R
NULL

#' Rename columns in a dataframe
#'
#' @name renamer
#' @param dataframe a data.frame.
#' @param replacements a list of name replacements with keys old names and
#'    respective values new names.
#' @export
#' @examples \dontrun{
#'   renamer()$run(iris, list('Sepal.Length' = 'seplen', 'Sepal.Width' = 'sepwid'))
#' }
NULL

renamer <- mungebit_template({
  train <- predict <- function(dataframe, replacements) {
    colnames(dataframe) <-
      Reduce(function(str, old_value) {
        str[colnames(dataframe) == old_value] <- replacements[[old_value]]
        str
      }, names(replacements), colnames(dataframe))
    dataframe
  }
})

