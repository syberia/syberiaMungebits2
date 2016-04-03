#' Replaces a variable in the dataframe.
#'
#' @name replace_variable
#' @param df dataframe. The dataframe to modify.
#' @param fun function. The function to run to convert the input into the new input.  Whichever argument is passed first to the function is the variable that will be replaced.
#' @return Does not return anything, but modifies-in-place the dataframe to replace that variable.
#' @author Peter Hurford
#' @examples
#' # Repaces Sepal.Width to add 1 to each datapoint.
#' replace_variable(iris, function(Sepal.Width) { Sepal.Width + 1 })
#'
#' # Repaces Sepal.Length to add the corresponding Sepal.Width to each datapoint for Sepal.Length.
#' replace_variable(iris, function(Sepal.Length, Sepal.Width) { Sepal.Width + Sepal.Length })
#'
#' # Usage in a Syberia file
#' list("Replace Sepal.Length with Sepal.Length + 1" = list(
#'  replace_variable,
#'  function(Sepal.Length) Sepal.Length + 1
#' ))
#'
#' @export
NULL

replace_variable <- mungebit_template({
  train <- predict <- function(df, fun) {
    new_variable()$run(df, fun, as.character(names(formals(fun))[[1]]))
  }
})

