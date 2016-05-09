#' @include mungebit_template.R
NULL

#' Replace arbitrary values in your data
#' 
#' @name value_replacer
#' @param x an atomic vector. The column to transform.
#' @param values_map a list. If named, the names get replaced w/ the values.
#'    Otherwise, the list elements are lists of 2 elements, the first of
#'    which are the values to be replaced and the second the replacement value.
#' @return the replaced column
#' @examples
#' \dontrun{
#' # replace A, B, D with 1 and NA with 0.
#' value_replacer()$run(c("A", "B", NA, "D"), list(list(c("A","B","D"), 1), list(NA, 0)))
#' value_replacer()$run(c("A", "B", NA, "D"), list(A = 1, B = 1, D = 1, list(NA, 0)))
#' }
NULL

value_replacer <- mungebit_template({
  column_transformation <- TRUE

  train <- predict <- function(x, values_map) { 
    replaced <- x
    is_factor <- is.factor(replaced)
    if (is_factor) replaced <- as.character(replaced)
    unnamed_indices <- names(values_map) == ""
    if (is.null(unnamed_indices) || length(unnamed_indices) == 0)
      unnamed_indices <- TRUE
    if (is_factor) { 
      if (!is.element("levels", names(input))) {
        rep_levels_unnamed <- c(recursive = TRUE, lapply(values_map[unnamed_indices], function(value_map) value_map[[2]])) 
        rep_levels_named <- c(recursive = TRUE, lapply(names(values_map)[!unnamed_indices], function(name) values_map[[name]])) 
        rep_levels <- unique(c(rep_levels_unnamed, rep_levels_named))
      } else rep_levels <- input$levels
    }
    for (value_map in values_map[unnamed_indices]) {
      replaced[x %in% value_map[[1]]] <-
        if (is_factor) as.character(value_map[[2]]) else value_map[[2]]
    }
    for (name in names(values_map)[!unnamed_indices]) {
      replaced[x == name] <- 
        if (is_factor) as.character(values_map[[name]]) else values_map[[name]]
    }
    if (is_factor) {
      factor(replaced, levels = union(unique(replaced), rep_levels))
    } else {
      replaced
    }
  }
})

