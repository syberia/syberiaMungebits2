#' Common feature engineering preparation using mungebits2.
#'
#' @name syberiaMungebits2
#' @import mungebits2
#' @useDynLib syberiaMungebits2
#' @docType package
NULL

#' Whether or not a function is a mungebit generator.
#'
#' Mungebit generators are arity-0 functions that generate
#' a new mungebit when called.
#'
#' @param obj ANY. An R object to test.
#' @examples
#' @return TRUE or FALSE according as \code{obj} has the
#'   class \code{"mungebit_generator"}.
is.mungebit_generator <- function(obj) {
  methods::is(obj, "mungebit_generator")
}

#' All mungebit generators exported by the syberiaMungebits2 package.
#'
#' @return A named list of mungebit generators exposed by this package, invisibly.
#' @export
mungebit_generators <- function() {
  invisible(Filter(is.mungebit_generator, as.list(getNamespace("syberiaMungebits2"))))
}

