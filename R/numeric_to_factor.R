#' numeric to factor helper function
#' 
#' TODO: Make this faster
#' 
#' @param num a numeric atomic vector. Will be restored to a factor variable.
#' @param levs a character vector of levels. 
#' @param na.to.missing a logical. Whether to convert NAs to a "Missing" level.
#' @keywords internal
numeric_to_factor <- function(num, levs, na.to.missing = TRUE) {
  res <- .Call("syberiaMungebits2_numeric_to_factor_cpp",
         num, levs, na.to.missing, PACKAGE = "syberiaMungebits2")
  if (na.to.missing && "Missing" %in% res) levs <- union(levs, "Missing")
  factor(res, levs) 
}


