#' @include mungebit_template.R
NULL

#' Discretizer function
#'
#' @name discretizer
#' @param column an atomic vector. The variable to discretize.
#' @param name character. Name of the colum.
#' @param granularity an integer. The suggested number of levels.
#' @param mode_freq_threshold a real value between 0 and 1. If the mode of the
#'    variable exceeds this value and is greater than
#'    \code{mode_ratio_threshold} (see next parameter) times the next greatest
#'    mode (i.e., the ratio of the value occuring most often over the value
#'    occuring second most often is over \code{mode_ratio_threshold}) then
#'    the variable will be attempted to be discretized in manner as to make
#'    the mode its own bucket. (so if the mode is 5, we'd want, e.g., [2,4),
#'    5, and (5, 7]).
#' @param mode_ratio_threshold a real value. See the \code{mode_freq_threshold}
#'    parameter.
#' @param category_range The number of levels to consider when the
#'    discretization procedure descrized in the \code{mode_freq_threshold}
#'    parameter is employed. The default is \code{min(granularity, 20):20}.
#' @param lower_count_bound an integer. Variables with less than or equal to
#'    this many unique values will not get discretized. Default is
#'    \code{granularity}.
#' @param upper_count_bound an integer. Variables with more than or equal to
#'    this many unique values will not get discretized. Default is
#'    \code{granularity}.
#' @param missing_level character. Any values that were \code{NA} prior to
#'    discretization will be replaced with this level. If set to \code{NULL},
#'    then the \code{NA}s will remain. The default is \code{"Missing"}.
#' @param ... additional arguments to pass to arules::discretize.
#' @export
NULL

discretizer_fn <- function(column, name,
    granularity = 3, mode_freq_threshold = 0.15, mode_ratio_threshold = 1.5,
    category_range = min(granularity, 20):20, lower_count_bound = granularity,
    upper_count_bound = NULL, missing_level = 'Missing', ...) {

  old_options <- options(digits = syberiaMungebits2:::MAX_DISCRETIZATION_DIGITS,
                         scipen = syberiaMungebits2:::MAX_DISCRETIZATION_DIGITS)
  on.exit(options(old_options))

  colname <- name
  if (!is.numeric(column)) return(column)

  previous_missing_values <- is.na(column)

  # Some caching optimizations
  uniques <- syberiaMungebits2:::present_uniques(column)
  if (!is.null(lower_count_bound) && length(uniques) <= lower_count_bound) return(column)
  if (!is.null(upper_count_bound) && length(uniques) >= upper_count_bound) return(column)
  variable_freqs <- syberiaMungebits2:::freqs(column, uniques)
  mode_value <- syberiaMungebits2:::Mode(column, uniques, variable_freqs)

  if (mean(column == mode_value, na.rm = TRUE) > mode_freq_threshold &&
      syberiaMungebits2:::mode_ratio(column, variable_freqs) > mode_ratio_threshold) {
    mode_corrected <- FALSE
    if (!is.null(category_range)) {
      for(i in category_range) {
        discretized_column <- try(suppressWarnings(arules::discretize(column,
          digits = syberiaMungebits2:::MAX_DISCRETIZATION_DIGITS, method = 'frequency',
          categories = i, ...)))
        if (inherits(discretized_column, 'try-error')) next
        trimmed_levels <- gsub('^ *| *$', '', levels(discretized_column))
        if (mode_value %in% suppressWarnings(as.numeric(trimmed_levels))) {
          mode_corrected <- TRUE
          break
        }
      }
      if (!mode_corrected) {
        # TODO: Turn into binary variable

        warning(paste0("Mode of variable '", colname ,"' is above ", 100 * mode_freq_threshold, "% ",
                "and/or mode ratio is above ", mode_ratio_threshold, " and no number of buckets between ",
                min(category_range), " and ", max(category_range), " fixes the problem. May want to ",
                "discretize manually"))
      }
    }
    if (!mode_corrected) {
      discretized_column <- try(arules::discretize(column,
        digits = syberiaMungebits2:::MAX_DISCRETIZATION_DIGITS,
        method = 'frequency', categories = granularity, ...))
      }
  } else {
    discretized_column <- try(arules::discretize(column,
      digits = syberiaMungebits2:::MAX_DISCRETIZATION_DIGITS,
      method = 'frequency', categories = granularity, ...))
  }

  # Handle weird discretizer bug
  # TODO: DO THIS IN RESTORE LEVELS
  if (is.list(discretized_column))
    discretized_column <- sapply(discretized_column, function(column) column[[1]])

  if (inherits(discretized_column, 'try-error'))
    stop(paste0("Problem discretizing variable '", colname, "': ", discretized_column))
  else {
    # Store the levels for restoring during prediction
    if (!is.null(missing_level) && sum(previous_missing_values) > 0) {
      discretized_column <- factor(discretized_column,
        levels = c(levels(discretized_column), missing_level))
      discretized_column[previous_missing_values] <- missing_level
    }
    input$levels <- levels(discretized_column)
    discretized_column
  }
}

restore_levels_fn <- function(column, missing_level = 'Missing', ...) {
  if (!is.element("levels", names(input))) {
    column
  } else {
    previous_missing_values <- is.na(column)
    col <- syberiaMungebits2:::numeric_to_factor(column, input$levels,
                                                 na.to.missing = FALSE)
    if (!is.null(missing_level)) {
      factor(ifelse(previous_missing_values,
            as.character(missing_level), as.character(col)), levels = levels(col))
    } else { col }
  }
}

discretizer <- mungebit_template({
  column_transformation <- TRUE

  train <- predict <- function(column, name, verbose = FALSE, ...) {
    fn <- if (trained) syberiaMungebits2:::restore_levels_fn
          else syberiaMungebits2:::discretizer_fn
    environment(fn) <- environment() # Make input available
    if (verbose) {
      fn(column, ..., name = name)
    } else  {
      suppressMessages(suppressWarnings(fn(column, ..., name = name)))
    }
  }
})

# Some helper functions
mode_ratio <- function(variable,
                       variable_freqs = syberiaMungebits2:::freqs(variable)) {
  if (length(variable_freqs) < 2) {
    stop("Cannot compute mode ratio of variable with ",
         "less than 2 unique values.")
  }
  variable_freqs[order(-variable_freqs)[1]] / variable_freqs[order(-variable_freqs)[2]]
}

# http://stackoverflow.com/questions/2547402/standard-library-function-in-r-for-finding-the-mode
Mode <- function(variable,
                 uniques = syberiaMungebits2:::present_uniques(variable),
                 variable_freqs = syberiaMungebits:::freqs(variable, uniques)) {
  uniques[which.max(variable_freqs)]
}

present_uniques <- function(variable) {
  unique(variable[!is.na(variable)])
}

freqs <- function(variable,
                  uniques = syberiaMungebits2:::present_uniques(variable)) {
  tabulate(match(variable, uniques))
}

MAX_DISCRETIZATION_DIGITS <- 8

