#' Generation of EDA objects.
#'
#' @description Creates a eda object from multiple possible outputs such as vectors or data frames.
#' @param x an object to be converted to EDA.
#' @param ... extra arguments passed to methods.
#' @return an object of class 'eda'.
#' @export

  eda <- function(x, ...) {

    UseMethod('eda')

  }

#' Tables objects.
#'
#' @description Converts an object to a table. A wrapper around \code{\link[base]{as.table}}.
#' @param x an object to be converted to a table.
#' @param ... extra arguments passed to methods.
#' @return an object of class 'table'.
#' @export

  as_table <- function(x, ...) {

    UseMethod('as_table')

  }

#' Counting of an object occurrence.
#'
#' @description Counts observations in each category.
#' @param x an object.
#' @param ... extra arguments passed to methods.
#' @return a tibble with the category counts.
#' @export

  count <- function (x, ..., wt = NULL, sort = FALSE, name = NULL) {

    UseMethod('count')

  }

#' Convert an object to a tibble.
#'
#' @description Converts an object to a tibble.
#' @param x an object.
#' @param ... extra arguments passed to methods.
#' @return a tibble
#' @export

  as_tibble <- function(...) {

    UseMethod('as_tibble')

  }

#' Coercion to a factor.
#'
#' @description Converts an object to a factor.
#' @param x an object to be converted to a factor.
#' @param ... extra arguments passed to methods.
#' @return an object of class 'factor'.
#' @export

  as.factor <- function(x, ...) {

    UseMethod('as.factor')

  }

#' Coercion to a factor.
#'
#' @description Converts an object to a factor.
#' @param x an object to be converted to a factor.
#' @param ... extra arguments passed to methods.
#' @return an object of class 'factor'.
#' @export

  as_factor <- function(x, ...) {

    UseMethod('as_factor')

  }

#' Coercion to a numeric.
#'
#' @description Converts an object to a numeric.
#' @param x an object to be converted to a numeric.
#' @param ... extra arguments passed to methods.
#' @return an object of class 'numeric'.
#' @export

  as_numeric <- function(x, ...) {

    UseMethod('as_numeric')

  }

#' Coercion to a vector.
#'
#' @description Converts an object to a vector.
#' @param x an object to be converted to a vector.
#' @param ... extra arguments passed to methods.
#' @return a vector.
#' @export

  as_vector <- function(x, ...) {

    UseMethod('as_vector')

  }

#' Calculate variance.
#'
#' @description Calculates variance of an object.
#' @param x an object.
#' @param ... extra arguments passed to methods.
#' @return a numeric.
#' @export

  var <- function(x, ...){

    UseMethod('var')

  }

#' Calculate standard deviation.
#'
#' @description Calculates standard deviation of an object.
#' @param x an object.
#' @param ... extra arguments passed to methods.
#' @return a numeric.
#' @export

  sd <- function(x, ...){

    UseMethod('sd')

  }

#' Calculate kurtosis.
#'
#' @description Calculates kurtosis of an object.
#' @param x an object.
#' @param ... extra arguments passed to methods.
#' @return a numeric.
#' @export

  kurtosis <- function(x, ...){

    UseMethod('kurtosis')

  }

#' Calculate skewness.
#'
#' @description Calculates skewness of an object.
#' @param x an object.
#' @param ... extra arguments passed to methods.
#' @return a numeric.
#' @export

  skewness <- function(x, ...){

    UseMethod('skewness')

  }

