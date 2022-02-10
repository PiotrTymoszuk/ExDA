# Numeric methods: mean, median, variance and SD ------

#' Quantiles of an EDA object.
#'
#' @description Gets quantiles of a numeric-type EDA object. For factor-type ones
#' NULL is returned and a warning generated. NAs are removed before computation.
#' @param x an EDA object, created by \code{\link{eda}}.
#' @param probs numeric vector of probabilities with values in [0,1].
#' @param plain logical, should the output be coerced to a single vector?
#' @return a tibble or vector with the requested quantile values.
#' @export

  quantile.eda <- function(x, probs = c(0, 0.25, 0.5, 0.75, 1), plain = FALSE) {

    stopifnot(is_eda(x))

    if(x$type == 'factor') {

      warning('No quantiles available for factor-type EDA objects.', call. = FALSE)

      return(NULL)

    }

    quants <- stats::quantile(x$value,
                              probs = probs,
                              na.rm = TRUE,
                              names = TRUE)

    if(plain) {

      quants

    } else {

      tibble::tibble(quantile = names(quants),
                     value = quants)

    }

  }

#' Mean of an EDA object.
#'
#' @description Gets arithmetic mean of a numeric-type EDA object. For factor-type ones
#' NULL is returned and a warning generated. NAs are removed before computation.
#' @param x an EDA object, created by \code{\link{eda}}.
#' @param plain logical, should the output be coerced to a single vector?
#' @return a tibble or a numeric value.
#' @export

  mean.eda <- function(x, plain = FALSE) {

    stopifnot(is_eda(x))

    exda:::stat_extract(x,
                        fun = mean,
                        stat_name = 'mean',
                        plain = plain,
                        na.rm = TRUE)

  }

#' Median of an EDA object.
#'
#' @description Gets median of a numeric-type EDA object. For factor-type ones
#' NULL is returned and a warning generated. NAs are removed before computation.
#' @param x an EDA object, created by \code{\link{eda}}.
#' @param plain logical, should the output be coerced to a single vector?
#' @return a tibble or a numeric value.
#' @export

  median.eda <- function(x, plain = FALSE) {

    stopifnot(is_eda(x))

    exda:::stat_extract(x,
                        fun = stats::median,
                        stat_name = 'median',
                        plain = plain,
                        na.rm = TRUE)

  }

#' Default variance calculation method.
#'
#' @description A wrapper around \code{\link[stats]{var}}
#' @param x an object.
#' @param ... extra arguments passed to methods.
#' @return a numeric.
#' @export

  var.default <- function(x, ...) {

    stats::var(x, ...)

  }

#' Variance of an EDA object.
#'
#' @description Gets variance of a numeric-type EDA object. For factor-type ones
#' NULL is returned and a warning generated. NAs are removed before computation.
#' @param x an EDA object, created by \code{\link{eda}}.
#' @param plain logical, should the output be coerced to a single vector?
#' @return a tibble or a numeric value.
#' @export

  var.eda <- function(x, plain = FALSE) {

    stopifnot(is_eda(x))

    exda:::stat_extract(x,
                        fun = stats::var,
                        stat_name = 'variance',
                        plain = plain,
                        na.rm = TRUE)

  }

#' Default standard deviation calculation method.
#'
#' @description A wrapper around \code{\link[stats]{sd}}
#' @param x an object.
#' @param ... extra arguments passed to methods.
#' @return a numeric.
#' @export

  sd.default <- function(x, ...) {

    stats::sd(x, ...)

  }

#' Standard deviance of an EDA object.
#'
#' @description Gets standard deviation of a numeric-type EDA object. For factor-type ones
#' NULL is returned and a warning generated. NAs are removed before computation.
#' @param x an EDA object, created by \code{\link{eda}}.
#' @param plain logical, should the output be coerced to a single vector?
#' @return a tibble or a numeric value.
#' @export

  sd.eda <- function(x, plain = FALSE) {

    stopifnot(is_eda(x))

    exda:::stat_extract(x,
                        fun = stats::sd,
                        stat_name = 'sd',
                        plain = plain,
                        na.rm = TRUE)

  }

# Skewness and kurtosis -----

#' Default kurtosis calculation method.
#'
#' @description A wrapper around \code{\link[moments]{kurtosis}}
#' @param x an object.
#' @param ... extra arguments passed to methods.
#' @return a numeric.
#' @export

  kurtosis.default <- function(x, ...) {

    moments::kurtosis(x, ...)

  }

#' Default skewness calculation method.
#'
#' @description A wrapper around \code{\link[moments]{skewness}}
#' @param x an object.
#' @param ... extra arguments passed to methods.
#' @return a numeric.
#' @export

  skewness.default <- function(x, ...) {

    moments::skewness(x, ...)

  }

#' Kurtosis of an EDA object.
#'
#' @description Gets kurtosis of a numeric-type EDA object. For factor-type ones
#' NULL is returned and a warning generated. NAs are removed before computation.
#' @param x an EDA object, created by \code{\link{eda}}.
#' @param plain logical, should the output be coerced to a single vector?
#' @return a tibble or a numeric value.
#' @export

  kurtosis.eda <- function(x, plain = FALSE) {

    stopifnot(is_eda(x))

    exda:::stat_extract(x,
                        fun = moments::kurtosis,
                        stat_name = 'kurtosis',
                        plain = plain,
                        na.rm = TRUE)

  }

#' Skewness of an EDA object.
#'
#' @description Gets skewness of a numeric-type EDA object. For factor-type ones
#' NULL is returned and a warning generated. NAs are removed before computation.
#' @param x an EDA object, created by \code{\link{eda}}.
#' @param plain logical, should the output be coerced to a single vector?
#' @return a tibble or a numeric value.
#' @export

  skewness.eda <- function(x, plain = FALSE) {

    stopifnot(is_eda(x))

    exda:::stat_extract(x,
                        fun = moments::skewness,
                        stat_name = 'skewness',
                        plain = plain,
                        na.rm = TRUE)

  }
