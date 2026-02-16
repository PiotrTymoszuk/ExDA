# Re-created generics

# Variance --------

#' Calculate descriptive statistics.
#'
#' @description
#' Calculates variance, standard deviation, kurtosis, skewness,
#' Gini coefficient, geometric means, and harmonic mean of an object.
#'
#' @details
#' Please refer to:
#'
#' * \code{\link[stats]{var}} and \code{\link[stats]{sd}}
#' for details of variance and standard deviation
#'
#' * \code{\link[moments]{kurtosis}} and \code{\link[moments]{skewness}}
#' for details of kurtosis and skewness
#'
#' * \code{\link[microViz]{Gini}}, \code{\link[microViz]{Hmean}},
#' and \code{\link[microViz]{Gmean}} doe details oF Gini coefficients,
#' geometric, and harmonic means.
#'
#'
#' @param x an object.
#' @param ... extra arguments passed to methods.
#'
#' @return a numeric value.
#'
#' @md
#' @export

  var <- function(x, ...) UseMethod("var")

#' @rdname var
#' @export

  var.default <- function(x, ...) stats::var(x, ...)

#' @rdname var
#' @export

  sd <- function(x, ...) UseMethod("sd")

#' @rdname var
#' @export

  sd.default <- function(x, ...) stats::sd(x, ...)

#' @rdname var
#' @export

  kurtosis <- function(x, ...) UseMethod("kurtosis")

#' @rdname var
#' @export

  kurtosis.default <- function(x, ...) moments::kurtosis(x, ...)

#' @rdname var
#' @export

  skewness <- function(x, ...) UseMethod("skewness")

#' @rdname var
#' @export

  skewness.default <- function(x, ...) moments::skewness(x, ...)

#' @rdname var
#' @export

  gini <- function(x, ...) UseMethod("gini")

#' @rdname var
#' @export

  gini.default <- function(x, ...) Gini(x, ...)

#' @rdname var
#' @export

  gmean <- function(x, ...) UseMethod("gmean")

#' @rdname var
#' @export

  gmean.default <- function(x, ...) Gmean(x, ...)

#' @rdname var
#' @export

  hmean <- function(x, ...) UseMethod("hmean")

#' @rdname var
#' @export

  hmean.default <- function(x, ...) Hmean(x, ...)

# END -------
