# Numeric methods: mean, median, variance and SD ------

#' Descriptive statistics of `eda` objects.
#'
#' @description
#' Gets quantiles, mean, median, variance, standard deviation, ,  of a numeric ``eda object.
#' For factors `NULL` is returned and a warning generated.
#' `NA` values are removed before computation.
#'
#' @details
#' Please refer to:
#'
#' * \code{\link[stats]{quantile}}, \code{\link[stats]{var}},
#' and \code{\link[stats]{sd}}
#' for details of variance and standard deviation
#'
#' * \code{\link[moments]{kurtosis}} and \code{\link[moments]{skewness}}
#' for details of kurtosis and skewness
#'
#' * \code{\link[microViz]{Gini}}, \code{\link[microViz]{Hmean}},
#' and \code{\link[microViz]{Gmean}} doe details of Gini coefficients,
#' geometric, and harmonic means.
#'
#' @param x an \code{\link{eda}} object.
#' @param probs numeric vector of probabilities with values in the 0 to 1 range.
#' @param na.rm logical, should `NA` values be removed prior to computation?
#' Required for compatibility with the `median()` generic function and ignored.
#' @param plain logical, should the output be coerced to a single vector?
#' @param ... additional arguments passed to methods.
#'
#' @return a tibble or vector (if `plain = TRUE`) with the requested statistics
#'
#' @export

  quantile.eda <- function(x,
                           probs = c(0, 0.25, 0.5, 0.75, 1),
                           plain = FALSE, ...) {

    stopifnot(is_eda(x))
    stopifnot(is.numeric(probs))
    stopifnot(is.logical(plain))

    if(is.factor(x)) {

      warning('No quantiles available for factorss.', call. = FALSE)

      return(NULL)

    }

    x <- x[!is.na(x)]

    quants <- quantile(x, probs = probs, names = TRUE)

    if(plain) return(quants)

    value <- NULL

    tibble(quantile = names(quants),
           value = quants)

  }

#' @rdname quantile.eda
#' @export

  mean.eda <- function(x, plain = FALSE, ...) {

   stat_extract(x,
                fun = mean,
                stat_name = "mean",
                plain = plain)

  }

#' @rdname quantile.eda
#' @export

  median.eda <- function(x, na.rm = TRUE, plain = FALSE, ...) {

    stat_extract(x,
                 fun = median,
                 stat_name = "median",
                 plain = plain)

  }

#' @rdname quantile.eda
#' @export

  var.eda <- function(x, plain = FALSE, ...) {

    stat_extract(x,
                 fun = stats::var,
                 stat_name = "variance",
                 plain = plain)

  }

#' @rdname quantile.eda
#' @export

  sd.eda <- function(x, plain = FALSE, ...) {

    stat_extract(x,
                 fun = stats::sd,
                 stat_name = "sd",
                 plain = plain)

  }


#' @rdname quantile.eda
#' @export

  kurtosis.eda <- function(x, plain = FALSE, ...) {

    stat_extract(x,
                 fun = moments::kurtosis,
                 stat_name = "kurtosis",
                 plain = plain)

  }

#' @rdname quantile.eda
#' @export

  skewness.eda <- function(x, plain = FALSE, ...) {

    stat_extract(x,
                 fun = moments::skewness,
                 stat_name = "skewness",
                 plain = plain)

  }

#' @rdname quantile.eda
#' @export

  gini.eda <- function(x, plain = FALSE, ...) {

    if(is.numeric(na.omit(x)) & any(na.omit(x) < 0)) {

      warning("Gini coefficient can't be calculated for negative values.",
              call. = FALSE)

      return(stat_extract(x,
                          fun = function(x) NA_real_,
                          stat_name = "Gini",
                          plain = plain))

    }

    stat_extract(x,
                 fun = Gini,
                 stat_name = "Gini",
                 plain = plain)

  }

#' @rdname quantile.eda
#' @export

  gmean.eda <- function(x, plain = FALSE, ...) {

    stat_extract(x,
                 fun = Gmean,
                 stat_name = "geometric_mean",
                 plain = plain)

  }

#' @rdname quantile.eda
#' @export

  hmean.eda <- function(x, plain = FALSE, ...) {

    stat_extract(x,
                 fun = Hmean,
                 stat_name = "harmonic_mean",
                 plain = plain)

  }

# END ---------
