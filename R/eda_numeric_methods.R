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
#' * \code{\link[microViz]{kurtosis}} and \code{\link[microViz]{skewness}}
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
                 fun = microViz::kurtosis,
                 stat_name = "kurtosis",
                 plain = plain)

  }

#' @rdname quantile.eda
#' @export

  skewness.eda <- function(x, plain = FALSE, ...) {

    stat_extract(x,
                 fun = microViz::skewness,
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

# Normalization ----------

#' Scaling and centering `eda` objects.
#'
#' @description
#' Scaling and centering of an `eda` object as described
#' in \code{\link[base]{scale}}.
#' For factor objects, the unmodified object is returned with a warning.
#'
#' @details
#' Unlike the generic \code{\link[base]{scale}} function, it returns an
#' \code{\link{eda}} object.
#' Any `NA` values are silently removed.
#'
#' @return a scaled, centered numeric \code{\link{eda}} object
#'
#' @param x an \code{\link{eda}} object
#' @param center either a logical value or numeric-alike vector of length
#' equal to the \code{\link{eda}} object.
#' See \code{\link[base]{scale}} for details.
#' @param scale either a logical value or numeric-alike vector of length
#' equal to the \code{\link{eda}} object.
#' See \code{\link[base]{scale}} for details.
#'
#' @export

  scale.eda <- function(x,
                        center = TRUE,
                        scale = TRUE) {

    ## entry control -------

    stopifnot(is_eda(x))

    if(is.factor(x)) {

      warning("Scaling is not possible for factor objects.", call. = FALSE)

      return(x)

    }

    ## scaling -------

    x <- x[!is.na(x)]

    eda(scale(x,
              center = center,
              scale = scale)[, 1])


  }

# Cutting ---------

#' Cut an `eda` object.
#'
#' @description
#' Converts a numeric-type EDA object to a factor-type EDA
#' by cutting the numeric values with the defined cutoffs.
#' Factor-type EDAs are returned without any changes.
#'
#' @param x an EDA object, created by \code{\link{eda}}.
#' @param type indicates how to generate cutoffs/breaks. If 'custom', the user-specified
#' breaks are used, otherwise the indicated statistic values are used including the
#' minimum and maximum values of the EDA object. Defaults to quartile.
#' @param breaks valid only for type 'custom'. Either a numeric vector of two or more unique
#' cut points or a single number (greater than or equal to 2)
#' giving the number of intervals into which x is to be cut.
#' @param labels valid only for type 'custom'. For the levels of the resulting category.
#' By default, labels are constructed using "(a,b]" interval notation.
#' If `labels = FALSE`, simple integer codes are returned instead of a factor.
#' @param include.lowest valid only for type 'custom'.
#' Logical, indicating if values equal to the lowest (or highest, for `right = FALSE`)
#' ‘breaks’ value should be included.
#' @param right valid only for type 'custom'. Logical, indicating if the intervals should be
#' closed on the right (and open on the left) or vice versa.
#' @param default_labels logical, should default labels be used for non-custom breaks? #
#' These are: Q1 to Q4 for quartiles and H1/H2 for mean and median.
#' @param ... extra arguments passed to \code{\link[base]{cut}}.
#'
#' @details a wrapper around \code{\link[base]{cut}}.
#' `NA` values are silently removed.
#'
#' @return a factor `eda` object.
#' @export

  cut.eda <- function(x,
                      type = c("quartile", "mean", "median", "tertile", "custom"),
                      breaks = NULL,
                      labels = NULL,
                      include.lowest = TRUE,
                      right = TRUE,
                      default_labels = TRUE, ...) {

    ## entry control ------

    stopifnot(is_eda(x))

    type <- match.arg(type[1],
                      c("quartile", "mean", "median", "tertile", "custom"))

    stopifnot(is.logical(include.lowest))
    stopifnot(is.logical(default_labels))

    if(is.factor(x)) {

      warning("No cutting possible for factors.", call. = FALSE)

      return(x)

    }

    x <- x[!is.na(x)]

    if(type != "custom") {

      breaks <-
        switch(type,
               quartile = c(-Inf,
                            quantile(x, c(0.25, 0.5, 0.75)),
                            Inf),
               mean = c(-Inf,
                        mean(x),
                        Inf),
               median = c(-Inf,
                          median(x),
                          Inf),
               tertile = c(-Inf,
                           quantile(x, c(1/3, 2/3)),
                           Inf))

      if(default_labels) {

        labels <- switch(type,
                         quartile = c("Q1", "Q2", "Q3", "Q4"),
                         mean = c("H1", "H2"),
                         median = c("H1", "H2"),
                         tertile = c("T1", "T2", "T3"))

      }

    }

    cut_vals <- cut(x,
                    breaks = breaks,
                    labels = labels,
                    include.lowest = include.lowest,
                    right = right, ...)

    eda(cut_vals)

  }

# END ---------
