# Conversion to data frame ------

#' Convert an EDA object to a data frame.
#'
#' @description converts an EDA object to a one-column data frame. The data type and levels are preserved.
#' @param eda_object an EDA object, created by \code{\link{eda}}.
#' @param newname a name of the data frame variable.
#' @return a data frame.
#' @export

  as.data.frame.eda <- function(eda_object, newname = 'variable') {

    stopifnot(all(class(eda_object) == 'eda'))

    df <- switch(eda_object$type,
                 numeric = data.frame(new_var = eda_object$value),
                 factor = data.frame(new_var = factor(eda_object$value, levels = eda_object$levels)))

    rlang::set_names(df, newname)

  }

#' Convert an EDA object to a tibble.
#'
#' @description converts an EDA object to a one-column data frame. The data type and levels are preserved.
#' @param eda_object an EDA object, created by \code{\link{eda}}.
#' @param newname a name of the data frame variable.
#' @return a data frame.
#' @export

  as_tibble.eda <- function(eda_object, newname = 'variable') {

    stopifnot(all(class(eda_object) == 'eda'))

    df <- switch(eda_object$type,
                 numeric = tibble::tibble(new_var = eda_object$value),
                 factor = tibble::tibble(new_var = factor(eda_object$value, levels = eda_object$levels)))

    rlang::set_names(df, newname)

  }

# Factor conversion -----

#' Convert to a factor.
#'
#' @description converts an object to a factor.
#' @param x an object.
#' @return a factor.
#' @export

  as.factor.default <- function(x) {

    base::as.factor(x)

  }

#' Convert numeric-type EDA to a factor.
#'
#' @description converts a numeric-type EDA object to a factor-type one.
#' @param eda_object an EDA object, created by \code{\link{eda}}.
#' @return a factor-type EDA.
#' @export

  as.factor.eda <- function(eda_object) {

    stopifnot(all(class(eda_object) == 'eda'))

    eda(factor(eda_object$value))

  }

#' Convert to a factor.
#'
#' @description converts an object to a factor.
#' @param x an object.
#' @return a factor.
#' @export

  as.factor.default <- function(x) {

    base::as.factor(x)

  }

#' Convert numeric-type EDA to a factor.
#'
#' @description converts a numeric-type EDA object to a factor-type one.
#' @param eda_object an EDA object, created by \code{\link{eda}}.
#' @return a factor-type EDA.
#' @export

  as_factor.eda <- function(eda_object) {

    as.factor(eda_object)

  }

# Re-leveling ------

#' Relevel an EDA object.
#'
#' @description Sets new levels for a factor-type EDA object.
#' Releveling of a numeric-type EDA raises an error.
#' @param eda_object an EDA object, created by \code{\link{eda}}.
#' @param newlevels a character vector with new levels.
#' @return a factor-type EDA.
#' @export

  relevel.eda <- function(eda_object, newlevels) {

    stopifnot(all(class(eda_object) == 'eda'))

    if(eda_object$type == 'numeric') {

      stop('Releveling is not available for numeric-type EDA objects.', call. = FALSE)

    }

    eda(factor(eda_object$value,
               levels = newlevels))


  }

# Numeric conversion -----

#' Convert to a numeric.
#'
#' @description converts an object to a numeric.
#' @param x an object.
#' @return a numeric.
#' @export

  as_numeric.default <- function(x, ...) {

    base::as.numeric(x, ...)

  }

#' Convert factor-type EDA to a numeric
#'
#' @description converts a factor-type EDA object to a numeric-type one.
#' @param eda_object an EDA object, created by \code{\link{eda}}.
#' @return a numeric-type EDA.
#' @export

  as_numeric.eda <- function(eda_object) {

    stopifnot(all(class(eda_object) == 'eda'))

    eda(base::as.numeric(eda_object$value))

  }

# conversion to a plain vector -----

#' Convert to a plain vector.
#'
#' @description Converts an object to a vector.
#' @param x an object to be converted to a vector.
#' @param ... extra arguments passed to methods.
#' @return a vector.
#' @export

  as_vector.default <- function(x, ...) {

    base::as.vector(x, ...)

  }

#' Convert an EDA object to a plain vector.
#'
#' @description Converts an EDA object to a vector. The data type is preserved.
#' @param eda_object an EDA object, created by \code{\link{eda}}.
#' @return a vector with the EDA values.
#' @export

  as_vector.eda <- function(eda_object) {

    stopifnot(all(class(eda_object) == 'eda'))

    eda_object$value

  }

# Cutting and scaling ----

#' Cut an EDA object.
#'
#' @description Converts a numeric-type EDA object to a factor-type EDA
#' by cutting the numeric values with the defined cutoffs. Factor-type EDAs are returned without any changes.
#' @param eda_object an EDA object, created by \code{\link{eda}}.
#' @param type indicates how to generate cutoffs/breaks. If 'custom', the user-specified
#' breaks are used, otherwise the indicated statistic values are used including the
#' minimum and maximum values of the EDA object. Defaults to quartile.
#' @param breaks valid only for type 'custom'. Either a numeric vector of two or more unique
#' cut points or a single number (greater than or equal to 2)
#' giving the number of intervals into which x is to be cut.
#' @param labels valid only for type 'custom'. For the levels of the resulting category.
#' By default, labels are constructed using "(a,b]" interval notation.
#' If labels = FALSE, simple integer codes are returned instead of a factor.
#' @param include.lowest valid only for type 'custom'.Logical, indicating if an ‘x[i]’ equal
#' to the lowest (or highest, for right = FALSE) ‘breaks’
#' value should be included.
#' @param right valid only for type 'custom'. Logical, indicating if the intervals should be
#' closed on the right (and open on the left) or vice versa.
#' @param default_labels logical, should default labels be used for non-custom breaks? #
#' These are: Q1 to Q4 for quartiles and H1/H2 for mean and median.
#' @param na.rm logical, should NAs be removed prior to cutting?
#' @param ... extra arguments passed to \code{\link[base]{cut}}.
#' @details a wrapper around \code{\link[base]{cut}}.
#' @return a factor-type EDA object.
#' @export cut.eda
#' @export

  cut.eda <- function(eda_object,
                      type = c('quartile', 'mean', 'median', 'custom'),
                      breaks = NULL,
                      labels = NULL,
                      include.lowest = TRUE,
                      right = TRUE,
                      default_labels = TRUE,
                      na.rm = FALSE, ...) {

    stopifnot(all(class(eda_object) == 'eda'))

    type <- match.arg(type[1], c('quartile', 'mean', 'median', 'custom'))

    stopifnot(is.logical(include.lowest))
    stopifnot(is.logical(default_labels))
    stopifnot(is.logical(na.rm))

    if(eda_object$type == 'factor') {

      return(eda_object)

    }

    if(na.rm) eda_object <- na.omit(eda_object)

    if(type != 'custom') {

      breaks <- switch(type,
                       quartile = c(-Inf, quantile(eda_object$value, c(0.25, 0.5, 0.75), na.rm = TRUE), Inf),
                       mean = c(-Inf, mean(eda_object$value, na.rm = TRUE), Inf),
                       median = c(-Inf, median(eda_object$value, na.rm = TRUE), Inf))

      if(default_labels) {

        labels <- switch(type,
                         quartile = c('Q1', 'Q2', 'Q3', 'Q4'),
                         mean = c('H1', 'H2'),
                         median = c('H1', 'H2'))

      }

    }

    cut_vals <- cut(eda_object$value,
                    breaks = breaks,
                    labels = labels,
                    include.lowest = include.lowest,
                    right = right, ...)

    eda(cut_vals)

  }

#' Normalize an EDA object.
#'
#' @description Normalizes values of an EDA object.
#' @details A wrapper around \code{\link[base]{scale}}.
#' @param eda_object an EDA object, created by \code{\link{eda}}.
#' @param center see: \code{\link[base]{scale}}.
#' @param scale see: \code{\link[base]{scale}}.
#' @param na.rm logical, should NAs be removed prior to scaling?
#' @export

  scale.eda <- function(eda_object,
                        center = TRUE,
                        scale = TRUE,
                        na.rm = FALSE) {

    stopifnot(all(class(eda_object) == 'eda'))
    stopifnot(is.logical(na.rm))

    if(eda_object$type == 'factor') {

      return(eda_object)

    }

    if(na.rm) eda_object <- na.omit(eda_object)

    scaled_val <- scale(eda_object$value,
                        center = center,
                        scale = scale)

    eda(scaled_val[, 1])


  }

