# This script contains constructor functions to generate EDA objects.

# EDA class objects ----

#' Generate `eda` objects.
#'
#' @description
#' Generates `eda` objects from multiple inputs such
#' as vectors or data frame variables.
#' `eda` objects are internally used vectors with predefined behavior
#' concerning handling of NAs and descriptive statistics returned by `summary()`.
#'
#' @param x and object to be converted to `eda` or a data frame with the
#' variable to be converted to an `eda` object.
#' @param variable a name of data frame's variable.
#' @param ... additional arguments for methods,
#'
#' @return an `eda` object: an enriched numeric or factor vector, which inherits
#' many of the numeric or factor canonical methods.
#'
#' @export

  eda <- function(x, ...) UseMethod("eda")

#' @rdname eda
#' @export

  eda.factor <- function(x, ...) {

    stopifnot(is.factor(x))

    if(is_eda(x)) return(x)

    if(all(is.na(x))) return(eda(as.numeric(x)))

    structure(x, class = c("eda", class(x)))

  }

#' @rdname eda
#' @export

  eda.numeric <- function(x, ...) {

    stopifnot(is.numeric(x))

    if(is_eda(x)) return(x)

    structure(x, class = c("eda", class(x)))

  }

#' @rdname eda
#' @export

  eda.character <- function(x, ...) {

    stopifnot(is.character(x))

    if(all(is.na(x))) return(eda(as.numeric(x)))

    message(paste("Converting the input character vector to a factor with the following levels:",
                  paste(levels(factor(x)), collapse = ", ")))

    eda(as.factor(x))

  }

#' @rdname eda
#' @export

  eda.logical <- function(x, ...) {

    stopifnot(is.logical(x))

    if(all(is.na(x))) return(eda(as.numeric(x)))

    levs <- c("FALSE", "TRUE")

    message(paste("Converting the input logical vector to a factor with the following levels:",
                  paste(levs, collapse = ", ")))

    eda(factor(x, levs))

  }

#' @rdname eda
#' @export

  eda.data.frame <- function(x, variable, ...) {

    stopifnot(is.data.frame(x))
    stopifnot(is.character(variable))

    if(variable %in% names(x)) return(eda(x[[variable]]))

    stop("Variable absent from the data frame.",
         call. = FALSE)

  }

#' @rdname eda
#' @export

  is_eda <- function(x) inherits(x, "eda")

# Test class objects ----

#' Generate `eTest` objects.
#'
#' @description
#' Generates `eTest` objects storing results of statistical hypothesis tests
#' in a standardized data frame.
#'
#' @param x an object.
#' @param test name of the statistical test.
#' @param stat_name name of the test statistic.
#' @param stat value of the test statistic.
#' @param df1 = degrees of freedom.
#' @param df2 degrees of freedom.
#' @param estimate_name name of the test estimate.
#' @param estimate value of the test estimate.
#' @param lower_ci value of the lower confidence interval limit.
#' @param upper_ci value of the upper confidence interval limit.
#' @param p_value p value.
#' @param n number of complete observations used for testing.
#'
#' @return an `etest` data frame. It shares most of its methods
#' with a "canonical" data frame.
#'
#' @export

  etest <- function(test,
                    stat_name = NA,
                    stat = NA,
                    df1 = NA,
                    df2 = NA,
                    estimate_name = NA,
                    estimate = NA,
                    lower_ci = NA,
                    upper_ci = NA,
                    p_value = NA,
                    n = NA) {

    x <- tibble(test = test,
                stat_name = stat_name,
                stat = stat,
                df1 = df1,
                df2 = df2,
                estimate_name = estimate_name,
                estimate = estimate,
                lower_ci = lower_ci,
                upper_ci = upper_ci,
                p_value = p_value,
                n = n)

    structure(x, class = c("etest", class(x)))

  }

#' @rdname etest
#' @export

  is_etest <- function(x) inherits(x, "etest")

# END -------
