# General methods of `eda` and `etest` objects

# Printing -----------

#' Appearance/printing of `eda` and `etest` objects

#' @description
#' Printing methods.
#'
#' @param x an object.
#' @param ... arguments for methods.
#'
#' @return invisibly returns the object.
#' @export

  print.eda <- function(x, ...) NextMethod()

#' @rdname print.eda
#' @export

  print.etest <- function(x, ...) NextMethod()

# NA removal ---------

#' `NA` removal from `eda` objects.
#'
#' @description removes `NA` values from the `eda` objects.
#'
#' @param object an \code{\link{eda}} object.
#' @param ... arguments for methods.
#'
#' @return an `eda` object without `NA` values.
#'
#' @export

  na.omit.eda <- function(object, ...) {

    stopifnot(is_eda(object))

    new_vals <- object[!is.na(object)]

    eda(new_vals)

  }

#' @rdname na.omit.eda
#' @export

  na.exclude.eda <- function(object, ...) {

    stopifnot(is_eda(object))

    new_vals <- object[!is.na(object)]

    eda(new_vals)

  }

# Numbers of observations --------

#' Number of (complete) observations in an `eda` object.
#'
#' @description counts the number of all (complete if na.rm is set to TRUE)
#' observations in the EDA object.
#'
#' @param object an \code{\link{eda}} object.
#' @param plain logical, should the values be returned as a numeric vector.
#' @param ... additional arguments passed to methods, currently none.
#'
#' @return a tibble or numeric vector with the number of all and complete
#' observations.
#'
#' @export

  nobs.eda <- function(object, plain = FALSE, ...) {

    stopifnot(is_eda(object))

    observations <- NULL
    n <- NULL

    lens <- c("all" = length(object),
              "complete" = length(na.omit(object)))

    if(plain) return(lens)

    tibble(observations = names(lens),
           n = lens)

  }

# END -------
