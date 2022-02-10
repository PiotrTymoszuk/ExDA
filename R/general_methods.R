# Appearance, length, NA and level handling methods -----

#' Printing EDA objects.
#'
#' @description print an EDA object.
#' @param x an EDA object, created by \code{\link{eda}}.
#' @return nothing, called for its side effects.
#' @export

  print.eda <- function(x, ...) {

    stopifnot(is_eda(x))

    cat(paste('EDA object of type:', x$type))
    cat('\n')

    eda_len <- length(x$value)

    cat(paste('Length:', eda_len))
    cat('\n')

    if(eda_len > 5) {

      cat(paste(paste(x$value[1:5], sep = ', ')), '...')

    } else {

      cat(paste(x$value, sep = ', '))

    }

  }

#' Length of EDA objects.
#'
#' @description returns length of the EDA object's value vector.
#' @param x an EDA object, created by \code{\link{eda}}.
#' @return a numeric value.
#' @export

  length.eda <- function(x) {

    stopifnot(is_eda(x))

    length(x$value)

  }

#' NA removal from EDA objects.
#'
#' @description removes NAs from the EDA object.
#' @param object an EDA object, created by \code{\link{eda}}.
#' @return an EDA objects without NAs.
#' @export

  na.omit.eda <- function(object, ...) {

    stopifnot(is_eda(object))

    new_vals <- object$value[!is.na(object$value)]

    eda(new_vals)

  }

#' NA removal from EDA objects.
#'
#' @description removes NAs from the EDA object.
#' @param object an EDA object, created by \code{\link{eda}}.
#' @return an EDA objects without NAs.
#' @export

  na.exclude.eda <- function(object) {

    stopifnot(is_eda(object))

    new_vals <- object$value[!is.na(object$value)]

    eda(new_vals)

  }

#' Dropping empty levels from a factor-type EDA object.
#'
#' @description Removes empty levels from the values of factor-type EDA object.
#' For numeric-type ones NULL is returned and a warning generated.
#' @param x an EDA object, created by \code{\link{eda}}.
#' @return an EDA object.
#' @export

  droplevels.eda <- function(x, ...) {

    stopifnot(is_eda(x))

    if(x$type == 'numeric') {

      warning('No levels available for numeric-type EDA objects.', call. = FALSE)

      return(NULL)

    }

    new_vals <- droplevels(x$value)

    eda(new_vals)

  }

#' Number of (complete) observations in an EDA object.
#'
#' @description counts the number of all (complete if na.rm is set to TRUE)
#' observations in the EDA object.
#' @param object an EDA object, created by \code{\link{eda}}.
#' @return a tibble with the number of all and complete observations.
#' @export

  nobs.eda <- function(object, ...) {

    stopifnot(is_eda(object))

    tibble::tibble(observations = c('all', 'complete'),
                   n = c(length(object),
                         length(na.omit(object))))

  }
