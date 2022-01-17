# Appearance, length, NA and level handling methods -----

#' Printing EDA objects.
#'
#' @description print an EDA object.
#' @param eda_object an EDA object, created by \code{\link{eda}}.
#' @return nothing, called for its side effects.
#' @export

  print.eda <- function(eda_object) {

    stopifnot(all(class(eda_object) == 'eda'))

    cat(paste('EDA object of type:', eda_object$type))
    cat('\n')

    eda_len <- length(eda_object$value)

    cat(paste('Length:', eda_len))
    cat('\n')

    if(eda_len > 5) {

      cat(paste(paste(eda_object$value[1:5], sep = ', ')), '...')

    } else {

      cat(paste(eda_object$value, sep = ', '))

    }

  }

#' Length of EDA objects.
#'
#' @description returns length of the EDA object's value vector.
#' @param eda_object an EDA object, created by \code{\link{eda}}.
#' @return a numeric value.
#' @export

  length.eda <- function(eda_object) {

    stopifnot(all(class(eda_object) == 'eda'))

    length(eda_object$value)

  }

#' NA removal from EDA objects.
#'
#' @description removes NAs from the EDA object.
#' @param eda_object an EDA object, created by \code{\link{eda}}.
#' @return an EDA objects without NAs.
#' @export

  na.omit.eda <- function(eda_object) {

    stopifnot(all(class(eda_object) == 'eda'))

    new_vals <- eda_object$value[!is.na(eda_object$value)]

    eda(new_vals)

  }

#' NA removal from EDA objects.
#'
#' @description removes NAs from the EDA object.
#' @param eda_object an EDA object, created by \code{\link{eda}}.
#' @return an EDA objects without NAs.
#' @export

  na.exclude.eda <- function(eda_object) {

    stopifnot(all(class(eda_object) == 'eda'))

    new_vals <- eda_object$value[!is.na(eda_object$value)]

    eda(new_vals)

  }

#' Dropping empty levels from a factor-type EDA object.
#'
#' @description Removes empty levels from the values of factor-type EDA object.
#' For numeric-type ones NULL is returned and a warning generated.
#' @param eda_object an EDA object, created by \code{\link{eda}}.
#' @return an EDA object.
#' @export

  droplevels.eda <- function(eda_object) {

    stopifnot(all(class(eda_object) == 'eda'))

    if(eda_object$type == 'numeric') {

      warning('No levels available for numeric-type EDA objects.', call. = FALSE)

      return(NULL)

    }

    new_vals <- droplevels(eda_object$value)

    eda(new_vals)

  }

#' Number of (complete) observations in an EDA object.
#'
#' @description counts the number of all (complete if na.rm is set to TRUE)
#' observations in the EDA object.
#' @param eda_object an EDA object, created by \code{\link{eda}}.
#' @return a tibble with the number of all and complete observations.
#' @export

  nobs.eda <- function(eda_object) {

    tibble::tibble(observations = c('all', 'complete'),
                   n = c(length(eda_object),
                         length(na.omit(eda_object))))

  }
