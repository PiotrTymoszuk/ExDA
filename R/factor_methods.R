# Table methods ----

#' Convert categorical EDA objects to a table.
#'
#' @description converts factor-type EDA objects to a table. For numeric-type ones
#' NULL is returned and a warning generated.
#' @param x an EDA object, created by \code{\link{eda}}
#' @param scale content of the output table: 'fraction' returns fraction of total,
#' 'percent' returns percentages of all observations. Defaults to none.
#' @return a table object.
#' @export

  as.table.eda <- function(x, scale = c('none', 'fraction', 'percent')) {

    stopifnot(is_eda(x))

    if(x$type == 'numeric') {

      warning('No table available for numeric-type EDA objects.', call. = FALSE)

      return(NULL)

    }

    scale <- match.arg(scale, c('none', 'fraction', 'percent'))

    tbl_obj <- table(x$value)

    switch(scale,
           none = tbl_obj,
           fraction = tbl_obj/sum(tbl_obj),
           percent = tbl_obj/sum(tbl_obj) * 100)

  }

#' Convert categorical EDA objects to a table.
#'
#' @description converts factor-type EDA objects to a table. For numeric-type ones
#' NULL is returned and a warning generated.
#' @param x an EDA object, created by \code{\link{eda}}.
#' 'fraction' returns fraction of total, 'percent' returns percentages of all observations.
#' @param scale content of the output table: 'fraction' returns fraction of total,
#' 'percent' returns percentages of all observations. Defaults to none.
#' @return a table object.
#' @export

  as_table.eda <- function(x, scale = c('none', 'fraction', 'percent')) {

    as.table(x, scale)

  }

#' Default method for as_table.
#'
#' @description Converts an object to a table. A wrapper around \code{\link[base]{as.table}}.
#' @param x an object to be converted to a table.
#' @param ... extra arguments passed to methods.
#' @return an object of class 'table'.
#' @export

  as_table.default <- function(x, ...) {

    table(x, ...)

  }

# Observation counting ----

#' Count observations categorical EDA objects to a table.
#'
#' @description counts of observations assigned to each category or occurrence of unique values
#' for numeric EDAs.
#' @details NAs are listed as a separate category and empty levels are not skipped by default.
#' @param x an EDA object, created by \code{\link{eda}}.
#' @param .drop logical, should empty levels be dropped?
#' @return a tibble with the columns 'category', 'n', 'fraction' and 'percent'.
#' @export

  frequency.eda <- function(x, .drop = FALSE) {

    stopifnot(is_eda(x))

    counts <- dplyr::count(as_tibble(x, 'category'), category, .drop = .drop)

    dplyr::mutate(counts,
                  fraction = n/sum(n),
                  percent = n/sum(n) * 100)

  }

#' Convert categorical EDA objects to a table.
#'
#' @description converts factor-type EDA objects to a table. For numeric-type ones
#' NULL is returned and a warning generated.
#' @param x an EDA object, created by \code{\link{eda}}.
#' @param .drop logical, should empty levels be dropped?
#' @return a tibble with the columns 'category', 'n', 'fraction' and 'percent'.
#' @export

  count.eda <- function(x, .drop = FALSE) {

    stopifnot(is_eda(x))

    frequency(x)

  }

#' Counting of an object occurrence.
#'
#' @description Counts observations in each category via \code{\link[dplyr]{count}}
#' @param x an object.
#' @param ... extra arguments passed to methods.
#' @return a tibble or data frame with the category counts.
#' @export

  count.default <- function(x, ...) {

    dplyr::count(x, ...)

  }
