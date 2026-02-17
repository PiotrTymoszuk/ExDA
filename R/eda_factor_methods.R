# Methods for factors


# Frequency of observations in categories -------

#' Counts of observations.
#'
#' @description
#' `as.table()` converts factor-type `eda` objects to a table objects with
#' counts, fractions, or percentages of observations.
#' `frequency()` returns a data frame/tibble with numbers of observations in
#' categories, total numbers of observations, number of complete observations,
#' and percentages of all and complete observations.
#'
#' For numeric objecs NULL is returned and a warning generated.
#'
#' @param x an EDA object, created by \code{\link{eda}}
#' @param scale content of the output table: 'fraction' returns fraction of total,
#' 'percent' returns percentages of all observations. Defaults to none.
#' @param .drop logical, should empty levels be removed?
#' @param ... additional arguments passed to methods.
#'
#' @return a table object (`as_table()`) or a tibble (`frequency()`)
#'
#' @export

  as.table.eda <- function(x,
                           scale = c("none", "fraction", "percent"),
                           ...) {

    stopifnot(is_eda(x))

    if(is.numeric(x)) {

      warning("No table available for numerics.",
              call. = FALSE)

      return(NULL)

    }

    scale <- match.arg(scale[1], c("none", "fraction", "percent"))

    tbl_obj <- table(x)

    switch(scale,
           none = tbl_obj,
           fraction = tbl_obj/sum(tbl_obj),
           percent = tbl_obj/sum(tbl_obj) * 100)

  }
#' @rdname as.table.eda
#' @export frequency.eda
#' @export

  frequency.eda <- function(x, .drop = FALSE, ...) {

    stopifnot(is_eda(x))
    stopifnot(is.logical(.drop))

    if(is.numeric(x)) {

      warning("No table available for numerics.",
              call. = FALSE)

      return(NULL)

    }

    category <- NULL

    counts <- count(tibble(category = x),
                    category,
                    .drop = .drop)

    counts[["n_total"]] <- sum(counts[["n"]])

    counts[["percent"]] <- counts[["n"]]/counts[["n_total"]] * 100

    counts[["n_complete"]] <- sum(counts[["n"]][!is.na(counts[["category"]])])

    counts[["percent_complete"]] <-
      ifelse(is.na(counts[["category"]]),
             NA,
             counts[["n"]]/counts[["n_complete"]] * 100)

    counts

  }

# Dropping empty levels --------

#' Dropping empty levels from a factor-type `eda` object.
#'
#' @description Removes empty levels from the values of factor-type `eda` object.
#' For numeric-type ones `NULL` is returned and a warning generated.
#'
#' @param x an \code{\link{eda}} object.
#' @param ... additional arguments for methods, currently none.
#'
#' @return an \code{\link{eda}} object.
#' @export

  droplevels.eda <- function(x, ...) {

    stopifnot(is_eda(x))

    if(is.numeric(x)) {

      warning('No levels available for numeric-type EDA objects.',
              call. = FALSE)

      return(NULL)

    }

    new_vals <- base::droplevels.factor(x)

    eda(new_vals)

  }

# END --------
