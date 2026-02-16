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

# EDA object summary -----

#' Summary of `eda` objects.
#'
#' @description Summary of numeric and factor `eda` objects.
#'
#' @details The `pub_styled` argument allows for generating a 'nicer' output
#' accepted by multiple journals.
#' A tibble with the column 'statistic' is returned.
#'
#' @param object \code{\link{eda}} object.
#' @param pub_styled logical, should the output be publication-ready formatted?
#' See the details.
#' @param signif_digits number of significant digits used for rounding in
#' the publication-style output.
#' @param ... additional arguments for methods.
#'
#' @export

  summary.eda <- function(object,
                          pub_styled = FALSE,
                          signif_digits = 2, ...) {

    ## input controls --------

    stopifnot(is_eda(object))
    stopifnot(is.logical(pub_styled))
    stopifnot(is.numeric(signif_digits))

    signif_digits <- as.integer(signif_digits[1])

    total_observations <- length(object)
    complete_observations <- length(object[!is.na(object)])

    statistic <- NULL

    ## summary for factors --------

    if(is.factor(object)) {

      if(!pub_styled) {

        return(frequency(object))

      } else {

        counts <- as.table(object)
        perc_complete <- as.table(object, scale = "percent")
        levs <- names(counts)

        cat_string <-
          pmap_chr(list(x = levs,
                        y = perc_complete,
                        z = counts),
                   function(x, y, z) paste0(x, ": ", signif(y, signif_digits),
                                            "% (", z, ")"))

        cat_string <- paste(cat_string, collapse = "\n")

        cat_string <- paste(cat_string,
                            complete_observations,
                            sep = "\ncomplete: n = ")

        return(tibble(statistic = cat_string))

        }

    }

    ## summary for numeric objects ---------

    stat_funs <- list(mean,
                      sd,
                      median,
                      function(x) quantile(x, c(0.25, 0.75)),
                      function(x) quantile(x, c(0, 1)))

    stat_names <-
      c("mean", "sd", "median", "perc_25", "perc_75", "min", "max")

    stats <- map_dfr(stat_funs, ~.x(object))

    if(!pub_styled) {

      stats[["statistic"]] <- stat_names

      stats[["n_complete"]] <- complete_observations

      return(stats[, c("statistic", "value", "n_complete")])

    }

    stat_string <- paste0("mean = ", signif(stats$value[1], signif_digits),
                          " (SD: ", signif(stats$value[2], signif_digits), ")",
                          "\nmedian = ", signif(stats$value[3], signif_digits),
                          " [IQR: ", signif(stats$value[4], signif_digits),
                          " - ", signif(stats$value[5], signif_digits), "]",
                          "\nrange: ", signif(stats$value[6], signif_digits),
                          " - ", signif(stats$value[7], signif_digits))

    stat_string <- paste(stat_string,
                         complete_observations,
                         sep = "\ncomplete: n = ")

    tibble(statistic = stat_string)

  }

# END -------
