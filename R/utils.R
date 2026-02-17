# Non-exported utilities

# Descriptive stats ---------


#'A universal statistic extractor.
#'
#' @description calculates the requested statistic for a numeric vector.
#' NAs are removed prior to analysis
#'
#' @param x an eda object.
#'
#' @param fun a statistic calculation function, should return a single numeric.
#' @param plain logical, should the output be coerced to a single vector?
#' @param stat_name name of the statistic included in the output data frame.
#' @param ... extra argument passed to fun.
#'
#' @return a tibble or a numeric value.

  stat_extract <- function(x,
                           fun,
                           stat_name,
                           plain = FALSE, ...) {

    ## input controls ----------

    stopifnot(is_eda(x))
    stopifnot(is_function(fun))
    stopifnot(is.logical(plain))
    stopifnot(is.character(stat_name))

    stat_name <- stat_name[1]

    if(is.factor(x)) {

      warning(paste(stat_name,
                    "statistic is not available for factors."),
              call. = FALSE)

      return(NULL)

    }

    total_observations <- length(x)

    x <- x[!is.na(x)]

    complete_observations <- length(x)

    val <- fun(x, ...)

    if(plain) return(val)

    statistic <- NULL
    value <- NULL
    n_total <- NULL
    n_complete <- NULL

    tibble(statistic = stat_name,
           value = val,
           n_total = total_observations,
           n_complete = complete_observations)

  }

# END -------
