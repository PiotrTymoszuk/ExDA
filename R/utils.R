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

# Check validity of a data frame and it's variables ---------

#' Check validity of a data frame and it's variables.
#'
#' @description
#' An internal function used to validate if an object is a data frame, and whether
#' it has specific variables.
#' Subsequently, the object is processed by removal of `NA` values, and,
#' optionally, empty levels.
#'
#' @return a data frame with columns whose names are specified by arguments
#' `variable` and `split_factor`. Numbers of observation before and after
#' pre-processing are stored as the `n_number` attribute.
#'
#' @inheritParams plot_df_factor

  validate_df <- function(data, variable, split_factor, .drop = TRUE) {

    ## validation --------

    if(!is.data.frame(data)) {

      stop("`data` has to be a data frame.", call. = FALSE)

    }

    stopifnot(is.character(variable))
    stopifnot(is.character(split_factor))

    if(!variable %in% names(data)) {

      stop("`variable` absent from the data frame.", call. = FALSE)

    }

    if(!split_factor %in% names(data)) {

      stop("`split_factor` absent from the data frame.", call. = FALSE)

    }

    ## pre-processing ---------

    n_numbers <- c("total" = nrow(data))

    data <- data[, c(variable, split_factor)]

    if(!is.factor(data[[split_factor]])) {

      data[[split_factor]] <- factor(data[[split_factor]])

    }

    data[[split_factor]] <- droplevels(data[[split_factor]])

    if(length(levels(data[[split_factor]])) == 1) {

      stop("The `split_factor` must have at least two non-empty categories.",
           call. = FALSE)

    }

    if(is.factor(data[[variable]]) & .drop) {

      data[[variable]] <- droplevels(data[[variable]])

    }

    data <- data[complete.cases(data), ]

    n_numbers <- c(n_numbers,
                   c("complete" = nrow(data)))

    attr(data, "n_numbers") <- n_numbers

    return(data)

  }

# END -------
