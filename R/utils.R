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
#' `variable` and `split_factor`, or `variable1` and `variable2`.
#' Numbers of observation before and after pre-processing are stored as
#' the `n_numbers` and `n_categories` attributes.
#'
#' @inheritParams plot_df_factor
#' @inheritParams plot_2df_numeric
#' @inheritParams draw_numeric_panel
#' @param format the intended format of the variables: numeric of factor.
#' If there are any requested variables in the data frame incompatible with
#' the `format`, and error is raised.

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

#' @rdname validate_df

  validate_2df <- function(data, variable1, variable2, .drop = TRUE) {

    ## validation ------

    if(!is.data.frame(data)) {

      stop("`data` has to be a data frame.", call. = FALSE)

    }

    stopifnot(is.character(variable1))
    stopifnot(is.character(variable2))

    if(!variable1 %in% names(data)) {

      stop("`variable1` absent from the data frame.", call. = FALSE)

    }

    if(!variable2 %in% names(data)) {

      stop("`variable2` absent from the data frame.", call. = FALSE)

    }

    if((is.factor(data[[variable1]]) & is.numeric(data[[variable2]])) |
       (is.factor(data[[variable2]]) & is.numeric(data[[variable1]]))) {

      stop(paste("Both variables have to be either numeric or factors.",
                 "For plotting of a numeric/factor pair, please use `plot_variables()`."),
           call. = FALSE)

    }

    ## pre-processing --------

    n_numbers <- c("total" = nrow(data))

    data <- data[, c(variable1, variable2)]

    data <- data[complete.cases(data), ]

    n_numbers <- c(n_numbers,
                   c("complete" = nrow(data)))

    if(is.factor(data[[variable1]]) & .drop) {

      data[[variable1]] <- droplevels(data[[variable1]])
      data[[variable2]] <- droplevels(data[[variable2]])

    }

    attr(data, "n_numbers") <- n_numbers

    return(data)

  }

#' @rdname validate_df

  validate_multi_df <- function(data,
                                variables,
                                split_factor = NULL,
                                format = c("numeric", "factor"),
                                .drop = TRUE) {

    ## input control and validation --------

    if(!is.data.frame(data)) {

      stop("`data` has to be a data frame.", call. = FALSE)

    }

    stopifnot(is.character(variables))

    if(length(variables) < 2) {

      stop(paste("`variables` must have at least two elements.",
                 "For plotting single variables or a",
                 "splitting factor - variable pair, please consider",
                 "`plot_variable()`."),
           call. = FALSE)

    }



    if(!is.null(split_factor)) {

      stopifnot(is.character(split_factor))

      if(!split_factor %in% names(data)) {

        stop("`split_factor` absent from the data frame.", call. = FALSE)

      }

      if(!is.factor(data[[split_factor]])) {

        data[[split_factor]] <- factor(data[[split_factor]])

      }

      data[[split_factor]] <- droplevels(data[[split_factor]])

      if(length(levels(data[[split_factor]])) == 1) {

        stop("The `split_factor` must have at least two non-empty categories.",
             call. = FALSE)

      }

    }

    missing_vars <- setdiff(variables, names(data))

    if(length(missing_vars) > 0) {

      stop(paste("Some variables are missing from the data frame:",
                 paste(missing_vars, collapse = ", ")),
           call. = FALSE)

    }

    format <- match.arg(format[1], c("numeric", "factor"))

    if(format == "numeric") {

      var_form <- map_lgl(data[, variables], is.numeric)

      if(any(!var_form)) {

        stop(paste("The following variables are not numeric:",
                   paste(variables[!var_form], collapse = ", ")),
             call. = FALSE)

      }

    } else {

      var_form <- map_lgl(data[, variables], is.factor)

      if(any(!var_form)) {

        stop(paste("The following variables are not factors:",
                   paste(variables[!var_form], collapse = ", ")),
             call. = FALSE)

      }

      if(.drop) {

        data[, variables] <- map_dfc(data[, variables], droplevels)

      }

    }

    ## handling of NA-only variables ---------

    na_only_check <- map_lgl(data[, variables], ~all(is.na(.x)))

    if(sum(na_only_check) == length(variables)) {

      stop("Nothing to plot: all variables are `NA`.", call. = FALSE)

    }

    na_only_vars <- names(na_only_check)[na_only_check]

    data <- data[, !names(data) %in% na_only_vars]

    ## N numbers and processing of single NA values ----------

    ### total N numbers and numbers of records with the complete
    ### splitting factor

    n_numbers <- c("total" = nrow(data))

    if(!is.null(split_factor)) {

      data <- data[!is.na(data[[split_factor]]),
                   c(split_factor, variables)]

      n_numbers <- c(n_numbers,
                     c("split_complete" = nrow(data)))

      ### numbers of complete cases in categories of the splitting
      ### factor for each variable

      n_categories <-
        map(variables,
            function(x) data[!is.na(data[[x]]), split_factor, drop = FALSE])

      n_categories <- set_names(map(n_categories,
                                    count,
                                    .data[[split_factor]]),
                                variables)

      variable <- NULL

      n_categories <- map2_dfr(n_categories,
                               names(n_categories),
                               ~mutate(.x, variable = .y))

    } else {

      data <- data[, variables]

      n_categories <- map_dbl(data, ~length(.x[!is.na(.x)]))

    }

    ## the output plotting data ---------

    attr(data, "n_numbers") <- n_numbers
    attr(data, "n_categories") <- n_categories

    return(data)

  }

# Axis and facet labels in plotting data ---------

#' Axis and facet labels for plotting data.
#'
#' @description
#' An internal function, which appends the plotting data with labels to
#' be presented in plot axes and facets.
#'
#' @return a data frame with additional columns `axis_label` and `facet_label`.
#'
#' @inheritParams draw_numeric_panel
#' @param n_column name of the data frame column storing information on N number
#' of complete observations to be included in axis labels if `n_labs = TRUE`.
#' @param plot_data a data frame used for later for plotting, already pre-formatted.

  add_labels <- function(plot_data,
                         split_factor = NULL,
                         n_labs = TRUE,
                         n_lab_sep = "\n",
                         labeller_fun = identity,
                         n_column = "n") {

    ## assuming the data is already pre-formatted and no checks required

    if(is.null(split_factor)) {

      ### axis labels

      plot_data[["axis_label"]] <-
        labeller_fun(as.character(plot_data[["variable"]]))

      if(n_labs) {

        plot_data[["axis_label"]] <-
          paste(plot_data[["axis_label"]],
                plot_data[[n_column]],
                sep = paste0(n_lab_sep, "n = "))

      }

      plot_data[["axis_label"]] <-
        reorder(plot_data[["axis_label"]],
                as.numeric(plot_data[["variable"]]))

    } else {

      ### axis and facet labels

      plot_data[["axis_label"]] <- as.character(plot_data[[split_factor]])

      if(n_labs) {

        plot_data[["axis_label"]] <-
          paste(plot_data[["axis_label"]],
                plot_data[[n_column]],
                sep = paste0(n_lab_sep, "n = "))

      }

      plot_data[["axis_label"]] <-
        reorder(plot_data[["axis_label"]],
                -as.numeric(plot_data[[split_factor]]))

      plot_data[["facet_label"]] <-
        labeller_fun(as.character(plot_data[["variable"]]))

      plot_data[["facet_label"]] <-
        reorder(plot_data[["facet_label"]],
                as.numeric(plot_data[["variable"]]))

    }

    plot_data

  }

# Formatting of p values ----------

#' Format p values in results of statistical hypothesis tests.
#'
#' @description
#' Formatting of p values.
#' P values greater or equal than 0.05 are presented in a `ns (p = ...)` form.
#' P values lower that `simplify_p` value are presented in a `p < ...` form.
#'
#' @return a character vector with the formatted p values.
#'
#' @param x a numeric vector.
#' @param significant_p significance threshold, defaults to p = 0.05.
#' @param simplify_p a numeric value, p values below `simplify_p` are presented
#' in a form `p < ...`, e.g. for the default p values < 0.001 be presented in a
#' p < 0.001 form.
#' @param signif_digits number of significant digits used for rounding of
#' p values.
#'
#' @export

  format_p <- function(x,
                       significant_p = 0.05,
                       simplify_p = 0.001,
                       signif_digits = 2) {

    ## input control -----

    if(!is.numeric(x)) stop("`x` has to be a numeric vector.", call. = FALSE)

    stopifnot(is.numeric(significant_p))

    if(significant_p >= 1) stop("`significant_p` must be < 1.", call. = FALSE)

    stopifnot(is.numeric(simplify_p))

    if(simplify_p > significant_p) {

      stop("`simplify_p` has to be lower than `significant_p`.", call. = FALSE)

    }

    ## formatting -----

    simplify_txt <- paste("p <", simplify_p)

    ifelse(x >= significant_p,
           paste0("ns (p = ", signif(x, signif_digits), ")"),
           ifelse(x >= simplify_p,
                  paste("p =", signif(x, signif_digits)),
                  simplify_txt))

  }

# END -------
