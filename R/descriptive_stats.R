# Functions for computation of descriptive statistics and result summaries

# Descriptive statistics --------

#' Exploratory data analysis of a data frame.
#'
#' @description
#' Performs exploratory data analysis for the selected variables of a data frame.
#' The output may include lists of distribution statistics, or
#' a publication-ready table with the distribution statistics.
#'
#' @details
#' The function is way more compact than `explore()` in previous version of the
#' package.
#' For instance it does not provide a plot output anymore, and skips
#' the `pub_styled` argument which are implicitly assumed to be true, if the user
#' requests a summary table as an output.
#' It implements also new features, like total number of observations in the summary
#' table, and and option to collapse the output into a single data frame.
#' The `style` argument specifies, which descriptive statistics for numeric
#' features will be displayed in the summary table:
#'
#' * `"full"`: means, standard deviations, medians, and interquartile ranges
#'
#' *`"mean/SD"`: means with standard deviations
#'
#' * `"median/IQR"`: medians with interquartile ranges
#'
#' @param x a data frame.
#' @param split_factor optional, the name of a factor used for splitting the
#' variables of interest into analysis groups.
#' @param variables a vector of variable names. If `NULL` all variables in the
#' data frame except of the splitting factor will be analyzed.
#' @param what the type of output: 'list' returns a list of distribution statistics,
#' 'table' (default) returns a publication-ready data frame with
#' the distribution stats,
#' 'skewness' and 'kurtosis' return data frames with the requested statistics.
#' @param .drop logical, should empty categories of factors in the data frame be removed?
#' @param signif_digits significant digits used for rounding in the
#' publication-style output.
#' @param one_table logical, should the output be coerced to a single data frame?
#' Applies to `what` set to 'table', 'skewness' or 'kurtosis', and ignored otherwise.
#' @param total_text a character vector (text) in the output's first row with the
#' total number of observations. Applied only if `pub_styled = TRUE`.
#' @param style style of the output table, see Details. Defaults to `full`.
#' Used only if `what = "table"`.
#' @param rm_range logical, should the information on range of numeric variables
#' be removed from the output data frame. Used only if `what = "table"`.
#' @param rm_complete logical, should the information on numbers of complete
#' observations be removed from the output data frame. Used only if `what = "table"`.
#' Useful, when the user analyzes a data frame without missing values.
#' @param ... additional arguments. Kept for compatibility with earlier versions
#' of `explore()`, which took e.g. `pub_styled` argument.
#'
#' @return as specified by the 'what' argument: a list of stats, a data frame
#' (if `one_table = TRUE`) or a list of data frames (if `one_table = FALSE`)
#' with descriptive statistics.
#' If `one_table = TRUE` and `what = "table"`,
#' the function returns a single data frame of class \code{\link{destat}}, which
#' may be later merged with statistical hypothesis test results
#' (\code{\link{compare_variables}}) into a publication-ready result summary table.
#'
#' @md
#' @export explore.data.frame
#' @export

  explore.data.frame <- function(x,
                                 variables = NULL,
                                 split_factor = NULL,
                                 what = c("table", "list", "skewness", "kurtosis"),
                                 .drop = TRUE,
                                 signif_digits = 2,
                                 one_table = TRUE,
                                 total_text = "observations, total, N",
                                 style = c("full", "mean/SD", "median/IQR"),
                                 rm_range = FALSE,
                                 rm_complete = FALSE,
                                 ...) {

    ## entry control ----------

    if(!is.data.frame(x)) stop("`x` has to be a data frame.", call. = FALSE)

    if(!is.null(split_factor)) {

      stopifnot(is.character(split_factor))

      split_factor <- split_factor[1]

      if(!split_factor %in% names(x)) {

        stop("`split_factor` is missing from `x`.", call. = FALSE)

      }

      if(!is.factor(x[[split_factor]])) {

        x[[split_factor]] <- factor(x[[split_factor]])

      }

      x[[split_factor]] <- droplevels(x[[split_factor]])

    }

    if(is.null(variables)) {

      if(is.null(split_factor)) {

        variables <- names(x)

      } else {

        variables <- names(x)[names(x) != split_factor]

      }

    }

    if(!all(variables %in% names(x))) {

      stop("Some variables are missing from `x`.", call. = FALSE)

    }

    what <-
      match.arg(what[1],
                c("table", "list", "skewness", "kurtosis"))

    stopifnot(is.logical(.drop))
    .drop <- .drop[1]

    stopifnot(is.logical(one_table))
    one_table <- one_table[1]

    stopifnot(is.numeric(signif_digits))
    signif_digits <- as.integer(signif_digits[1])

    stopifnot(is.character(total_text))
    total_text <- total_text[1]

    stopifnot(is.logical(rm_range))
    rm_range <- rm_range[1]

    stopifnot(is.logical(rm_complete))
    rm_complete <- rm_complete[1]

    style <- match.arg(style[1], c("full", "mean/SD", "median/IQR"))

    ## splitting factor present: multiple data frames --------

    if(!is.null(split_factor)) {

      ## empty at the moment

      data_lst <- split(x[, variables], x[[split_factor]])

      if(what == "list") {

        return(map(data_lst,
                   explore,
                   variables = variables,
                   split_factor = NULL,
                   what = "list"))

      } else {

        res <- map(data_lst,
                   explore,
                   variables = variables,
                   split_factor = NULL,
                   what = what,
                   .drop = .drop,
                   signif_digits = signif_digits,
                   one_table = one_table,
                   total_text = total_text,
                   style = style,
                   rm_range = rm_range,
                   rm_complete = rm_complete)

        if(!one_table) return(res)

        if(what == "table") {

          split_names <- names(res)

          res <- reduce(res, full_join, by = "variable")

          res <- set_names(res, c("variable", split_names))

          return(destat(res, variables))

        } else {

          res <- map2_dfr(res, names(res),
                          ~mutate(.x,
                                  !!split_factor := factor(.y, names(res))))

          return(relocate(res, .data[[split_factor]]))

        }

      }

    }

    ## analysis data --------

    variable <- NULL

    obj_lst <- map(x[, variables], eda, .drop = .drop)

    ## splitting factor absent, list output ----------

    if(what == "list") return(map(obj_lst, summary, pub_styled = FALSE))

    if(what %in% c("kurtosis", "skewness")) {

      stat_fun <- switch(what,
                         kurtosis = function(x) kurtosis(x, plain = FALSE),
                         skewness = function(x) skewness(x, plain = FALSE))

      stat_lst <- map(obj_lst, stat_fun)

      stat_lst <- compact(stat_lst)

      if(!one_table) return(stat_lst)

      stat_lst <- map2_dfr(stat_lst, names(stat_lst),
                           ~mutate(.x, variable = .y))

      return(relocate(stat_lst, variable))

    }

    ## splitting factor absent, summary data frame output --------

    if(what == "table") {

      stat_lst <- map(obj_lst,
                      summary,
                      pub_styled = TRUE,
                      signif_digits = signif_digits)

      stat_lst <- compact(stat_lst)

      stat_lst <- map2_dfr(stat_lst, names(stat_lst),
                           ~mutate(.x, variable = .y))

      statistic <- NULL

      n_obs_info <- tibble(variable = total_text,
                           statistic = nrow(x))

      if(rm_range) {

        stat_lst[["statistic"]] <-
          stri_replace(stat_lst[["statistic"]],
                       regex = "\\nrange.*\\n",
                       replacement = "\n")

      }

      if(rm_complete) {

        stat_lst[["statistic"]] <-
          stri_replace(stat_lst[["statistic"]],
                       regex = "\\ncomplete.*",
                       replacement = "")

      }

      if(style == "median/IQR") {

        stat_lst[["statistic"]] <-
          stri_replace(stat_lst[["statistic"]],
                       regex = "^mean.*\\n",
                       replacement = "")

      } else if(style == "mean/SD") {

        stat_lst[["statistic"]] <-
          stri_replace(stat_lst[["statistic"]],
                       regex = "\\nmedian.*\\]",
                       replacement = "")

      }

      res <- relocate(rbind(n_obs_info, stat_lst), variable)

      return(destat(res, variables))

    }

  }

# Summary of descriptive statistics and hypothesis testing results --------

#' Summary table with descriptive statistics and results of statistical hypothesis tests.
#'
#' @description
#' Function `result_summary()` creates a ready-to-use summary table which bundles
#' a data frame with descriptive statistics (created with \code{\link{explore.data.frame}})
#' with results of statistical hypothesis tests for comparison of expected values
#' (optional, created with \code{\link{compare_variables}}).
#'
#' @return
#' A data frame with formatted descriptive statistics and, optionally,
#' test type information, raw and adjusted p values, and effect sizes.
#'
#' @param x a data frame with descriptive statistics: an object of class
#' \code{\link{destat}} created with \code{\link{explore.data.frame}}.
#' @param y `NULL` or a data frame of \code{\link{etest}} with results of
#' statistical hypothesis testing created with \code{\link{compare_variables}}.
#' @param test_columns a character vector of names of columns in `y` to be merged
#' with the descriptive statistic information.
#' @param labeller_fun a function used to transform names of variables in the
#' result summary data frame.
#' @param ... additional arguments, currently none.
#'
#' @export

  result_summary <- function(x,
                             y = NULL,
                             labeller_fun = identity,
                             test_columns = c("test",
                                              "raw_significance",
                                              "significance",
                                              "effect_size_txt"),
                             ...) {

    ## input control --------

    if(!is_destat(x)) {

      stop("`x` has to be a data frame created with `explore.data.frame()`.",
           call. = FALSE)

    }

    if(!is_function(labeller_fun)) {

      stop("`labeller_fun` has to be a function.", call. = FALSE)

    }

    if(!is.null(y)) {

      if(!is_etest(y)) {

        stop("`y` has to be an `etest` data frame returned by `compare_variables()`.",
             call. = FALSE)

      }

      if(!"variable" %in% names(y)) {

        stop("Column `variable` must be present in `y` data frame.",
             call. = FALSE)

      }

      missing_cols <- setdiff(test_columns, names(y))

      if(length(missing_cols) > 0) {

        stop(paste("The following columns specified in `test_columns` are missing:",
                   paste(missing_cols, collapse = ", ")),
             call. = FALSE)

      }

    }

    # result summary ---------

    variables <- attr(x, "variable_names")

    if(!is.null(y)) {

      x <- left_join(x,
                     y[c("variable", test_columns)],
                     by = "variable")

    }

    x[["variable"]] <-
      ifelse(x[["variable"]] %in% variables,
             labeller_fun(x[["variable"]]),
             x[["variable"]])

    return(x)

  }

# END ---------
