# Functions for computation of descriptive statistics

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
#' @param data a data frame.
#' @param split_factor optional, the name of a factor used for splitting the
#' variables of interest into analysis groups.
#' @param variables a vector of variable names. If `NULL` all variables in the
#' data frame except of the splitting factor will be analyzed.
#' @param what the type of output: 'list' returns a list of distribution statistics,
#' 'table' (default) returns a publication-ready data frame with
#' the distribution stats,
#' 'skewness' and 'kurtosis' return data frames with the requested statistics.
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
#'
#' @md
#' @export

  explore <- function(data,
                      variables = NULL,
                      split_factor = NULL,
                      what = c("table", "list", "skewness", "kurtosis"),
                      signif_digits = 2,
                      one_table = TRUE,
                      total_text = "observations, total, N",
                      style = c("full", "mean/SD", "median/IQR"),
                      rm_range = FALSE,
                      rm_complete = FALSE,
                      ...) {

    ## entry control ----------

    if(!is.data.frame(data)) stop("`data` has to be a data frame.", call. = FALSE)

    if(!is.null(split_factor)) {

      stopifnot(is.character(split_factor))

      split_factor <- split_factor[1]

      if(!split_factor %in% names(data)) {

        stop("`split_factor` is missing from `data`.", call. = FALSE)

      }

      if(!is.factor(data[[split_factor]])) {

        data[[split_factor]] <- factor(data[[split_factor]])

      }

    }

    if(is.null(variables)) {

      if(is.null(split_factor)) {

        variables <- names(data)

      } else {

        variables <- names(data)[names(data) != split_factor]

      }

    }

    if(!all(variables %in% names(data))) {

      stop("Some variables are missing from `data`.", call. = FALSE)

    }

    what <-
      match.arg(what[1],
                c("table", "list", "skewness", "kurtosis"))

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

      data_lst <- split(data[, variables], data[[split_factor]])

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

          return(set_names(res, c("variable", split_names)))

        } else {

          res <- map2_dfr(res, names(res),
                          ~mutate(.x,
                                  !!split_factor := factor(.y, names(res))))

          return(relocate(res, .data[[split_factor]]))

        }

      }

    }

    ## splitting factor absent, list output ----------

    variable <- NULL

    obj_lst <- map(data[, variables], eda)

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

      stat_lst <- map(obj_lst, summary, pub_styled = TRUE)

      stat_lst <- compact(stat_lst)

      stat_lst <- map2_dfr(stat_lst, names(stat_lst),
                           ~mutate(.x, variable = .y))

      statistic <- NULL

      n_obs_info <- tibble(variable = total_text,
                           statistic = nrow(data))

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

      return(relocate(rbind(n_obs_info, stat_lst), variable))

    }

  }

# END ---------
