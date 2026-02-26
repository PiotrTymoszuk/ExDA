# This script contains constructor functions to generate EDA objects.

# EDA class objects ----

#' Generate `eda` objects.
#'
#' @description
#' Generates `eda` objects from multiple inputs such
#' as vectors or data frame variables.
#' `eda` objects are internally used vectors with predefined behavior
#' concerning handling of NAs and descriptive statistics returned by `summary()`.
#'
#' @param x and object to be converted to `eda` or a data frame with the
#' variable to be converted to an `eda` object.
#' @param variable a name of data frame's variable.
#' @param .drop logical, should empty levels of a factor be dropped?
#' Defaults to `TRUE`.
#' @param ... additional arguments for methods,
#'
#' @return an `eda` object: an enriched numeric or factor vector, which inherits
#' many of the numeric or factor canonical methods.
#'
#' @export

  eda <- function(x, ...) UseMethod("eda")

#' @rdname eda
#' @export

  eda.factor <- function(x, .drop = TRUE, ...) {

    stopifnot(is.factor(x))

    if(is_eda(x)) return(x)

    if(all(is.na(x))) return(eda(as.numeric(x)))

    if(.drop) x <- droplevels(x)

    structure(x, class = c("eda", class(x)))

  }

#' @rdname eda
#' @export

  eda.numeric <- function(x, ...) {

    stopifnot(is.numeric(x))

    if(is_eda(x)) return(x)

    structure(x, class = c("eda", class(x)))

  }

#' @rdname eda
#' @export

  eda.character <- function(x, ...) {

    stopifnot(is.character(x))

    if(all(is.na(x))) return(eda(as.numeric(x)))

    message(paste("Converting the input character vector to a factor with the following levels:",
                  paste(levels(factor(x)), collapse = ", ")))

    eda(as.factor(x))

  }

#' @rdname eda
#' @export

  eda.logical <- function(x, ...) {

    stopifnot(is.logical(x))

    if(all(is.na(x))) return(eda(as.numeric(x)))

    levs <- c("FALSE", "TRUE")

    message(paste("Converting the input logical vector to a factor with the following levels:",
                  paste(levs, collapse = ", ")))

    eda(factor(x, levs))

  }

#' @rdname eda
#' @export

  eda.data.frame <- function(x, variable, ...) {

    stopifnot(is.data.frame(x))
    stopifnot(is.character(variable))

    if(variable %in% names(x)) return(eda(x[[variable]]))

    stop("Variable absent from the data frame.",
         call. = FALSE)

  }

#' @rdname eda
#' @export

  is_eda <- function(x) inherits(x, "eda")

# Test class objects ----

#' Generate `eTest` objects.
#'
#' @description
#' Generates `eTest` objects storing results of statistical hypothesis tests
#' in a standardized data frame.
#'
#' @details
#' The formatted significance and adjusted significance information
#' appear in the columns `raw_significance` and `significance`.
#' The formatted effect size text (e.g. `"V = 0.32"`) is stored in the
#' column `effect_size_txt`.
#' Ready-to-use subtitles/captions for plots with effect sizes and significance
#' are stored in the column named `plot_caption`.
#'
#' @inheritParams format_p
#' @param x an object.
#' @param test vector with names of the statistical test.
#' @param stat_name vector with names of the test statistic.
#' @param stat vector with values of the test statistic.
#' @param n vectors of number of complete observations used for testing
#' @param df1 vector of degrees of freedom.
#' @param df2 vector of degrees of freedom.
#' @param estimate_name vector of names name of the test estimate.
#' @param estimate vector of values of the test estimate.
#' @param lower_ci vector of values of the lower confidence interval limit.
#' @param upper_ci vector of value of the upper confidence interval limit.
#' @param p_value vector of raw p value not adjusted for multiple testing.
#' @param p_adjust_method vector of names of the p value adjustment methods.
#' @param p_adjusted vector of adjusted p value.
#' @param effect_name vector of names of effect size statistics.
#' @param effect_size vector of values of effect size statistics.
#' @param plot_caption names of the columns of the output data frame which are
#' merged in the ready-to-use plot captions. By default they are `"eff_size_txt"`
#' and `"significance"`, which means that the plot caption column will contain
#' formatted text with the effect size information and p values adjusted for
#' multiple testing.
#' @param plot_caption_sep character separator pasted between statistics in the
#' plot caption (`plot_caption`) column.
#' @param ... additional arguments, currently none.
#'
#' @return an `etest` data frame. It shares most of its methods
#' with a "canonical" data frame.
#'
#' @export

  etest <- function(test,
                    stat_name = NA,
                    stat = NA,
                    n = NA,
                    df1 = NA,
                    df2 = NA,
                    estimate_name = NA,
                    estimate = NA,
                    lower_ci = NA,
                    upper_ci = NA,
                    p_value = NA,
                    p_adjust_method = NA,
                    p_adjusted = NA,
                    significant_p = 0.05,
                    simplify_p = 0.001,
                    signif_digits = 2,
                    effect_name = NA,
                    effect_size = NA,
                    plot_caption = c("effect_size_txt", "significance"),
                    plot_caption_sep = "\n", ...) {

    ## bare container with the testing results ------

    x <- tibble(test = test,
                stat_name = stat_name,
                stat = stat,
                n = n,
                df1 = df1,
                df2 = df2,
                estimate_name = estimate_name,
                estimate = estimate,
                lower_ci = lower_ci,
                upper_ci = upper_ci,
                p_value = p_value,
                p_adjust_method = p_adjust_method,
                p_adjusted = p_adjusted,
                effect_name = effect_name,
                effect_size = effect_size)

    ## formatting of the testing results ---------

    x[["raw_significance"]] <- format_p(x[["p_value"]],
                                        significant_p = significant_p,
                                        simplify_p = simplify_p)

    x[["significance"]] <- format_p(x[["p_adjusted"]],
                                    significant_p = significant_p,
                                    simplify_p = simplify_p)

    x[["n_txt"]] <- paste("n =", x[["n"]])

    if(any(!is.na(x[["lower_ci"]])) | any(!is.na(x[["upper_ci"]]))) {

      x[["ci_text"]] <-
        paste0("[", signif(x[["lower_ci"]], signif_digits),
               " to ", signif(x[["upper_ci"]], signif_digits), "]")

    }

    x[["effect_size_txt"]] <- paste(x[["effect_name"]],
                                    signif(x[["effect_size"]], signif_digits),
                                    sep = " = ")

    stopifnot(is.character(plot_caption))
    stopifnot(length(plot_caption) > 0)

    if(any(!plot_caption %in% names(x))) {

      stop("Components of the plot caption not found in the testing result data frame.",
           call. = FALSE)

    }

    x[["plot_caption"]] <-
      reduce(x[plot_caption], paste, sep = plot_caption_sep)

    ## the output object -------

    as_etest(x)

  }

#' @rdname etest

  is_etest <- function(x) inherits(x, "etest")

#' @rdname etest

  as_etest <- function(x) {

    if(is_etest(x)) return(x)

    etest_cols <-
      c("test", "stat_name", "stat",
        "n", "df1", "df2",
        "estimate_name", "estimate", "lower_ci", "upper_ci",
        "p_value", "p_adjust_method", "p_adjusted",
        "effect_name", "effect_size")

    if(!is.data.frame(x)) stop("`x` has to be a data frame.", call. = FALSE)

    missing_cols <- setdiff(etest_cols, names(x))

    if(length(missing_cols) > 0) {

      stop(paste("The following obligatory columns are missing:",
                 paste(missing_cols, collapse = ", ")),
           call. = FALSE)

    }

    num_cols <- c("stat",
                  "n", "df1", "df2",
                  "estimate", "lower_ci", "upper_ci",
                  "p_value", "p_adjusted", "effect_size")

    num_check <- map(x[num_cols],
                     function(v) is.na(v) | is.numeric(v))

    num_check <- map_lgl(num_check, all)

    if(any(!num_check)) {

      stop(paste("The following columns are not numeric or NA:",
                 paste(num_cols[!num_check]), collapse = ", "),
           call. = FALSE)

    }

    ## the output ------

    x <- as_tibble(x)

    structure(x, class = c("etest", class(x)))

  }

# END -------
