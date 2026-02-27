# Normality and comparison of distribution of variables

#' Normality and comparison of distribution of variables in a data frame.
#'
#' @description
#' Function `check_normality()` performs standard Shapiro-Wilk test for normality
#' of distribution of variables in a data frame, optionally in subset defined
#' by categories of a splitting factor variables.
#' Function `compare_distributions()` compares distribution of variables in a
#' data frame - pairwise or between two groups defined by categories of a
#' splitting factor variable - with two-sample asymptotic (not exact)
#' Kolmogorov-Smirnov test.
#' Function `compare_variances()` compares variances of variables in a data frame
#' between categories of a splitting factor variable with Levene test.
#'
#' @details
#' The functions employ tools of `fastTest` package:
#' \code{\link[fastTest]{f_shapiro_test}},
#' \code{\link[fastTest]{f_ks_test}}, and \code{\link[fastTest]{f_levene_test}}.
#'
#' Please note that functions `check_normality()`, `compare_distributions()`, and
#' `compare_variances()` as exploratory tools usually used to detect violations
#' of assumptions of statistical tests do not offer automatic corrections of
#' p values for multiple testing.
#'
#' `check_normality()`: if `split_factor = NULL`, normality of the distributions
#' is tested for the entire data frame.
#' `compare_distributions()`: if `split_factor = NULL`, distributions of
#' the variables of interest are compared in a pairwise manner.
#'
#' @return a data frame with the testing results: either in a native form
#' of the `fastTest` functions or, if `pub_styled = TRUE`, in a pre-formatted
#' form with significance and effect size texts, and ready-to-use plot labels.
#'
#' @inheritParams correlate_variables
#' @param data a data frame.
#' @param variables a vector with names of variables for which the correlations
#' will be computed (one against all other). Note, all variables need to be numeric.
#' If `NULL`, all variables in the table except of the splitting factor are
#' investigated.
#' @param split_factor name of the splitting factor variable. For `check_normality()`
#' and `compare_distributions()` it can be also `NULL`, see Details.
#' @param ... additional arguments passed to the testing functions
#' \code{\link[fastTest]{f_shapiro_test}}, \code{\link[fastTest]{f_ks_test}} and
#' \code{\link[fastTest]{f_levene_test}}, and the function \code{\link{etest}}
#' used to format the testing results in a publication-styled manner.
#'
#' @export

  check_normality <- function(data,
                              variables = NULL,
                              split_factor = NULL,
                              pub_styled = TRUE, ...) {

    ## input control -------

    stopifnot(is.logical(pub_styled))
    pub_styled <- pub_styled[1]

    data <-
      validate_tst_df(data, variables, split_factor, coerce = TRUE)

    var_formats <- attr(data, "variable_format")

    variables <- names(var_formats)

    non_numeric_variables <-
      variables[var_formats != "numeric"]

    if(length(non_numeric_variables) > 0) {

      stop(paste("The following variables are not numeric:",
                 paste(non_numeric_variables, collapse = ", ")),
           call. = FALSE)

    }

    ## testing ----------

    if(is.null(split_factor)) {

      result <- f_shapiro_test(x = data[, variables],
                               as_data_frame = TRUE)

      result <- as_tibble(result)

    } else {

      result <- f_shapiro_test(x = data[, variables],
                               f = data[[split_factor]],
                               as_data_frame = TRUE)

      variable <- NULL

      result <-
        map2_dfr(result, names(result),
                 ~mutate(.x,
                         variable = .y,
                         !!split_factor := factor(.data[["f"]],
                                                  levels(data[[split_factor]]))))

      result <- relocate(result,
                         .data[["variable"]],
                         .data[[split_factor]])

    }

    ## formatting of the testing results --------

    if(!pub_styled) return(result)

    pub_result <-
      etest(test = "Shapiro-Wilk test for normality",
            stat_name = "W",
            stat = result[["w"]],
            n = result[["n"]],
            p_value = result[["p_value"]],
            p_adjust_method = "none",
            p_adjusted = result[["p_value"]],
            effect_name = "W",
            effect_size = result[["w"]])

    if(is.null(split_factor)) {

      pub_result <- cbind(result[, "variable"], pub_result)

    } else {

      pub_result <- cbind(result[, c("variable", split_factor)],
                          pub_result)

    }

    return(as_etest(pub_result))

  }

#' @rdname check_normality
#' @export

  compare_variances <- function(data,
                                variables = NULL,
                                split_factor,
                                pub_styled = TRUE, ...) {

    ## input controls ---------

    stopifnot(is.logical(pub_styled))
    pub_styled <- pub_styled[1]

    data <- validate_tst_df(data, variables, split_factor, coerce = TRUE)

    var_formats <- attr(data, "variable_format")

    variables <- names(var_formats)

    non_numeric_variables <-
      variables[var_formats != "numeric"]

    if(length(non_numeric_variables) > 0) {

      stop(paste("The following variables are not numeric:",
                 paste(non_numeric_variables, collapse = ", ")),
           call. = FALSE)

    }

    ## testing --------

    result <-
      f_levene_test(x = data[, variables],
                    f = data[[split_factor]],
                    as_data_frame = TRUE,
                    safely = TRUE, ...)

    result <- as_tibble(result)

    if(!pub_styled) return(result)

    pub_test <- etest(test = "Levene test",
                      stat_name = "F",
                      stat = result[["f"]],
                      n = result[["n"]],
                      df1 = result[["df1"]],
                      df2 = result[["df2"]],
                      p_value = result[["p_value"]],
                      p_adjust_method = "none",
                      p_adjusted = result[["p_value"]],
                      effect_name = "F",
                      effect_size = result[["f"]], ...)

    as_etest(cbind(result[, "variable"],
                   pub_test))

  }

#' @rdname check_normality
#' @export

  compare_distributions <- function(data,
                                    variables = NULL,
                                    split_factor = NULL,
                                    pub_styled = TRUE, ...) {

    ## entry control --------

    stopifnot(is.logical(pub_styled))

    data <-
      validate_tst_df(data, variables, split_factor, coerce = TRUE)

    var_formats <- attr(data, "variable_format")

    variables <- names(var_formats)

    non_numeric_variables <-
      variables[var_formats != "numeric"]

    if(length(non_numeric_variables) > 0) {

      stop(paste("The following variables are not numeric:",
                 paste(non_numeric_variables, collapse = ", ")),
           call. = FALSE)

    }

    ## cases with more than two categories of the splitting factor ------

    if(!is.null(split_factor)) {

      split_levs <- levels(data[[split_factor]])

      if(length(split_levs) > 2) {

        data <- filter(data, .data[[split_factor]] %in% split_levs[1:2])

        data[[split_factor]] <- droplevels(data[[split_factor]])

        warning(paste("More than two categories of the splitting factor.",
                      "The first two categories:",
                      paste(split_levs[1:2], collapse = ", "),
                      "will be compared."),
                call. = FALSE)

      }

    }

    ## testing ---------

    if(is.null(split_factor)) {

      result <- f_ks_test(x = data[, variables],
                          as_data_frame = TRUE, ...)

    } else {

      result <- f_ks_test(x = data[, variables],
                          f = data[[split_factor]],
                          as_data_frame = TRUE, ...)

    }

    result <- as_tibble(result)

    if(!pub_styled) return(result)

    ## publication-styled output -------

    pub_result <-
      etest(test = "Kolmogorov-Smirnov test",
            stat_name = "d",
            stat = result[["d"]],
            n = result[["n1"]] + result[["n2"]],
            p_value = result[["p_value"]],
            p_adjust_method = "none",
            p_adjusted = result[["p_value"]],
            effect_name = "d",
            effect_size = result[["d"]])

    if(is.null(split_factor)) {

      pub_result <-
        cbind(result[, c("variable1", "variable2", "n1", "n2", "ties")],
              pub_result)

    } else {

      pub_result <-
        cbind(result[, c("variable", "n1", "n2", "ties")],
              pub_result)

    }

    return(as_etest(pub_result))

  }

# END --------
