# Comparison of variables between analysis groups in a data frame

#' Compare variable's expected values between analysis groups.
#'
#' @description
#' Function `compare_variables`
#'
#' @details
#' The argument `type` specifies the type of statistical hypothesis test and
#' effect size statistic.
#' If `type = NULL`, distributions of factors between the analysis groups are
#' compared with chi-square tests with Cramer's V effect size statistics,
#' distributions of numeric variables are compared with Mann-Whitney tests
#' (two categories, biserial r effect size statistic) or Kruskal-Wallis tests
#' (eta-squared effect size statistic).
#' Available types and the corresponding testing functions are:
#'
#' * `"t"`: Student's T tests with \code{\link[fastTest]{f_t_test}}.
#' The effect size statistic is Cohen's d.
#'
#' * `"welch_t"`: Welch's T test, i.e. T test without assumption of
#' equality of variances, with \code{\link[fastTest]{f_t_test}}. The effect
#' size statistic is Cohen's d.
#'
#' * `"paired_t"`; paired T test with \code{\link[fastTest]{f_t_test}}.
#' The effect size statistic is Cohen's d.
#'
#' * `"wilcoxon"`: Mann-Whitney or Wilcoxon test with
#' \code{\link[fastTest]{f_wilcox_test}}. The effect size statistic is biserial
#' rank correlation coefficient r.
#'
#' * `"paired_wilcoxon"`: paired Wilcoxon test with \code{\link[fastTest]{f_wilcox_test}}.
#' The effect size statistic is biserial rank correlation coefficient r.
#'
#' * `"one_anova"`: one-way analysis of variance (ANOVA) with \code{\link[fastTest]{f_one_anova}}.
#' The effect size statistic is eta-square.
#'
#' * `"kruskal"`: Kruskal-Wallis test with \code{\link[fastTest]{f_kruskal_test}}.
#' The effect size statistic is eta-square.
#'
#' * `"chisq"`: chi-square test with \code{\link[fastTest]{f_chisq_test}}. The effect
#' size statistic is Cramer's V.
#'
#' If `split_factor` variable defines more than two analysis groups and a test
#' comparing two categories is chosen, only the first two categories are compared
#' and a warning is raised.
#' Please note that for paired tests, the observations need to be sorted by levels
#' of a block-defining variable such as an identifier of an individual or a measurement
#' series.
#' The functions tests in a "safely" mode: test failures raise a warning
#' and are excluded from the output.
#'
#' @return
#' If `pub_styled = FALSE`, the function returns the native output of the
#' testing function from `fastTest` package as a named list.
#' If `pub_styled = TRUE`, the function returns a data frame of class
#' \code{\link{etest}} storing the testing results and formatted text features
#' which may be used in scientific plots and tables.
#'
#' @inheritParams correlate_variables
#' @param data a data frame.
#' @param variables a character vector of names of variables, whose expected
#' values will be compared between the analysis groups defined by `split_factor`.
#' If `variables = NULL`, all variables in the data frame except of the one
#' specified by `split_factor` will be analyzed.
#' @param split_factor a character string specifying the name of the splitting
#' factor , i.e. variable whose categories define the analysis groups.
#' @param type `NULL` or a character vector of the same length as `variables`
#' which specifies types of statistical hypothesis tests and effect size metrics.
#' For available tests and behavior when `type = NULL` see Details.
#' @param ... additional arguments passed to the testing functions
#' and function \code{\link{etest}} used to format the results
#' when `pub_styled = TRUE`.
#'
#' @md
#' @export

  compare_variables <- function(data,
                                variables = NULL,
                                split_factor,
                                type = NULL,
                                adj_method = "none",
                                pub_styled = TRUE, ...) {

    ## input control --------

    data <- validate_tst_df(data, variables, split_factor, coerce = TRUE)

    stopifnot(is.logical(pub_styled))
    pub_styled <- pub_styled[1]

    variable_types <- attr(data, "variable_format")

    variables <- names(variable_types)

    split_levs <- levels(data[[split_factor]])

    dots <- list2(...)

    if(!is.null(type)) {

      if(!is.character(type)) {

        stop("`type` must be `NULL` a character vector.", call. = FALSE)

      }

    }

    ## types and testing functions -------

    num_types <- c("t", "welch_t", "paired_t",
                   "wilcoxon", "paired_wilcoxon",
                   "one_anova", "kruskal")

    fct_types <- c("chisq")

    if("as_data_frame" %in% names(dots)) {

      tst_fun <-
        list(t = function(x, f) f_t_test(x, f, type = "standard", ...),
             welch_t = function(x, f) f_t_test(x, f, type = "welch", ...),
             paired_t = function(x, f) f_t_test(x, f, type = "paired", ...),
             wilcoxon = function(x, f) f_wilcox_test(x, f, type = "standard", ...),
             paired_wilcoxon = function(x, f) f_wilcox_test(x, f, type = "paired", ...),
             one_anova = function(x, f) f_one_anova(x, f, ...),
             kruskal = function(x, f) f_kruskal_test(x, f, ...),
             chisq = function(x, f) f_chisq_test(x, f, ...))

    } else {

      tst_fun <-
        list(t = function(x, f) f_t_test(x, f, type = "standard", as_data_frame = TRUE, ...),
             welch_t = function(x, f) f_t_test(x, f, type = "welch", as_data_frame = TRUE, ...),
             paired_t = function(x, f) f_t_test(x, f, type = "paired", as_data_frame = TRUE, ...),
             wilcoxon = function(x, f) f_wilcox_test(x, f, type = "standard", as_data_frame = TRUE, ...),
             paired_wilcoxon = function(x, f) f_wilcox_test(x, f, type = "paired", as_data_frame = TRUE, ...),
             one_anova = function(x, f) f_one_anova(x, f, as_data_frame = TRUE, ...),
             kruskal = function(x, f) f_kruskal_test(x, f, as_data_frame = TRUE, ...),
             chisq = function(x, f) f_chisq_test(x, f, as_data_frame = TRUE, ...))

    }

    ## test type management --------

    if(is.null(type)) {

      if(length(split_levs) == 2) num_type <- "wilcoxon" else num_type <- "kruskal"

      type <- ifelse(variable_types == "factor",
                     "chisq", num_type)

    }

    unknown_types <- type[!type %in% c(num_types, fct_types)]

    if(length(unknown_types) > 0) {

      stop(paste("Uknown test types:", paste(unknown_types, collapse = ", "),
                 "\n",
                 "The available types are:",
                 paste(c(num_types, fct_types), collapse = ", ")),
           call. = FALSE)

    }

    if(length(unique(type)) == 1) {

      type <- rep(unique(type), length(variables))

    }

    if(length(variables) != length(type)) {

      stop("Lengths of `variables` and `type` must be equal.", call. = FALSE)

    }

    if(any(type %in% c("t", "welch_t", "paired_t", "wilcoxon", "paired_wilcoxon"))) {

      if(length(split_levs) > 2) {

        warning(paste("There are more than two categories of the splitting factor.",
                      "Two sample tests will compare",
                      paste(split_levs[c(2, 1)], collapse = " and ")),
                call. = FALSE)

      }

    }

    ## tests -----------

    results <-
      pmap(list(fun = tst_fun[type],
                vars = variables),
           function(fun, vars) safely(fun)(x = data[[vars]],
                                           f = data[[split_factor]], ...))

    results <- set_names(results, variables)

    results <- compact(map(results, ~.x$result))

    test_failures <- setdiff(variables, names(results))

    if(length(test_failures) == length(variables)) {

      stop("All caclulations failed. Please consult the warnings.",
           call. = FALSE)

    }

    if(length(test_failures) > 0) {

      warning(paste("Calculation failed for the following variables:",
                    paste(test_failures, collapse = ", ")),
              call. = FALSE)

    }

    test_successes <- variables %in% names(results)

    variables <- variables[test_successes]
    type <- type[test_successes]

    p_values <- map_dbl(results, ~.x[, "p_value", drop = TRUE])
    p_adjusted <- p.adjust(p_values, method = adj_method)

    variable <- NULL
    test_type <- NULL

    results <- map(results, as.data.frame)

    results <-
      pmap(list(x = results,
                y = variables,
                z = type,
                v = p_adjusted),
           function(x, y, z, v) cbind(x,
                                      variable = y,
                                      test_type = z,
                                      p_adjusted = v))

    if(!pub_styled) return(results)

    ## publication-styled output --------

    test_names <-
      c("t" = "Student's T test",
        "welch_t" = "Welch's T test",
        "paired_t" = "paired T test",
        "wilcoxon" = "Mann-Whitney/Wilcoxon test",
        "paired_wilcoxon" = "paired Wilcoxon test",
        "one_anova" = "one-way ANOVA",
        "kruskal" = "Kruskal-Wallis test",
        "chisq" = "\u03C7\u00B2 test")

    stat_names <-
      c("t" = "t",
        "welch_t" = "t",
        "paired_t" = "t",
        "wilcoxon" = "U",
        "paired_wilcoxon" = "U",
        "one_anova" = "F",
        "kruskal" = "h",
        "chisq" = "\u03C7\u00B2")

    stat_values <-
      c("t" = "t",
        "welch_t" = "t",
        "paired_t" = "t",
        "wilcoxon" = "u",
        "paired_wilcoxon" = "u",
        "one_anova" = "f",
        "kruskal" = "h",
        "chisq" = "chisq")

    n_values <-
      list("t" = c("n1", "n2"),
           "welch_t" = c("n1", "n2"),
           "paired_t" = c("n1", "n2"),
           "wilcoxon" = c("n1", "n2"),
           "paired_wilcoxon" = c("n1", "n2"),
           "one_anova" = "n",
           "kruskal" = "n",
           "chisq" = "n")

    estimate_names <-
      c("t" = "difference of means",
        "welch_t" = "difference of means",
        "paired_t" = "difference of means",
        "wilcoxon" = "difference of medians",
        "paired_wilcoxon" = "difference of medians",
        "one_anova" = NA,
        "kruskal" = NA,
        "chisq" = NA)

    eff_names <-
      c("t" = "d",
        "welch_t" = "d",
        "paired_t" = "d",
        "wilcoxon" = "r",
        "paired_wilcoxon" = "r",
        "one_anova" = "\u03B7\u00B2",
        "kruskal" = "\u03B7\u00B2",
        "chisq" = "V")

    eff_values <-
      c("t" = "cohen_d",
        "welch_t" = "cohen_d",
        "paired_t" = "cohen_d",
        "wilcoxon" = "biserial_r",
        "paired_wilcoxon" = "biserial_r",
        "one_anova" = "etasq",
        "kruskal" = "etasq",
        "chisq" = "cramer_v")

    dfs <-
      ### extraction of primary degrees of freedom
      map(results,
          function(x) {

            if("df" %in% names(x)) return(x[["df"]])
            if("df1" %in% names(x)) return(x[["df1"]])
            return(NA)

          })

    pub_results <-
      pmap_dfr(list(x = results,
                    y = type,
                    z = dfs),
               function(x, y, z, deg)
                 etest(test = test_names[[y]],
                       stat_name = stat_names[[y]],
                       stat = x[[stat_values[[y]]]],
                       n = reduce(x[n_values[[y]]], `+`),
                       df1 = z,
                       df2 = if("df2" %in% names(x)) x[["df2"]] else NA,
                       estimate_name = estimate_names[[y]],
                       estimate = if("estimate" %in% names(x)) x[["estimate"]] else NA,
                       lower_ci = if("lower_ci" %in% names(x)) x[["lower_ci"]] else NA,
                       upper_ci = if("lower_ci" %in% names(x)) x[["lower_ci"]] else NA,
                       p_value = x[["p_value"]],
                       p_adjust_method = adj_method,
                       p_adjusted = x[["p_adjusted"]],
                       effect_name = eff_names[[y]],
                       effect_size = x[[eff_values[[y]]]], ...))

    variable <- NULL

    return(as_etest(cbind(variable = variables,
                          pub_results)))

  }

# END ------
