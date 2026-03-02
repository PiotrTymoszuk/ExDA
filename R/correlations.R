# Functions for computing pairwise correlation tests.

# Correlation and covariance coefficients ---------

#' Correlation and covariance coefficients.
#'
#' @description
#' Pairwise correlation and covariance coefficients.
#'
#' @return
#' If `pub_styled = FALSE`: the raw output of the covariance and correlation
#' computing functions.
#' If `pub_styled = TRUE`, a data frame of class \code{\link{etest}} with the
#' covariance or correlation coefficients, or Cohen's kappas is returned.
#'
#' @details
#' The function uses internally \code{\link[fastTest]{f_cov}},
#' \code{\link[fastTest]{f_cor}}, and \code{\link[fastTest]{f_kappa}}.
#' If `type = NULL`, `cov_variables()` resorts to Spearman's covariance for
#' numeric variables.
#' If `type = NULL`, `cor_variables()` resorts to Spearman's correlations
#' for numeric variables, and Cohen's kappa for integers and factors.
#'
#' @inheritParams correlate_variables
#' @param type `NULL` or a string which specifies type of co-variances or
#' correlations.
#' If `NULL`, Spearman's covariance or correlations are computed for numeric
#' variables, and Cohen's kappa for factors or integers.
#' Other options are `"pearson"`, `"spearman"` covariance for numeric variables,
#' `"pearson"`, `"spearman"`, `"kendallA"`, `"kendallB"`, `"xiaA"`, `"xiB"`
#' for correlation of numeric variables, and `"kappa"` for factors or integers.
#' @param ... additional arguments passed to functions computing the statistics
#' (\code{\link[fastTest]{f_cov}}, \code{\link[fastTest]{f_cor}},
#' \code{\link[fastTest]{f_kappa}}), and function \code{\link{etest}} used
#' to format the results when `pub_styled = TRUE`.
#'
#' @export

  cov_variables <- function(data,
                            variables = NULL,
                            type = NULL,
                            pub_styled = TRUE, ...) {

    ## input control ---------

    data <- validate_tst_df(data,
                            variables,
                            split_factor = NULL,
                            minimal = TRUE)

    num_check <- map_lgl(data, is.numeric)

    if(any(!num_check)) {

      stop("All variables must be numeric.", call. = FALSE)

    }

    stopifnot(is.logical(pub_styled))
    pub_styled <- pub_styled[1]

    if(is.null(type)) type <- "spearman"

    type <- match.arg(type[1], c("pearson", "spearman"))

    dots <- list2(...)

    ## covariance ---------

    if(!"as_data_frame" %in% names(dots)) {

      result <- f_cov(data, type, as_data_frame = TRUE, ...)

    } else {

      result <- f_cov(data, type, ...)

    }

    if(!pub_styled) return(result)

    ## publication-styled output --------

    test_name <- switch(type,
                        spearman = "Spearman's covariance",
                        pearson = "Pearson's covariance")

    if("plot_caption" %in% names(dots)) {

      pub_result <- etest(test = test_name,
                          stat_name = "Cov",
                          stat = result[["cov"]],
                          n = result[["n"]], ...)

    } else {

      pub_result <- etest(test = test_name,
                          stat_name = "Cov",
                          stat = result[["cov"]],
                          n = result[["n"]],
                          plot_caption = c("n_txt", "stat_txt"),
                          plot_caption_sep = ", ", ...)

    }

    return(as_etest(cbind(result[, c("variable1", "variable2")],
                          pub_result)))

  }

#' @rdname cov_variables
#' @export

  cor_variables <- function(data,
                            variables = NULL,
                            type = NULL,
                            pub_styled = TRUE, ...) {

    ## input controls -------

    data <- validate_tst_df(data,
                            variables,
                            split_factor = NULL,
                            minimal = TRUE)

    stopifnot(is.logical(pub_styled))
    pub_styled <- pub_styled[1]

    dots <- list2(...)

    ## type consistency and type re-routing ----------

    num_types <- c("pearson", "spearman",
                   "kendallA", "kendallB",
                   "xiA", "xiB")

    fct_types <- "kappa"

    if(!is.null(type)) type <- match.arg(type[1], c(num_types, fct_types))

    ### type consistency

    fct_tst <- map_lgl(data, is.factor)
    int_tst <- map_lgl(data, is.integer)
    num_tst <- map_lgl(data, is.numeric)

    types_consistent <- FALSE

    if(all(fct_tst)) {

      types_consistent <- TRUE

    } else if(all(int_tst)) {

      types_consistent <- TRUE

    } else if(all(num_tst)) {

      types_consistent <- TRUE

    }

    if(!types_consistent) {

      stop(paste("All variables need to be of the same type:",
                 "either factor, integer, or numeric."),
           call. = FALSE)

    }

    if(all(fct_tst)) {

      if(is.null(type)) type <- "kappa"

      if(!type %in% fct_types) {

        stop(paste("The correlation type", type,
                   "is not available for factors.",
                   "Please choose NULL or kappa instead."),
             call. = FALSE)

      }

    } else if(all(int_tst)) {

      if(is.null(type)) type <- "kappa"

    } else {

      if(is.null(type)) type <- "spearman"

      if(!type %in% num_types) {

        stop(paste("The correlation type", type,
                   "is not available for numeric variables.",
                   "Please choose NULL or",
                   paste(num_types, collapse = ", "),
                   "instead."),
             call. = FALSE)

      }

    }

    ## correlation coefficients -------

    if(!pub_styled) {

      if(type == "kappa") {

        if("as_data_frame" %in% names(dots)) {

          return(f_kappa(data, ...))

        } else {

          return(f_kappa(data, as_data_frame = TRUE, ...))

        }

      } else {

        if("as_data_frame" %in% names(dots)) {

          return(f_cor(data, method = type, ...))

        } else {

          return(f_kappa(data, method = type, as_data_frame = TRUE, ...))

        }

      }

    } else {

      if(type == "kappa") {

        result <- f_kappa(data, as_data_frame = TRUE, ...)

        result[["cor"]] <- result[["kappa"]]

      } else {

        result <- f_cor(data, method = type, as_data_frame = TRUE, ...)

      }

    }

    ## publication-styled output --------

    test_name <- switch(type,
                        kappa = "Cohen's kappa inter-rater reliability",
                        spearman = "Spearman's correlation",
                        pearson = "Pearson's correlation",
                        kendallA = "Kendall's correlation",
                        kendallB = "Kendall's correlation",
                        xiA = "Chatterjee's correlation",
                        xiB = "Chatterjee's correlation")

    stat_name <- switch(type,
                        kappa = "\u03BA",
                        pearson = "r",
                        spearman = "\u03C1",
                        kendallA = "\u03C4A",
                        kendallB = "\u03C4B",
                        xiA = "\u03BEA",
                        xiB = "\u03BEB")

    if("plot_caption" %in% names(dots)) {

      pub_result <- etest(test = test_name,
                          stat_name = stat_name,
                          stat = result[["cor"]],
                          n = result[["n"]], ...)

    } else {

      pub_result <- etest(test = test_name,
                          stat_name = stat_name,
                          stat = result[["cor"]],
                          n = result[["n"]],
                          plot_caption = c("n_txt", "stat_txt"),
                          plot_caption_sep = ", ", ...)

    }

    return(as_etest(cbind(result[, c("variable1", "variable2")],
                          pub_result)))


  }

# Correlation tests ---------

#' Pairwise correlation tests.
#'
#' @description
#' Tests for significant correlations of two numeric variables (methods: Pearson,
#' Spearman, Kendall TauA and TauB, Chatterjee's XiA and XiB) or two integer/factor
#' variables (Cohen's kappa inter-rater reliability statistic).
#' P values are computed with the permutation or bootstrap method.
#'
#' @details
#' The function uses internally \code{\link[fastTest]{f_cor_test}} and
#' \code{\link[fastTest]{f_kappa_test}}, for a richer set of correlation options,
#' please refer to the genuine correlation functions.
#' If `type = NULL`, `correlate_variables()` resorts to Spearman's correlations
#' for numeric variables and Cohen's kappa for factors or integers.
#'
#' @return a data frame with the correlation results: either in a native form
#' of the `fastTest` functions or, if `pub_styled = TRUE`, in a pre-formatted
#' form with significance and effect size texts, and ready-to-use plot labels.
#'
#' @param data a data frame.
#' @param variables a vector with names of variables for which the correlations
#' will be computed (one against all other). Note, all variables need to be of
#' the same type (e.g. all numeric). If `NULL`, all variables in the table
#' are correlated.
#' @param type `NULL` or a string which specifies type of the correlations.
#' If `NULL`, Spearman's correlations are computed for numeric variables, and
#' Cohen's kappa for factors or integers. Other options are
#' `"pearson"`, `"spearman"`, `"kendallA"`, `"kendallB"`, `"xiaA"`, `"xiB"`
#' for numeric variables, and `"kappa"` for factors or integers.
#' @param test_method method of computing p values: "permutation" (default) or
#' "bootstrap".
#' @param alternative type of the alternative hypothesis concerning correlation
#' coefficient sign. Ignored if `type = "bootstrap"`.
#' @param boot_method indicates how the bootstrap confidence intervals are
#' calculated.
#' Can be `"bca"` for bias-corrected accelerated confidence intervals
#' (default) or `"percentile"` for percentile confidence intervals.
#' @param conf_level confidence level used for computation of the
#' confidence intervals.
#' @param n_iter number of permutation or bootstraps.
#' @param adj_method the method for adjusting p values for multiple testing,
#' as defined for \code{\link[stats]{p.adjust}},
#' defaults to `"none"`. The adjusted p value appears in the `p_adjusted` column.
#' @param pub_styled logical, should the output be returned as a publication-styled
#' data frame (class \code{\link{etest}})?
#' @param ... additional arguments passed to \code{\link[fastTest]{f_cor_test}},
#' \code{\link[fastTest]{f_kappa_test}}, and function \code{\link{etest}} used
#' to format the results when `pub_styled = TRUE`.
#'
#' @export

  correlate_variables <- function(data,
                                  variables = NULL,
                                  type = NULL,
                                  test_method = c("permutation", "bootstrap"),
                                  alternative = c("two.sided", "less", "greater"),
                                  boot_method = c("bca", "percentile"),
                                  conf_level = 0.95,
                                  n_iter = 1000,
                                  adj_method = "none",
                                  pub_styled = TRUE, ...) {

    ## input control ---------

    data <- validate_tst_df(data, variables, split_factor = NULL, minimal = TRUE)

    test_method <- match.arg(test_method[1], c("permutation", "bootstrap"))
    alternative <- match.arg(alternative[1], c("two.sided", "less", "greater"))
    boot_method <- match.arg(boot_method[1], c("bca", "percentile"))

    stopifnot(is.numeric(n_iter))
    n_iter <- as.integer(n_iter[1])

    stopifnot(is.logical(pub_styled))

    ## type consistency checks and rerouting ----------

    num_types <- c("pearson", "spearman",
                   "kendallA", "kendallB",
                   "xiA", "xiB")

    fct_types <- "kappa"

    if(!is.null(type)) type <- match.arg(type[1], c(num_types, fct_types))

    ### type consistency

    fct_tst <- map_lgl(data, is.factor)
    int_tst <- map_lgl(data, is.integer)
    num_tst <- map_lgl(data, is.numeric)

    types_consistent <- FALSE

    if(all(fct_tst)) {

      types_consistent <- TRUE

    } else if(all(int_tst)) {

      types_consistent <- TRUE

    } else if(all(num_tst)) {

      types_consistent <- TRUE

    }

    if(!types_consistent) {

      stop(paste("All variables need to be of the same type:",
                 "either factor, integer, or numeric."),
           call. = FALSE)

    }

    if(all(fct_tst)) {

      if(is.null(type)) type <- "kappa"

      if(!type %in% fct_types) {

        stop(paste("The correlation type", type,
                   "is not available for factors.",
                   "Please choose NULL or kappa instead."),
             call. = FALSE)

      }

    } else if(all(int_tst)) {

      if(is.null(type)) type <- "kappa"

    } else {

      if(is.null(type)) type <- "spearman"

      if(!type %in% num_types) {

        stop(paste("The correlation type", type,
                   "is not available for numeric variables.",
                   "Please choose NULL or",
                   paste(num_types, collapse = ", "),
                   "instead."),
             call. = FALSE)

      }

    }

    ## computing the correlations ---------

    if(type != "kappa") {

      result <- f_cor_test(x = data,
                           type = test_method,
                           method = type,
                           alternative = alternative,
                           ci_type = boot_method,
                           conf_level = conf_level,
                           as_data_frame = TRUE,
                           n_iter = n_iter,
                           adj_method = adj_method, ...)

    } else {

      result <- f_kappa_test(x = data,
                             type = test_method,
                             alternative = alternative,
                             ci_type = boot_method,
                             conf_level = conf_level,
                             as_data_frame = TRUE,
                             n_iter = n_iter,
                             adj_method = adj_method, ...)


    }

    result <- as_tibble(result)

    if(!pub_styled) return(result)

    ## publication-styled output --------

    if(type == "kappa") {

      test_txt <- "inter-rater relibility"

    } else {

      test_txt <- "correlation"

    }

    test_txt <- paste0(test_txt, ", ", test_method, " test")

    stat_name <- switch(type,
                        kappa = "\u03BA",
                        pearson = "r",
                        spearman = "\u03C1",
                        kendallA = "\u03C4A",
                        kendallB = "\u03C4B",
                        xiA = "\u03BEA",
                        xiB = "\u03BEB")

    dots <- list2(...)

    pub_result <-
      etest(test = test_txt,
            stat_name = stat_name,
            stat = result[[4]],
            n = result[["n"]],
            estimate_name = stat_name,
            estimate = result[[4]],
            lower_ci = if(test_method == "bootstrap") result[["lower_ci"]] else NA,
            upper_ci = if(test_method == "bootstrap") result[["upper_ci"]] else NA,
            p_value = result[["p_value"]],
            p_adjust_method = adj_method,
            p_adjusted = if(adj_method == "none") result[["p_value"]] else result[["p_adjusted"]],
            effect_name = stat_name,
            effect_size = result[[4]], ...)

    if(!"plot_caption" %in% dots) {

      if(test_method == "permutation") {

        pub_result[["plot_caption"]] <-
          paste0(pub_result[["n_txt"]], "\n",
                 pub_result[["estimate_txt"]], ", ",
                 pub_result[["significance"]])

      } else {

        pub_result[["plot_caption"]] <-
          paste0(pub_result[["n_txt"]], "\n",
                 pub_result[["estimate_txt"]], " ",
                 pub_result[["ci_txt"]], "\n",
                 pub_result[["significance"]])

      }

    }

    as_etest(cbind(result[, c("variable1", "variable2")],
                   pub_result))

  }

# END --------
