# Explore the variables of a data frame -----

#' Explorative data analysis of data frame variables.
#'
#' @description Performs exploratory data analysis for the selected variables of a data frame.
#' The output may include lists of distribution statistics, a publication-ready table with the distribution statistics
#' or distribution plots.
#' @param data a data frame.
#' @param variables a vector of variable names.
#' @param what the type of output: 'list' returns a list of distribution statistics, 'table' returns
#' a publication-ready table with the distribution stats, 'plots' returns a list of plots,
#' 'raw' returns a list of EDA objects (\code{\link{eda}}).
#' @param ... additional arguments passed to \code{\link{summary.eda}} or \code{\link{plot.eda}}.
#' @return as specified by the 'what' argument: a list of stats, a statistic table or a list of plots.
#' @export

  explore <- function(data,
                      variables = names(data),
                      what = c('list', 'table', 'plota', 'raw'), ...) {

    ## entry control

    if(!any(class(data) == 'data.frame')) stop('Please provide a data frame as data.', call. = FALSE)

    if(!all(variables %in% names(data))) stop('Some variables are missing from data.', call. = FALSE)

    what <- match.arg(what[1], c('list', 'table', 'plots', 'raw'))

    ## EDA object list

    edas <- purrr::map(data[variables], exda::eda)

    switch(what,
           list = purrr::map(edas, summary, pub_styled = FALSE, ...),
           table = dplyr::mutate(purrr::map_dfr(edas, summary, pub_styled = TRUE, ...),
                                 variable = variables)[c('variable', 'statistic')],
           plots = purrr::pmap(list(eda_object = edas,
                                    plot_title = variables),
                               plot, ...),
           raw = edas)

  }

# Compare or correlate the selected variable between two or more collectives ------

#' Compare or correlate features between collectives/cohorts.
#'
#' @description Compares the given variable between the provided data sets
#' with a range of statistical sets as described for \code{\link{test}} (statistical testing),
#' \code{\link{distribution}} (distribution comparison), \code{\link{variance}} (variance comparison),
#' \code{\link{correlate}} (correlation) or \code{\link{covariance}} (covariance).
#' @param ... data frames.
#' @param variable a variable name.
#' @param what the requested analysis: 'test', 'distribution', 'variance', 'correlation' or 'covariance'. Defaults to 'test'.
#' @param type type of statistical test, see \code{\link{test}}, \code{\link{correlate}} and
#' \code{\link{covariance}}  for details.
#' @param exact logical, should exact values for Chi-squared. Mann-Whitney and Wilcoxon test be returned?
#' @param ci logical, should confidence intervals for the test effect size be returned?
#' @param boot_method indicates how the bootstrap confidence intervals are calculated.
#' Can be any of 'percentile', 'bca', or 'normality', defaults to 'percentile'.
#' @param pub_styled logical, should the output be publication-ready formatted? See the details.
#' @param signif_digits significant digits used for rounding in the publication-style output.

  compare <- function(...,
                      variable,
                      what = c('test', 'distribution', 'variance', 'correlation', 'covariance'),
                      type = 't_test',
                      exact = TRUE,
                      ci = TRUE,
                      boot_method = 'percentile',
                      pub_styled = FALSE,
                      signif_digits = 2) {

    ## entry control

    stopifnot(is.logical(exact))
    stopifnot(is.logical(ci))

    what <- match.arg(what[1],
                      c('test', 'distribution', 'variance', 'correlation', 'covariance'))

    inp_list <- rlang::list2(...)

    classes <- purrr::map_lgl(inp_list,
                              ~any(class(.x) == 'data.frame'))

    if(any(!classes)) stop('Please provide data frames as an analysis input.', call. = FALSE)

    if(length(inp_list) < 2) stop('At least two data frames required.', call. = FALSE)

    vars <- purrr::map(inp_list, names)

    vars <- purrr::reduce(vars, intersect)

    if(!variable %in% vars) stop('Variable absent from the provided data sets.', call. = FALSE)

    if(length(inp_list) > 2) {

      if(type %in% c('correlation', 'covariance', 'distribution')) {

        warning('Mode than two data sets provided. Only the first two will be used for the requested analysis.', call. = FALSE)

      }

    }

    ## analysis

    inp_list <- purrr::map(inp_list, ~eda(.x[[variable]]))

    test_res <- switch(what,
                       test = exda::test(!!!inp_list,
                                         type = type,
                                         exact = exact,
                                         ci = ci,
                                         boot_method = boot_method),
                       distribution = exda::distribution(eda_object = inp_list[[1]],
                                                         y = inp_list[[2]]),
                       variance = exda::variance(!!!inp_list),
                       correlation = exda::correlation(eda_object = inp_list[[1]],
                                                       y = inp_list[[2]],
                                                       type = type,
                                                       ci = ci),
                       covariance = exda::covariance(eda_object = inp_list[[1]],
                                                     y = inp_list[[2]],
                                                     type = type))


    summary(test_res,
            pub_styled = pub_styled,
            signif_digits = signif_digits)

  }

# Compare or correlate the selected variable between two or more collectives ----

#' Compare or correlate features between collectives/cohorts.
#'
#' @description Compares the given variable between the provided data sets
#' with a range of statistical sets as described for \code{\link{test}} (statistical testing),
#' \code{\link{distribution}} (distribution comparison), \code{\link{variance}} (variance comparison),
#' \code{\link{correlate}} (correlation) or \code{\link{covariance}} (covariance).
#' @param ... data frames.
#' @param variables a vector with variable names.
#' @param what the requested analysis: 'test', 'distribution', 'variance', 'correlation' or 'covariance'. Defaults to 'test'.
#' @param types a vector with the types of statistical test, see \code{\link{test}}, \code{\link{correlate}} and
#' \code{\link{covariance}} for details. The vector length must be either one or the length of the 'variables' vector.
#' @param exact logical, should exact values for Chi-squared. Mann-Whitney and Wilcoxon test be returned?
#' @param ci logical, should confidence intervals for the test effect size be returned?
#' @param boot_method indicates how the bootstrap confidence intervals are calculated.
#' Can be any of 'percentile', 'bca', or 'normality', defaults to 'percentile'.
#' @param pub_styled logical, should the output be publication-ready formatted? See the details.
#' @param signif_digits significant digits used for rounding in the publication-style output.
#' @param adj_method the method for adjusting p values for multiple testing, as defined for \code{\link[stats]{p.adjust}},
#' defaults to 'none'. The adjusted p value is summarized in the 'significance' column, when pub_style output is chosen.
#' @param simplify_p logical, should p_values < 0.001 be presented in a p < 0.001 form?
#' @export

  compare_variables <- function(...,
                                variables,
                                what = c('test', 'distribution', 'variance', 'correlation', 'covariance'),
                                types = 't_test',
                                exact = TRUE,
                                ci = TRUE,
                                boot_method = 'percentile',
                                pub_styled = FALSE,
                                signif_digits = 2,
                                adj_method = 'none',
                                simplify_p = TRUE) {

    test_res <- purrr::pmap_dfr(list(x = variables,
                                     y = types),
                                function(x, y) exda:::compare(...,
                                                              variable = x,
                                                              what = what,
                                                              type = y,
                                                              exact = exact,
                                                              ci = ci,
                                                              boot_method = boot_method,
                                                              pub_styled = pub_styled,
                                                              signif_digits = signif_digits))

    test_res <- dplyr::mutate(test_res,
                              variable = variables,
                              p_adjusted = p.adjust(p_value, adj_method))

    if(simplify_p) {

      test_res <- dplyr::mutate(test_res,
                                significance = ifelse(p_adjusted < 0.001,
                                                      'p < 0.001',
                                                      ifelse(p_adjusted < 0.05,
                                                             paste('p =', signif(p_adjusted, signif_digits)),
                                                             paste0('ns (p = ', signif(p_adjusted, signif_digits), ')'))))

    } else {

      test_res <- dplyr::mutate(test_res,
                                significance = ifelse(p_adjusted < 0.05,
                                                      paste('p =', signif(p_adjusted, signif_digits)),
                                                      paste0('ns (p = ', signif(p_adjusted, signif_digits), ')')))

    }

    test_res[c('variable', names(test_res)[names(test_res) != 'variable'])]

  }
