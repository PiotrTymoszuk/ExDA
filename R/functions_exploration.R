# Explore the variables of a data frame -----

#' Explorative data analysis of data frame variables.
#'
#' @description Performs exploratory data analysis for the selected variables of a data frame.
#' The output may include lists of distribution statistics, a publication-ready table with the distribution statistics
#' or distribution plots.
#' @param data a data frame.
#' @param split_factor optional, the name of a factor used for splitting the variables of interest into analysis groups.
#' @param variables a vector of variable names.
#' @param what the type of output: 'list' returns a list of distribution statistics, 'table' returns
#' a publication-ready table with the distribution stats, 'plots' returns a list of plots,
#' 'raw' returns a list of EDA objects (\code{\link{eda}}), 'normality' returns a table with
#' results of Shapiro-Wilk test, 'skewness' and 'kurtosis' return tables with the requested statistics.
#' @param pub_styled logical, should the output be publication-ready formatted?
#' @param signif_digits significant digits used for rounding in the publication-style output.
#' @param simplify_p logical, should p_values < 0.001 be presented in a p < 0.001 form?
#' @param ... additional arguments passed to \code{\link{summary.eda}} or \code{\link{plot.eda}}.
#' @return as specified by the 'what' argument: a list of stats, a statistic table or a list of plots.
#' @export

  explore <- function(data,
                      split_factor = NULL,
                      variables = names(data),
                      what = c('list', 'table', 'plots', 'raw', 'normality', 'skewness', 'kurtosis'),
                      pub_styled = TRUE,
                      signif_digits = 2,
                      simplify_p = TRUE, ...) {

    ## entry control

    if(!any(class(data) == 'data.frame')) stop('Please provide a data frame as data.', call. = FALSE)

    if(!all(variables %in% names(data))) stop('Some variables are missing from data.', call. = FALSE)

    what <- match.arg(what[1], c('list', 'table', 'plots', 'raw', 'normality', 'skewness', 'kurtosis'))

    if(!is.null(split_factor)) {

      if(!split_factor %in% names(data)) stop('The split_factor absent from the data.', call. = FALSE)

      if(!is.factor(data[[split_factor]])) {

        data <- dplyr::mutate(data, !!split_factor := factor(.data[[split_factor]]))

      }

      newdata <- plyr::dlply(data, split_factor)

      dots <- rlang::list2(...)

      calls <- purrr::map(newdata,
                          ~rlang::call2(.fn = 'explore',
                                        data = .x,
                                        split_factor = NULL,
                                        variables = variables[variables != split_factor],
                                        what = what,
                                        pub_styled = pub_styled,
                                        signif_digits = signif_digits,
                                        simplify_p = simplify_p,
                                        !!!dots))

      return(purrr::map(calls, eval, envir = rlang::fn_env(explore)))

    }

    ## EDA object list

    edas <- purrr::map(data[variables], exda::eda)

    output <- switch(what,
                     list = purrr::map(edas,
                                       summary,
                                       pub_styled = FALSE,
                                       signif_digits = signif_digits),
                     table = dplyr::mutate(purrr::map_dfr(edas,
                                                          summary,
                                                          pub_styled = TRUE,
                                                          signif_digits = signif_digits),
                                           variable = variables)[c('variable', 'statistic')],
                     plots = purrr::pmap(list(x = edas,
                                              plot_title = variables),
                                         exda::plot.eda, ...),
                     raw = edas)

    if(what == 'normality') {

      output <- purrr::map(edas, normality)

      output <- purrr::map_dfr(output,
                               summary,
                               pub_styled = pub_styled,
                               signif_digits = signif_digits,
                               simplify_p = simplify_p)

      output <- dplyr::mutate(output,
                              variable = variables)

      output <- output[c('variable', names(output)[names(output) != 'variable'])]

    } else if(what %in% c('skewness', 'kurtosis')) {

      stat_fun <- switch(what,
                         skewness = skewness,
                         kurtosis = kurtosis)

      stat_name <- switch(what,
                          skewness = 'skewness',
                          kurtosis = 'kurtosis')

      output <- purrr::map_dfr(edas, stat_fun)

      output <- dplyr::mutate(output,
                              variable = variables)

      output <- rlang::set_names(output[c('variable', 'value')],
                                 c('variable', stat_name))

    }

    output

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
#' @param split_factor optional, the name of a factor used for splitting the variables of interest into analysis groups.
#' @param what the requested analysis: 'test', 'distribution', 'variance', 'correlation', 'covariance' or 'eff_size'.
#' Defaults to 'test'.
#' @param type type of statistical test, see \code{\link{test}}, \code{\link{correlate}},
#' \code{\link{covariance}} or \code{\link{eff_size}}  for details.
#' @param exact logical, should exact values for Chi-squared, Mann-Whitney and Wilcoxon test be returned?
#' @param ci logical, should confidence intervals for the test effect size be returned?
#' @param boot_method indicates how the bootstrap confidence intervals are calculated.
#' Can be any of 'percentile', 'bca', or 'normality', defaults to 'percentile'.
#' @param pub_styled logical, should the output be publication-ready formatted?
#' @param signif_digits significant digits used for rounding in the publication-style output.
#' @details in case, the split_factor is provided, only the first of the input data frames will be analyzed.


  compare <- function(...,
                      variable,
                      split_factor = NULL,
                      what = c('test', 'distribution', 'variance', 'correlation', 'covariance', 'eff_size'),
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
                      c('test', 'distribution', 'variance', 'correlation', 'covariance', 'eff_size'))

    inp_list <- rlang::list2(...)

    classes <- purrr::map_lgl(inp_list,
                              ~any(class(.x) == 'data.frame'))

    if(any(!classes)) stop('Please provide data frames as an analysis input.', call. = FALSE)

    if(length(inp_list) < 2 & is.null(split_factor)) stop('At least two data frames required.', call. = FALSE)

    vars <- purrr::map(inp_list, names)

    vars <- purrr::reduce(vars, intersect)

    if(!variable %in% vars) stop('Variable absent from the provided data sets.', call. = FALSE)

    if(length(inp_list) > 2) {

      if(what %in% c('correlation', 'covariance', 'distribution')) {

        warning('More than two data sets provided. Only the first two will be used for the requested analysis.', call. = FALSE)

      }

    }

    if(!is.null(split_factor)) {

      data <- inp_list[[1]]

      if(!split_factor %in% names(data)) stop('The split_factor absent from the data.', call. = FALSE)

      if(length(inp_list) > 2) message('split_factor provided: only the first data set will be analyzed.')

      if(!is.factor(data[[split_factor]])) {

        data <- dplyr::mutate(data, !!split_factor := factor(.data[[split_factor]]))

      }

      newdata <- plyr::dlply(data, split_factor)

      return(exda:::compare(!!!newdata,
                            variable = variable,
                            what = what,
                            type = type,
                            exact = exact,
                            ci = ci,
                            boot_method = boot_method,
                            pub_styled = pub_styled,
                            signif_digits = signif_digits))

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
                                                     type = type),
                       eff_size = exda::eff_size(!!!inp_list,
                                                 type = type,
                                                 exact = exact,
                                                 ci = ci,
                                                 boot_method = boot_method))


    summary(test_res,
            pub_styled = pub_styled,
            signif_digits = signif_digits)

  }

# Compare or correlate the selected variables between two or more collectives ----

#' Compare or correlate features between collectives/cohorts.
#'
#' @description Compares the given variable between the provided data sets
#' with a range of statistical sets as described for \code{\link{test}} (statistical testing),
#' \code{\link{distribution}} (distribution comparison), \code{\link{variance}} (variance comparison),
#' \code{\link{correlate}} (correlation), \code{\link{covariance}} (covariance) or \code{\link{eff_size}} (effect size).
#' @param ... data frames.
#' @param variables a vector with variable names.
#' @param split_factor optional, the name of a factor used for splitting the variables of interest into analysis groups.
#' @param what the requested analysis: 'test', 'distribution', 'variance', 'correlation', 'covariance' or 'eff_size'.
#' Defaults to 'test'.
#' @param types a vector with the types of statistical test, see \code{\link{test}}, \code{\link{correlate}} and
#' \code{\link{covariance}} for details. The vector length must be either one or the length of the 'variables' vector.
#' @param exact logical, should exact values for Chi-squared. Mann-Whitney and Wilcoxon test be returned?
#' @param ci logical, should confidence intervals for the test effect size be returned?
#' @param boot_method indicates how the bootstrap confidence intervals are calculated.
#' Can be any of 'percentile', 'bca', or 'normality', defaults to 'percentile'.
#' @param pub_styled logical, should the output be publication-ready formatted?
#' @param signif_digits significant digits used for rounding in the publication-style output.
#' @param adj_method the method for adjusting p values for multiple testing, as defined for \code{\link[stats]{p.adjust}},
#' defaults to 'none'. The adjusted p value is summarized in the 'significance' column, when pub_style output is chosen.
#' @param simplify_p logical, should p_values < 0.001 be presented in a p < 0.001 form?
#' @param .parallel logical, should the analysis be run in parallel? Experimental, uses the parallel solutions provided by
#' furrr package.
#' @param .paropts an object created by \code{\link[furrr]{furrr_options}}, enabling i.e. provision of globals by the user.
#' @details in case, the split_factor is provided, only the first of the input data frames will be analyzed.
#' @export

  compare_variables <- function(...,
                                variables,
                                split_factor = NULL,
                                what = c('test', 'distribution', 'variance', 'correlation', 'covariance', 'eff_size'),
                                types = 't_test',
                                exact = TRUE,
                                ci = TRUE,
                                boot_method = 'percentile',
                                pub_styled = FALSE,
                                signif_digits = 2,
                                adj_method = 'none',
                                simplify_p = TRUE,
                                .parallel = FALSE,
                                .paropts = furrr::furrr_options(seed = TRUE)) {

    if(!.parallel) {

      test_res <- purrr::pmap_dfr(list(x = variables,
                                       y = types),
                                  function(x, y) exda:::compare(...,
                                                                variable = x,
                                                                split_factor = split_factor,
                                                                what = what,
                                                                type = y,
                                                                exact = exact,
                                                                ci = ci,
                                                                boot_method = boot_method,
                                                                pub_styled = pub_styled,
                                                                signif_digits = signif_digits))

    } else {

      future::plan('multisession')

      test_res <- furrr::future_pmap_dfr(list(x = variables,
                                              y = types),
                                         function(x, y) exda:::compare(...,
                                                                       variable = x,
                                                                       split_factor = split_factor,
                                                                       what = what,
                                                                       type = y,
                                                                       exact = exact,
                                                                       ci = ci,
                                                                       boot_method = boot_method,
                                                                       pub_styled = pub_styled,
                                                                       signif_digits = signif_digits),
                                         .options = .paropts)

      future::plan('sequential')

    }

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

# Compare or correlate a variable in a plot -----

#' Plot a variable in two or more data sets objects.
#'
#' @description Plots values of two or more EDA objects
#' as a classical bar, violin,
#' boxplot or correlation point plot representation.
#' @param ... data sets.
#' @param variable variable name.
#' @param split_factor optional, the name of a factor used for
#' splitting the variables of interest into analysis groups.
#' @param type type of the plot. 'default' plots violin for
#' numeric variables and bars for factors.
#' 'bar', 'bubble' and 'stack' (stacked bar plot) are available for factors.
#' 'violin', 'box', 'hist', 'correlation' and 'paired'
#' are available for numeric variables.
#' @param data_names a vector with names of the data sets.
#' @param scale the feature to be presented in factor bar plots.
#' 'none' plots counts, 'percent' plots percentages,
#' 'fraction' presents fraction fo complete observations.
#' @param point_alpha alpha of the plot points.
#' @param point_hjitter point jitter height.
#' @param point_wjitter point jitter width.
#' @param point_color color of the points in the correlation plot.
#' @param point_size size of the points in the plots.
#' @param line_color color of the trend line in the correlation plots
#' or the connecting lines in the paired plots.
#' @param line_alpha opacity of the connecting lines in the paired plot.
#' @param cust_theme custom ggplot2 theme.
#' @param plot_title text to be presented in the plot title.
#' @param plot_subtitle text to be presented in the plot subtitle.
#' @param x_lab text to be presented in the X axis title.
#' @param y_lab text to be presented in the Y axis title.
#' @param show_trend logical, should a trend line with 95\% confidence
#' intervals be presented in the correlation plots?
#' @param show_labels logical, should labels with count numbers,
#' percentages or fractions be presented in bar plots?
#' @param geom_label logical, should the text in the stacked bar plot be
#' presented as a ggplot's geom_label?
#' @param signif_digits significant digits used for the label value rounding.
#' @param txt_size size of the text label.
#' @param txt_color color of the text label.
#' @param bins bin number, passed to \code{\link[ggplot2]{histogram}}.
#' @param facet_hist 'none': histograms are overlaid,
#' 'horizontal': horizontal or 'vertical': vertical faceting.
#' @param x_n_labs logical. If TRUE, n numbers per strata are displayed in the
#' X axis of the plot instead of the plot tag. Defaults to FALSE and concerns
#' violin, box, paired and stack plots.
#' @details the particular data sets representations are color coded.
#' In case, the split_factor is provided, only the first
#' of the input data frames will be plotted.
#' @export

  plot_variable <- function(...,
                            variable,
                            split_factor = NULL,
                            data_names = NULL,
                            type = c('default', 'bar', 'violin', 'box', 'hist',
                                     'correlation', 'paired', 'stack'),
                            scale = c('none', 'fraction', 'percent'),
                            point_alpha = 0.5,
                            point_hjitter = 0.05,
                            point_wjitter = 0.1,
                            point_color = 'steelblue',
                            point_size = 2,
                            line_color = 'black',
                            line_alpha = 0.25,
                            cust_theme = ggplot2::theme_classic(),
                            plot_title = NULL,
                            plot_subtitle = NULL,
                            x_lab = NULL,
                            y_lab = NULL,
                            show_trend = TRUE,
                            show_labels = TRUE,
                            geom_label = TRUE,
                            signif_digits = 2,
                            txt_size = 2.75,
                            txt_color = 'black',
                            bins = NULL,
                            facet_hist = c('none', 'horizontal', 'vertical'),
                            x_n_labs = FALSE) {

    ## entry control

    inp_list <- rlang::list2(...)

    classes <- purrr::map_lgl(inp_list,
                              ~any(class(.x) == 'data.frame'))

    if(any(!classes)) stop('Please provide data frames as an analysis input.', call. = FALSE)

    if(length(inp_list) < 2 & is.null(split_factor)) stop('At least two data frames required.', call. = FALSE)

    vars <- purrr::map_lgl(inp_list,
                           ~any(variable %in% names(.x)))

    if(any(!vars)) stop('Variable absent from the data sets.', call. = FALSE)

    if(!is.null(split_factor)) {

      data <- inp_list[[1]]

      if(!split_factor %in% names(data)) stop('The split_factor absent from the data.', call. = FALSE)

      if(length(inp_list) > 2) message('split_factor provided: only the first data set will be analyzed.')

      if(!is.factor(data[[split_factor]])) {

        data <- dplyr::mutate(data, !!split_factor := factor(.data[[split_factor]]))

      }

      inp_list <- plyr::dlply(data, split_factor)

      if(is.null(data_names)) data_names <- levels(data[[split_factor]])

    }

    ## plotting

    inp_list <- purrr::map(inp_list, ~eda(.x[[variable]]))

    multiplot(!!!inp_list,
              eda_names = if(is.null(names(inp_list))) data_names else names(inp_list),
              type = type,
              scale = scale,
              point_alpha = point_alpha,
              point_hjitter = point_hjitter,
              point_wjitter = point_wjitter,
              point_color = point_color,
              point_size = point_size,
              line_color = line_color,
              line_alpha = line_alpha,
              cust_theme = cust_theme,
              plot_title = plot_title,
              plot_subtitle = plot_subtitle,
              x_lab = x_lab,
              y_lab = y_lab,
              show_trend = show_trend,
              show_labels = show_labels,
              signif_digits = signif_digits,
              txt_size = txt_size,
              txt_color = txt_color,
              geom_label = geom_label,
              bins = bins,
              facet_hist = facet_hist,
              x_n_labs = x_n_labs)

  }

# Correlate variables -----

#' Correlate variables in the data set.
#'
#' @description Correlates two variables in the provided data sets by Pearson, Spearman
#' or Kendall method.
#' @param ... data sets.
#' @param variables a vector with variable names. If more than two provided, only the first two are analyzed.
#' @param what the requested analysis: 'correlation' or 'covariance'. Defaults to 'correlation'.
#' @param type the type of correlation/covariance: Pearson, Spearman, Kendall or Kohen's kappa. Defaults to Pearson.
#' See: \code{\link{correlate}} and \code{\link{covariance}} for details.
#' @param ci logical, should confidence intervals for the test effect size be returned?
#' @param boot_method indicates how the bootstrap confidence intervals are calculated.
#' Can be any of 'percentile', 'bca', or 'normality', defaults to 'percentile'.
#' @param pub_styled logical, should the output be publication-ready formatted?
#' @param signif_digits significant digits used for rounding in the publication-style output.
#' @param adj_method the method for adjusting p values for multiple testing, as defined for \code{\link[stats]{p.adjust}},
#' defaults to 'none'. The adjusted p value is summarized in the 'significance' column, when pub_style output is chosen.
#' @param simplify_p logical, should p_values < 0.001 be presented in a p < 0.001 form?
#' @export

  correlate_variables <- function(...,
                                  variables,
                                  what = c('correlation', 'covariance'),
                                  type = c('pearson', 'spearman', 'kendall', 'kappa'),
                                  ci = TRUE,
                                  boot_method = 'percentile',
                                  pub_styled = FALSE,
                                  signif_digits = 2,
                                  adj_method = 'none',
                                  simplify_p = TRUE) {

    ## entry control

    stopifnot(is.logical(ci))

    what <- match.arg(what[1], c('correlation', 'covariance'))
    type <- match.arg(type[1], c('pearson', 'spearman', 'kendall', 'kappa'))

    inp_list <- rlang::list2(...)

    classes <- purrr::map_lgl(inp_list,
                              ~any(class(.x) == 'data.frame'))

    if(any(!classes)) stop('Please provide data frames as an analysis input.', call. = FALSE)

    vars <- purrr::map(inp_list, names)

    vars <- purrr::reduce(vars, intersect)

    if(length(variables) < 2) stop('Please provide at least two variable names.', call. = FALSE)

    if(length(variables) > 2) warning('Only first two variables will be used for analysis.', call. = FALSE)

    variables <- variables[1:2]

    if(any(!variables %in% vars)) stop('Variable absent from the data set.', call. = FALSE)

    ## analysis

    eda_list1 <- purrr::map(inp_list, ~eda(.x[[variables[1]]]))
    eda_list2 <- purrr::map(inp_list, ~eda(.x[[variables[2]]]))

    if(what == 'covariance') {

      test_res <- purrr::pmap(list(eda_object = eda_list1,
                                   y = eda_list2),
                              exda::covariance,
                              type = type)

    } else {

      test_res <- purrr::pmap(list(eda_object = eda_list1,
                                   y = eda_list2),
                              exda::correlation,
                              type = type,
                              ci = ci)

    }

    test_res <- purrr::map_dfr(test_res,
                               summary,
                               pub_styled = pub_styled,
                               signif_digits = signif_digits)

    ## adjustment

    test_res <- dplyr::mutate(test_res,
                              variable1 = variables[1],
                              variable2 = variables[2],
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

    test_res[c('variable1', 'variable2', names(test_res)[!names(test_res) %in% c('variable1', 'variable2')])]


  }

# Plot correlations ------

#' Plot correlation of two variables.
#'
#' @description plots correlation of two variables. Point plot is available for numeric features
#' and bar plot or bubble plot of the contingency matrix for factors.
#' @param data a data frame.
#' @param variables a vector with variable names. If more than two provided, only the first two are analyzed.
#' @param type type of the plot. 'correlation' returns a standard point plot, 'bar' returns a bar plot of
#' frequencies, 'bubble' represents the contingency matrix as a bubble plot.
#' @param scale the feature to be presented in factor bar plots. 'none' plots counts, 'percent' plots percentages,
#' 'fraction' presents fraction fo complete observations.
#' @param point_alpha alpha of the plot points.
#' @param point_hjitter point jitter height.
#' @param point_wjitter point jitter width.
#' @param point_color color of the points in the correlation plot.
#' @param point_size size of the points in the plots.
#' @param line_color color of the trend line in the correlation plots or the connecting lines in the paired plots.
#' @param line_alpha opacity of the connecting lines in the paired plot.
#' @param cust_theme custom ggplot2 theme.
#' @param plot_title text to be presented in the plot title.
#' @param plot_subtitle text to be presented in the plot subtitle.
#' @param x_lab text to be presented in the X axis title.
#' @param y_lab text to be presented in the Y axis title.
#' @param show_trend logical, should a trend line with 95\% confidence intervals be presented in the correlation plots?
#' @param show_labels logical, should labels with count numbers, percentages or fractions be presented in bar plots?
#' @param signif_digits significant digits used for the label value rounding.
#' @param txt_size size of the text label.
#' @export

  plot_correlation <- function(data,
                               variables,
                               type = c('correlation', 'bar', 'bubble'),
                               scale = c('none', 'percent', 'fraction'),
                               point_alpha = 0.5,
                               point_hjitter = 0.05,
                               point_wjitter = 0.1,
                               point_color = 'steelblue',
                               point_size = 2,
                               line_color = 'black',
                               line_alpha = 0.25,
                               cust_theme = ggplot2::theme_classic(),
                               plot_title = NULL,
                               plot_subtitle = NULL,
                               x_lab = NULL,
                               y_lab = NULL,
                               show_trend = TRUE,
                               show_labels = TRUE,
                               signif_digits = 2,
                               txt_size = 2.75) {

    ## entry control

    if(!any(class(data) == 'data.frame')) stop('Please provide a data frame as an analysis input.', call. = FALSE)

    if(length(variables) < 2) stop('Please provide at least two variable names.', call. = FALSE)

    if(length(variables) > 2) warning('Only first two variables will be used for analysis.', call. = FALSE)

    variables <- variables[1:2]

    if(!all(variables %in% names(data))) stop('Variable absent from the data sets.', call. = FALSE)

    type <- match.arg(type[1], c('correlation', 'bar', 'bubble'))

    stopifnot(any(class(cust_theme) == 'theme'))

    ## plotting

    eda1 <- exda::eda(data[[variables[1]]])
    eda2 <- exda::eda(data[[variables[2]]])

    multiplot(eda1, eda2,
              eda_names = variables,
              type = type,
              scale = scale,
              point_alpha = point_alpha,
              point_hjitter = point_hjitter,
              point_wjitter = point_wjitter,
              point_color = point_color,
              point_size = point_size,
              line_color = line_color,
              line_alpha = line_alpha,
              cust_theme = cust_theme,
              plot_title = plot_title,
              plot_subtitle = plot_subtitle,
              x_lab = x_lab,
              y_lab = y_lab,
              show_trend = show_trend,
              show_labels = show_labels,
              signif_digits = signif_digits,
              txt_size = txt_size)

  }
