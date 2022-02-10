# The script contains functions for comparing of multiple EDA objects and computing difference effect sizes.

# Testing function -----

#' Compare EDA objects.
#'
#' @description compare two or more EDA objects with a range of statistical tests.
#' @param ... EDA objects, at least two, created by \code{\link{eda}}.
#' @param type type of statistic test. For two numeric EDA objects: t test ('t_test', \code{\link[stats]{t.test}}),
#' paired t test ('paired_t_test', \code{\link[stats]{t.test}}), sign test ('sign_test', \code{\link[DescTools]{SignTest}}),
#' Mann-Whitney test ('mann_whitney_test', \code{\link[stats]{t.test}})
#' or paired Wilcoxon test ('wilcoxon_test', \code{\link[stats]{wilcox.test}}).
#' Multiple numeric-type EDA objects can be compared with one-way ANOVA ('anova', \code{\link[stats]{aov}}),
#' repeated measure one-way ANOVA ('rm_anova', \code{\link[stats]{aov}}), Kruskal-Wallis test
#' ('kruskal_test', \code{\link[stats]{kruskal.test}}) or Friedman test for repeated measurements
#' ('friedman_test', \code{\link[stats]{friedman.test}}). For factor-type EDA objects, Chi-squared test
#' may be applied ('chisq_test', \code{\link[stats]{chisq.test}}).
#' @param exact logical, should exact values for Chi-squared. Mann-Whitney and Wilcoxon test be returned?
#' @param ci logical, should confidence intervals for the test effect size be returned?
#' @param boot_method indicates how the bootstrap confidence intervals are calculated.
#' Can be any of 'percentile', 'bca', or 'normality', defaults to 'percentile'.
#' @details EDA object type is coerced to factor or numeric, as appropriate for the requested analysis.
#' The default effect or location sizes returned as estimates with along with the test results include difference in mean for t tests,
#' difference in (pseudo-) median for sign, Mann-Whitney and Wilcoxon tests, eta-squared (% explained variance)
#' for one-way ANOVA and repeated measure one-way ANOVA, eta-square for Kruskal-Wallis test (calculated by
#' \code{\link[rstatix]{kruskal_effsize}}) and Kendall W for Friedman test (\code{\link[rstatix]{friedman_effsize}}).
#' For Chi-squared test, Cramer's V is returned (\code{\link[rcompanion]{cramerV}}).
#' @return an eTest object with the test name, test statistic value and
#' the default effect/location size with 95/% confidence intervals.
#' @export

  test <- function(...,
                   type = c('t_test', 'paired_t_test', 'sign_test', 'mann_whitney_test', 'wilcoxon_test',
                            'anova', 'rm_anova', 'kruskal_test', 'friedman_test', 'chisq_test'),
                   exact = TRUE,
                   ci = TRUE,
                   boot_method = c('percentile', 'bca', 'normality')) {

    ## entry control

    stopifnot(is.logical(exact),
              is.logical(ci))

    type <- match.arg(type[1],
                      c('t_test', 'paired_t_test', 'sign_test',
                        'mann_whitney_test', 'wilcoxon_test',
                        'anova', 'rm_anova', 'kruskal_test',
                        'friedman_test', 'chisq_test'))

    boot_method <- match.arg(boot_method[1],
                             c('percentile', 'bca', 'normality'))

    boot_method <- switch(boot_method,
                          percentile = 'perc',
                          bca = 'bca',
                          normality = 'norm')

    inp_list <- rlang::list2(...)

    classes <- purrr::map_lgl(inp_list, is_eda)

    if(any(!classes)) stop('EDA objects are required.', call. = FALSE)

    if(length(inp_list) < 2) stop('Please provide at least two EDA objects.', call. = FALSE)

    ## factor testing

    if(type == 'chisq_test') {

      test_res <- exda:::chi_tester(..., test_mtx = FALSE, coerce = TRUE)

      eff <- rcompanion::cramerV(exda:::chi_tester(..., test_mtx = TRUE, coerce = TRUE),
                                 ci = ci,
                                 type = boot_method)

      return(dplyr::mutate(test_res,
                           estimate_name = 'V',
                           estimate = if(ci) eff[1, 1] else eff[1],
                           lower_ci = if(ci) eff[1, 2] else NA,
                           upper_ci = if(ci) eff[1, 3] else NA))

    }

    ## numeric testing

    paired <- type %in% c('paired_t_test', 'wilcoxon_test', 'rm_anova', 'friedman_test')

    num_input <- exda:::convert_eda(!!!inp_list, paired = paired)

    if(paired) {

      n <- nrow(dplyr::filter(num_input,
                              complete.cases(num_input),
                              !duplicated(id)))

      n <- n * length(inp_list)

    } else {

      n <- nrow(dplyr::filter(num_input,
                              complete.cases(num_input)))

    }

    ## two-sample

    if(type %in% c('t_test', 'paired_t_test', 'sign_test', 'mann_whitney_test', 'wilcoxon_test')) {

      if(length(levels(num_input$group)) > 2) {

        warning('More than two EDA objects provided. The first two will be compared.', call. = FALSE)

      }

      test_fun <- switch(type,
                         t_test = function(x, y) stats::t.test(x, y, paired = FALSE),
                         paired_t_test = function(x, y) stats::t.test(x, y, paired = TRUE),
                         sign_test = function(x, y) DescTools::SignTest(x, y),
                         mann_whitney_test = function(x, y) stats::wilcox.test(x, y,
                                                                               paired = FALSE,
                                                                               conf.int = TRUE,
                                                                               exact = exact),
                         wilcoxon_test = function(x, y) stats::wilcox.test(x, y,
                                                                           paired = TRUE,
                                                                           conf.int = TRUE,
                                                                           exact = exact))

      test_res <- test_fun(as_numeric(inp_list[[1]]$value),
                           as_numeric(inp_list[[2]]$value))

      test <- switch(type,
                     t_test = 'unpaired two-sided T test',
                     paired_t_test = 'paired two-sided T test',
                     sign_test = 'sign test',
                     mann_whitney_test = 'Mann-Whitney test',
                     wilcoxon_test = 'paired Wilcoxon test')

      stat_name <- switch(type,
                          t_test = 't',
                          paired_t_test = 't',
                          sign_test = 'S',
                          mann_whitney_test = 'W',
                          wilcoxon_test = 'V')

      df1 <- switch(type,
                    t_test = test_res[['parameter']],
                    paired_t_test = test_res[['parameter']],
                    sign_test = NA,
                    mann_whitney_test = NA,
                    wilcoxon_test = NA)

      estimate_name <- switch(type,
                              t_test = 'mean difference',
                              paired_t_test = 'mean difference',
                              sign_test = 'median difference',
                              mann_whitney_test = 'median difference',
                              wilcoxon_test = 'median difference')

      estimate <- switch(type,
                         t_test = test_res[['estimate']][1] - test_res[['estimate']][2],
                         paired_t_test = test_res[['estimate']][1],
                         sign_test = test_res[['estimate']][1],
                         mann_whitney_test = test_res[['estimate']][1],
                         wilcoxon_test = test_res[['estimate']][1])

      return(exda::etest(test = test,
                         stat_name = stat_name,
                         stat = test_res[['statistic']],
                         df1 = df1,
                         estimate_name = estimate_name,
                         estimate = estimate,
                         lower_ci = if(ci) test_res[['conf.int']][1] else NA,
                         upper_ci = if(ci) test_res[['conf.int']][2] else NA,
                         p_value = test_res[['p.value']],
                         n = n))

    }

    if(type %in% c('anova', 'rm_anova')) {

      tst_formula <- switch(type,
                            anova = variable ~ group,
                            rm_anova = variable ~ group + Error(id))

      test <- switch(type,
                     anova = 'one-way ANOVA',
                     rm_anova = 'repeated-measure one-way ANOVA')

      tst_result <- summary(stats::aov(formula = tst_formula,
                                       data = num_input))

      tst_result <- switch(type,
                           anova = tst_result[[1]],
                           rm_anova = tst_result[[2]][[1]])

      tst_result <- tibble::as_tibble(tst_result)

      tst_result <- dplyr::mutate(tst_result,
                                  eta_sq = `Sum Sq`/sum(`Sum Sq`))

      return(exda::etest(test = test,
                         stat_name = 'F',
                         stat = tst_result[['F value']][1],
                         df1 = tst_result[['Df']][1],
                         df2 = tst_result[['Df']][2],
                         estimate_name = 'eta squared',
                         estimate = tst_result[['eta_sq']][1],
                         p_value = tst_result[['Pr(>F)']][1],
                         n = n))

    }

    if(type %in% c('kruskal_test', 'friedman_test')) {

      test <- switch(type,
                     kruskal_test = 'Kruskal-Wallis test',
                     friedman_test = 'Friedman test')

      stat_name <- switch(type,
                          kruskal_test = 'rank sum',
                          friedman_test = 'chi squared')

      test_res <- switch(type,
                         kruskal_test = stats::kruskal.test(formula = variable ~ group,
                                                            data = num_input),
                         friedman_test = stats::friedman.test(formula = variable ~ group | id,
                                                              data = num_input))

      eff_name <- switch(type,
                         kruskal_test = 'eta squared',
                         friedman_test = 'W')

      eff_res <- switch(type,
                        kruskal_test = rstatix::kruskal_effsize(formula = variable ~ group,
                                                                data = num_input,
                                                                ci = ci,
                                                                ci.type = boot_method),
                        friedman_test = rstatix::friedman_effsize(formula = variable ~ group | id,
                                                                  data = num_input,
                                                                  ci = ci,
                                                                  ci.type = boot_method))

      return(exda::etest(test = test,
                         stat_name = stat_name,
                         stat = test_res[['statistic']][1],
                         df1 = test_res[['parameter']][1],
                         estimate_name = eff_name,
                         estimate = eff_res[['effsize']][1],
                         lower_ci = if(ci) eff_res[['conf.low']][1] else NA,
                         upper_ci = if(ci) eff_res[['conf.high']][1] else NA,
                         p_value = test_res[['p.value']][1],
                         n = n))

    }

  }

# Effect size calculation ------

#' Effect size for difference between EDA objects.
#'
#' @description compare two or more EDA objects with a range of statistical tests.
#' @param ... EDA objects, at least two, created by \code{\link{eda}}.
#' @param type type of statistic the effect size. For two numeric EDA objects: independent variable Cohen's D
#' ('cohens_d', \code{\link[rstatix]{cohens_d}}), paired Cohen's D ('paired_cohen_d', \code{\link[rstatix]{cohen_d}}),
#' independent variable Wilcoxon r ('wilcoxon_r', \code{\link[rstatix]{wilcox_effsize}}) or paired Wilcoxon r
#' ('paired_wilcoxon_r', \code{\link[rstatix]{wilcox_effsize}}) are currently implemented. For two factor EDA objects:
#' Cohen's Kappa ('cohen_kappa', \code{\link[vcd]{Kappa}}) and Freeman's theta
#' ('freeman_theta', \code{\link[rcompanion]{freemanTheta}}) is implemented. For 2 or more factor EDAs: Cramer's V
#' ('cramer_v', \code{\link[DescTools]{CramerV}}) can be calculated. For two or more numeric EDAs: ANOVA-derived (partial)
#' eta-squared ('etasq' or 'partial_etasq', \code{\link[rstatix]{eta_squared}}) or
#' Kruskal-Wallis-derived eta-squared ('kruskal_etasq', \code{\link[rstatix]{kruskal_effsize}}) and
#' for aired data Kendall's W ('kendall_w', \code{\link[rstatix]{friedman_effsize}}) are currently available.
#' @param exact logical, should exact values for Chi-squared. Mann-Whitney and Wilcoxon test be returned?
#' @param ci logical, should confidence intervals for the test effect size be returned?
#' @param boot_method indicates how the bootstrap confidence intervals are calculated.
#' Can be any of 'percentile', 'bca', or 'normality', defaults to 'percentile'.
#' @details EDA object type is coerced to factor or numeric, as appropriate for the requested analysis. The default
#' statistic test results are returned as well: T test for Cohen's D, Mann-Whitney/Wilcoxon test for Wilcoxon r,
#' Chi-squared test results for Cohen's kappa, Cramer's V and Freeman's theta, one-way ANOVA for eta-squared and
#' partial eta-squared, Kruskal-Wallis test for Kruskal eta-squared and Friedman test for Kendall#s W. The p value
#' referst to the statistical test result.
#' @return an eTest object with the effect size statistic name as 'estimate' variable, effect size statistic value and
#' with 95/% confidence intervals.
#' @export

  eff_size <- function(...,
                       type = c('cohen_d', 'paired_cohen_d', 'wilcoxon_r', 'paired_wilcoxon_r',
                                'cohen_kappa', 'freeman_theta', 'cramer_v', 'cohen_kappa',
                                'etasq', 'partial_etasq', 'kruskal_etasq', 'kendall_w'),
                       exact = TRUE,
                       ci = TRUE,
                       boot_method = c('percentile', 'bca', 'normality')) {

    ## entry check

    stopifnot(is.logical(exact),
              is.logical(ci))

    type <- match.arg(type[1],
                      c('cohen_d', 'paired_cohen_d', 'wilcoxon_r', 'paired_wilcoxon_r',
                        'cohen_kappa', 'freeman_theta', 'cramer_v', 'cohen_kappa',
                        'etasq', 'partial_etasq', 'kruskal_etasq', 'kendall_w'))

    boot_method <- match.arg(boot_method[1],
                             c('percentile', 'bca', 'normality'))

    boot_method <- switch(boot_method,
                          percentile = 'perc',
                          bca = 'bca',
                          normality = 'norm')

    inp_list <- rlang::list2(...)

    classes <- purrr::map_lgl(inp_list, is_eda)

    if(any(!classes)) stop('EDA objects are required.', call. = FALSE)

    if(length(inp_list) < 2) stop('Please provide at least two EDA objects.', call. = FALSE)

    if(type %in% c('cohen_d', 'paired_cohen_d', 'wilcoxon_r', 'paired_wilcoxon_r', 'cohen_kappa', 'freeman_theta')) {

      if(length(inp_list) > 2) {

        warning('More than two EDA objects provided. The first two will be used for analysis.', call. = FALSE)

      }

      inp_list <- inp_list[1:2]

    }

    ## factor testing

    if(type == 'cramer_v') {

      return(exda::test(!!!inp_list,
                        type = 'chisq_test',
                        exact = exact,
                        ci = ci,
                        boot_method = boot_method))


    }

    if(type == 'cohen_kappa') {

      if(length(inp_list[[1]]) != length(inp_list[[2]])) stop('EDA objects of the same length are required.', call. = FALSE)

      test_res <- exda::test(!!!inp_list,
                             type = 'chisq_test',
                             exact = exact,
                             ci = ci,
                             boot_method = boot_method)

      eff <- exda::correlation(inp_list[[1]],
                               y = inp_list[[2]],
                               type = 'kappa',
                               ci = ci)

      return(dplyr::mutate(test_res,
                           estimate_name = 'kappa',
                           estimate = eff$estimate[1],
                           lower_ci = eff$lower_ci[1],
                           upper_ci = eff$upper_ci[1]))


    }

    if(type == 'freeman_theta') {

      if(length(inp_list[[1]]) != length(inp_list[[2]])) stop('EDA objects of the same length are required.', call. = FALSE)

      test_res <- exda::test(!!!inp_list,
                             type = 'chisq_test',
                             exact = exact,
                             ci = ci,
                             boot_method = boot_method)

      eff <- rcompanion::freemanTheta(exda:::chi_tester(..., test_mtx = TRUE, coerce = TRUE),
                                      ci = ci,
                                      type = boot_method)

      return(dplyr::mutate(test_res,
                           estimate_name = 'theta',
                           estimate = if(ci) eff[1, 1] else eff[1],
                           lower_ci = if(ci) eff[1, 2] else NA,
                           upper_ci = if(ci) eff[1, 3] else NA))

    }

    ## numeric testing

    paired <- type %in% c('paired_wilcoxon_r', 'paired_cohen_d', 'partial_etasq', 'kendall_w')

    num_input <- exda:::convert_eda(!!!inp_list, paired = paired)

    ## two-sample

    if(type %in% c('cohen_d', 'paired_cohen_d', 'wilcoxon_r', 'paired_wilcoxon_r')) {

      ## testing

      tst_type <- switch(type,
                         cohen_d = 't_test',
                         paired_cohen_d = 'paired_t_test',
                         wilcoxon_r = 'mann_whitney_test',
                         paired_wilcoxon_r = 'wilcoxon_test')


      test_res <- exda::test(!!!inp_list,
                             type = tst_type,
                             ci = FALSE,
                             boot_method = boot_method,
                             exact = exact)

      ## effect size

      tst_formula <- variable ~ group

      eff_size <- switch(type,
                         cohen_d = rstatix::cohens_d(data = num_input,
                                                     formula = tst_formula,
                                                     paired = FALSE,
                                                     var.equal = FALSE,
                                                     ci = ci,
                                                     conf.level = 0.95,
                                                     ci.type = boot_method),
                         paired_cohen_d = rstatix::cohens_d(data = num_input,
                                                            formula = tst_formula,
                                                            paired = TRUE,
                                                            var.equal = FALSE,
                                                            ci = ci,
                                                            conf.level = 0.95,
                                                            ci.type = boot_method),
                         wilcoxon_r = rstatix::wilcox_effsize(data = num_input,
                                                              formula = tst_formula,
                                                              paired = FALSE,
                                                              ci = ci,
                                                              onf.level = 0.95,
                                                              ci.type = boot_method),
                         paired_wilcoxon_r = rstatix::wilcox_effsize(data = num_input,
                                                                     formula = tst_formula,
                                                                     paired = TRUE,
                                                                     ci = ci,
                                                                     onf.level = 0.95,
                                                                     ci.type = boot_method))

      return(dplyr::mutate(test_res,
                           estimate_name = if(type %in% c('wilcoxon_r', 'paired_wilcoxon_r')) 'r' else 'd',
                           estimate = eff_size$effsize[1],
                           lower_ci = if(ci) eff_size$conf.low[1] else NA,
                           upper_ci = if(ci) eff_size$conf.low[2] else NA))


    }

    if(type %in% c('etasq', 'partial_etasq', 'kruskal_etasq', 'kendall_w')) {

      tst_type <- switch(type,
                         etasq = 'anova',
                         partial_etasq = 'rm_anova',
                         kruskal_etasq = 'kruskal_test',
                         kendall_w = 'friedman_test')

      return(exda::test(!!!inp_list,
                        type = tst_type,
                        exact = exact,
                        ci = ci,
                        boot_method = boot_method))

    }



  }

