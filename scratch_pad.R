library(tidyverse)
library(generics)
library(exda)


test1 <- eda(c(rep('a', 10), rep('b', 5), rep('c', 3)))
test2 <- eda(c(rep('a', 5), rep('b', 5), rep('c', 8)))
test5 <- eda(c(rep('a', 1), rep('b', 2), rep('d', 8), NA))

summary(test5, pub_styled = TRUE)

summary(distribution(test1, test2))

summary(correlation(test1, test2, type = 'kappa'), pub_styled = TRUE)

covariance(test1, test2)

#variance(test1, test2)

summary(test(test2, test5, type = 'chisq_test'), pub_styled = TRUE)

test3 <- eda(sort(c(NA, rnorm(100))))
test4 <- eda(sort(c(rt(100, df = 10), NA)))
test6 <- eda(sort(c(rchisq(100, df = 10), NA)))

summary(test3)

summary(eda(c(NA, rnorm(100))), pub_styled = TRUE)


summary(normality(test3), pub_styled = TRUE)
normality(test4)

summary(distribution(test3, test4), pub_styled = TRUE)

summary(correlation(test3, test4, type = 'kendall'), pub_styled = TRUE)

#as_tibble(test3)

variance(test3, test4)

covariance(test3, test4, 'kendall')

summary(test(test3, test4, test6, type = 'friedman_test', ci = FALSE),
        pub_styled = TRUE,
        signif_digits = 2,
        simplify_p = TRUE)

plot(test6, type = 'hist')

explore(mtcars, what = 'table', signif_digits = 4)

explore(data = mtcars, split_factor = 'am', what = 'plots', type = 'violin')

explore(data = mtcars, split_factor = 'am', what = 'table')

compare_variables(mtcars[1:10, ], mtcars[11:20, ], mtcars[21:32, ],
                  variables = c('mpg', 'disp', 'wt', 'qsec'),
                  what = 'test',
                  types = 'anova',
                  pub_styled = TRUE,
                  adj_method = 'holm')

compare_variables(mtcars[1:10, ], mtcars[11:20, ], mtcars[21:32, ],
                  variables = 'mpg',
                  what = 'correlation',
                  types = 'pearson')

new_cars <- mtcars %>%
  as_tibble %>%
  mutate(vs = factor(vs),
         am = factor(am),
         gear = factor(gear),
         grouping = sample(c('a', 'b'), 32, replace = TRUE),
         corr_grouping = rep(c('c', 'd'), 16))


exda:::compare(new_cars, variable = 'gear', split_factor = 'vs', type = 'kruskal_test')


compare_variables(new_cars[1:10, ], new_cars[11:20, ], new_cars[21:30, ],
                  variables = c('mpg', 'vs', 'gear', 'am'),
                  what = 'correlation',
                  types = c('pearson', 'kendall', 'kendall', 'kendall'))

compare_variables(new_cars[1:10, ], new_cars[11:20, ], new_cars[21:30, ],
                  variables = c('mpg', 'vs', 'gear', 'am', 'qsec', 'hp', 'carb'),
                  what = 'test',
                  types = c('mann_whitney_test', 'chisq_test', 'kruskal_test',
                            'chisq_test', 'rm_anova', 'kruskal_test', 'friedman_test'),
                  pub_styled = TRUE,
                  adj_method = 'holm',
                  .parallel = TRUE,
                  .paropts = furrr::furrr_options(seed = TRUE, globals = c('new_cars')))

compare_variables(new_cars,
                  variables = c('mpg', 'vs', 'gear', 'am', 'qsec', 'hp', 'carb'),
                  split_factor = 'gear',
                  types = c('mann_whitney_test', 'chisq_test', 'chisq_test',
                            'chisq_test', 'anova', 'kruskal_test', 'anova'),
                  pub_styled = TRUE,
                  adj_method = 'holm',
                  ci = FALSE)

multiplot(test1, test2, test5,
          type = 'bar',
          eda_names = c('t1', 't2', 't3'),
          scale = 'percent')

multiplot(test3, test4, test6,
          type = 'paired',
          eda_names = c('t1', 't2', 't3'),
          bins = 10,
          facet_hist = 'vertical')

plot_variable(new_cars[1:10, ], new_cars[11:20, ], new_cars[21:30, ],
              variable = 'mpg',
              type = 'correlation')

plot_variable(new_cars[1:10, ], new_cars[11:20, ], new_cars[21:30, ],
              variable = 'mpg',
              type = 'violin')

plot_variable(new_cars, variable = 'mpg', split_factor = 'gear', type = 'violin')

plot_variable(new_cars, variable = 'mpg', split_factor = 'corr_grouping', type = 'correlation')

correlate_variables(new_cars[1:16, ],
                    new_cars[17:32, ],
                    variables = c('mpg', 'hp', 'qsec'),
                    what = 'correlation',
                    type = 'spearman')

correlate_variables(new_cars[1:16, ],
                    new_cars[17:32, ],
                    variables = c('vs', 'am', 'qsec'),
                    what = 'correlation',
                    type = 'kappa')

plot_correlation(new_cars,
                 variables = c('vs', 'am'),
                 type = 'bar',
                 scale = 'percent')

multiplot(test1, test2, test5,
          type = 'bubble',
          eda_names = c('t1', 't2', 't3'),
          scale = 'none')

#multiplot(test1, eda(c(rep('e', 6), rep('a', 6), rep('b', 5), NA)),
   #       type = 'bubble',
    #      eda_names = c('t1', 't5'),
    #      scale = 'percent')

plot_correlation(new_cars,
                 variables = c('vs', 'am'),
                 type = 'bubble',
                 scale = 'none')

eff_size(test1, test2, type = 'freeman_theta', ci = FALSE, boot_method = 'bca')

eff_size(test1, test5, type = 'cramer_v', ci = TRUE)

eff_size(test1, test2, type = 'cramer_v', ci = TRUE)


eff_size(test3, test4, type = 'paired_cohen_d')

eff_size(test3, test6, type = 'paired_wilcoxon_r', ci = TRUE, boot_method = 'normality')

eff_size(test1, test2, test5, type = 'cohen_kappa')

eff_size(test3, test4, test6, type = 'kendall_w')

compare_variables(new_cars,
                  what = 'eff_size',
                  variables = c('mpg', 'vs', 'gear', 'am', 'qsec', 'hp', 'carb'),
                  split_factor = 'gear',
                  types = c('wilcoxon_r', 'cramer_v', 'cramer_v',
                            'cramer_v', 'etasq', 'kruskal_etasq', 'etasq'),
                  pub_styled = TRUE,
                  adj_method = 'holm',
                  ci = FALSE)

test_eff <- compare_variables(new_cars[1:16, ],
                              new_cars[17:32, ],
                              variables = c('vs', 'am', 'qsec'),
                              what = 'eff_size',
                              types = c('cohen_kappa', 'cramer_v', 'paired_cohen_d'))

plot(test_eff, show_labels = 'signif')

multiplot(test1, test2, test5,
          type = 'stack',
          eda_names = c('t1', 't2', 't3'),
          scale = 'percent')

plot_variable(new_cars[1:10, ], new_cars[11:20, ], new_cars[21:30, ],
              variable = 'gear',
              type = 'bar')

plot_variable(new_cars[1:10, ], new_cars[11:20, ], new_cars[21:30, ],
              variable = 'gear',
              type = 'stack')

plot_variable(new_cars,
              variable = 'gear',
              split_factor = 'grouping',
              type = 'stack',
              scale = 'fraction')

draw_quantile_elli(mtcars,
                   quantiles = c(0.025, 0.975),
                   variables = c('disp', 'wt', 'hp'))

draw_quantile_elli(dplyr::mutate(mtcars, vs = factor(vs, c('1', '0'))),
                   quantiles = c(0.025, 0.975),
                   variables = c('disp', 'wt', 'hp'),
                   split_factor = 'vs')


draw_violin_panel(mtcars,
                  variables = c('disp', 'hp'))

draw_violin_panel(dplyr::mutate(mtcars, vs = factor(vs, c('0', '1'))),
                  variables = c('disp', 'hp'),
                  split_factor = 'vs',
                  distr_geom = 'box')

draw_stat_panel(dplyr::mutate(mtcars,
                              gear_fct = factor(gear, c(4, 5, 3)),
                              vs = factor(vs, c('1', '0'))),
                variables = c('disp', 'hp', 'wt', 'mpg', 'qsec', 'carb'),
                stat = 'mean',
                split_factor = 'vs',
                err_stat = '2se',
                form = 'line')

plot_variable(dplyr::mutate(mtcars,
                            gear_fct = factor(gear, c(4, 5, 3)),
                            vs = factor(vs, c('1', '0'))),
              split_factor = 'vs',
              variable = 'gear_fct',
              type = 'stack',
              scale = 'percent',
              txt_color = 'orangered3')

plot_variable(dplyr::mutate(mtcars,
                            gear_fct = factor(gear, c(4, 5, 3)),
                            vs = factor(vs, c('1', '0'))),
              split_factor = 'vs',
              variable = 'mpg',
              type = 'violin',
              scale = 'percent',
              txt_color = 'orangered3')

draw_freq_panel(dplyr::mutate(mtcars,
                              gear_fct = factor(gear, c(4, 5, 3)),
                              vs = factor(vs, c('1', '0')),
                              am = factor(am, c('1', '0'))),
                #split_factor = 'cyl',
                variables = c('vs', 'am'),
                show_labels = TRUE)












