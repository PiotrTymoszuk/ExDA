# R code for GitHub examples

# packages -------

  library(tidyverse)
  library(exda)

  library(patchwork)

# analysis data ------

  ## a derivative of MTCARS data set with some NA values

  my_cars <- mtcars %>%
    mutate(
           ## engine shape
           vs = factor(ifelse(vs == 1, "I-shaped", "V-shaped"),
                       c("I-shaped", "V-shaped", "unknown")),

           ### cylinder number coded as a factor

           cyl = paste(cyl, "cylinders"),
           cyl = factor(cyl),

           ### transmission

           am = factor(ifelse(am == 0,
                              "automatic", "manual"),
                       c("automatic", "manual")),

           ## factor-coded number of gears

           gear = factor(gear),

           ### a NA-only variable

           na_dummy = NA_real_) %>%
  as_tibble

  my_cars[5, "disp"] <- NA
  my_cars[6, "cyl"] <- NA

  ## numeric variables of interest used in correlation analyses

  num_variables <-
    c("mpg", "disp", "hp", "drat", "wt", "qsec", "carb")

  ## categorical variables

  fct_variables <- c("vs", "am", "gear")

# descriptive statistics --------

  car_stats_whole_data <-
    explore(my_cars,
            split_factor = NULL,
            what = "table",
            one_table = TRUE)

  car_stats_cyl_groups <-
    explore(my_cars,
            split_factor = "cyl",
            what = "table",
            one_table = TRUE)

# Normality checks --------

  ## Shapiro-Wilk tests

  car_normality_cyl_groups <-
    check_normality(my_cars,
                    variables = num_variables,
                    split_factor = "cyl")

  ## QQ plots for selected variables

  hp_qq_plot <-
    plot_variable(my_cars,
                  variable = "hp",
                  split_factor = "cyl",
                  type = "qq",
                  facet = "horizontal",
                  plot_title = "HP variable: normality")

  disp_qq_plot <-
    plot_variable(my_cars,
                  variable = "disp",
                  split_factor = "cyl",
                  type = "qq",
                  facet = "horizontal",
                  plot_title = "Displacement variable: normality")

  hp_qq_plot + disp_qq_plot + plot_layout(nrow = 2)

# Equality of variances --------

  ## Levene tests

  car_eov_cyl_groups <-
    compare_variances(my_cars,
                      variables = num_variables,
                      split_factor = "cyl")

  ## density plots

  mpg_density <-
    plot_variable(my_cars,
                  variable = "mpg",
                  split_factor = "cyl",
                  type = "density",
                  plot_title = "MPG variable: distribution",
                  facet = "vertical",
                  facet_space = "fixed") +
    theme(legend.position = "none")

  hp_density <-
    plot_variable(my_cars,
                  variable = "hp",
                  split_factor = "cyl",
                  type = "density",
                  plot_title = "HP variable: distribution",
                  facet = "vertical",
                  facet_space = "fixed") +
    theme(legend.position = "none")

  mpg_density + hp_density + plot_layout(ncol = 2)

# Multi-variable plots --------

  ## normalized numeric variables (Z-scores) for a violin plot panel

  numeric_data <- my_cars[, c("cyl", num_variables)]

  numeric_data[, -1] <- numeric_data[, -1] %>%
    map(~scale(.x)[, 1])

  ## violin plot panel for numeric variables

  numeric_panel <-
    plot_multi_variables(numeric_data,
                         variables = num_variables,
                         split_factor = "cyl",
                         scale = "width")

  ## stack plot panel for factor variables

  factor_panel <-
    plot_multi_variables(my_cars,
                         variables = fct_variables,
                         split_factor = "cyl",
                         scale = "percent")

# Correlations -------

  ## Kendall's TauB

  cor_test <-
    correlate_variables(my_cars,
                        variables = num_variables,
                        type = "kendallB",
                        test_method = "bootstrap",
                        adj_method = "BH")

  plot(cor_test,
       n_top = 5)

# Comparison of expected values ---------

  cyl_group_tests <-
    compare_variables(my_cars,
                      split_factor = "cyl",
                      adj_method = "BH")

  ## plots of effect sizes and p values

  cyl_group_effect_plots <-
    plot(cyl_group_tests,
         n_top = 3) %>%
    map(~.x + theme(legend.position = "bottom"))

  cyl_group_effect_plots[[1]] +
    cyl_group_effect_plots[[2]]

  ## plots for single variables

  mpg_cyl_plot <-
    plot_variable(my_cars,
                  variable = "mpg",
                  split_factor = "cyl",
                  type = "box", ### plot type
                  plot_title = "MPG and cylinder number", ### plot title
                  x_lab = "cylinder number", ### X axis title
                  y_lab = "efficacy, MPG") ### Y axis title

  mpg_cyl_plot <- mpg_cyl_plot +
    theme(legend.position = "none")

  vs_cyl_plot <-
    plot_variable(my_cars,
                  variable = "vs",
                  split_factor = "cyl",
                  type = "stack",
                  scale = "percent", ### percentages instead of counts shown
                  plot_title = "Engine shape and cylinder number",
                  x_lab = "cylinder number",
                  y_lab = "% of cars in cylinder groups",
                  palette = wes_grandbudapest1_colors(), ### fill scale colors
                  fill_lab = "engine shape") ### title of the fill scale

  mpg_cyl_plot + vs_cyl_plot

# Result summary table ----------

  cyl_summary <-
    result_summary(car_stats_cyl_groups,
                   cyl_group_tests) %>%
    set_names(c("Variable",
                "4 cylinders", "6 cylinders", "8 cylinders",
                "Test type",
                "p value, raw", "p value, BH-adjusted",
                "Effect size"))

# END --------
