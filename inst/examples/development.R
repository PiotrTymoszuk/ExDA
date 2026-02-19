
# Tools ----------

  library(tidyverse)
  library(exda)

# Builders --------

  eda(c(sample(LETTERS, 10, replace = TRUE), NA))

  eda(1:100)

  eda(c(TRUE, FALSE))

  eda(mtcars, variable = "mpg")

  eda(mtcars, variable = "gear")

  eda(NA)

  na.omit(eda(c(1:100, NA)))
  na.exclude(eda(c(1:100, NA)))

  nobs(eda(c(1:100, NA)))

# Factor methods ---------

  as.table(eda(c("A", "B", NA)))

  frequency(eda(c("A", "B", NA)))

  factor(LETTERS[1:5], c("A", "B", "Z")) %>%
    eda %>%
    droplevels %>%
    is_eda

# Numeric methods --------

  quantile(eda(c(1:100, NA)), plain = FALSE)

  mean(eda(c(1:100, NA)), plain = FALSE)

  median(eda(c(1:100, NA)), plain = FALSE)

  kurtosis(eda(c(-100:100, NA)), plain = FALSE)

  skewness(eda(c(-100:100, NA)), plain = FALSE)

  gini(eda(c(1:100, NA)), plain = FALSE)

  hmean(eda(c(10:100, NA)), plain = FALSE)

  gmean(eda(c(10:100, NA)), plain = FALSE)

  scale(eda(1:100))

  cut(eda(1:100), default_labels = FALSE)

  cut(eda(1:100), type = "mean")

  cut(eda(1:100), type = "median")

  cut(eda(1:100), type = "tertile", default_labels = FALSE)

# summary methods -------

  eda(c(sample(LETTERS, 10, replace = TRUE), NA)) %>%
    summary(pub_styled = FALSE)

  eda(c(sample(LETTERS, 10, replace = TRUE), NA)) %>%
    summary(pub_styled = TRUE)

  eda(c(1:100, NA)) %>%
    summary(pub_styled = FALSE)

  eda(c(1:100, NA)) %>%
    summary(pub_styled = TRUE)

  mtcars %>%
    map(eda) %>%
    map_dfr(summary, pub_styled = TRUE)

# conversion methods --------

  eda(c(1:10, NA)) %>%
    as.data.frame

  eda(c(sample(LETTERS, 10, replace = TRUE), NA)) %>%
    as.numeric %>%
    is_eda

  eda(c(sample(LETTERS, 10, replace = TRUE), NA)) %>%
    as.integer %>%
    is_eda

  eda(c(1:10, NA)) %>%
    factor %>%
    is_eda

  eda(factor(c(LETTERS[1:5], NA))) %>%
    factor(levels = c("A", "B")) %>%
    is_eda

# splitting --------

  tst_x <- eda(1:20)
  tst_y <- eda(LETTERS[1:20])

  tst_f <- c(sample(c("A", "B"), size = 19, replace = TRUE), NA) %>%
    eda

  split(tst_x, tst_f)

  split(factor(LETTERS[1:20]), tst_f) %>%
    map(eda) %>%
    map(class)

  split(tst_y, tst_f) %>%
    map(droplevels) %>%
    map(summary, pub_styled = TRUE)

# descriptive statistics ----------

  set.seed(213245)

  ## testing data

  my_cars <- mtcars %>%
    mutate(vs = factor(ifelse(vs == 1, "yes", "no"),
                       c("yes", "no", "unknown")),
           car_group = sample(c("A", "B", "C"),
                              size = nrow(.),
                              replace = TRUE),
           car_group = factor(car_group),
           na_dummy = NA,
           car_pair = c(rep("group1", 16),
                        rep("group2", 16)),
           car_pair = factor(car_pair))

  my_cars[5, "disp"] <- NA
  my_cars[6, "car_group"] <- NA

  ## the entire data frame, no splitting factor

  my_cars %>%
    explore(what = "list", .drop = FALSE)

  my_cars %>%
    explore(what = "kurtosis")

  my_cars %>%
    explore(what = "skewness")

  my_cars %>%
    explore(what = "table",
            rm_range = FALSE,
            rm_complete = TRUE,
            style = "median/IQR")

  ## splitting factor present

  my_cars %>%
    explore(split_factor = "car_group",
            what = "list")

  my_cars %>%
    explore(split_factor = "car_group",
            what = "kurtosis",
            one_table = TRUE)

  my_cars %>%
    explore(split_factor = "car_group",
            what = "skewness",
            one_table = TRUE)

  my_cars %>%
    explore(split_factor = "car_group",
            what = "table",
            one_table = TRUE,
            rm_range = TRUE,
            rm_complete = FALSE,
            style = "median/IQR",
            .drop = TRUE)

# plotting methods for single variables in the whole data frame --------

  ## plots for factors

  my_cars %>%
    plot_variable(variable = "vs",
                  .drop = FALSE)

  my_cars %>%
    plot_variable(variable = "vs",
                  type = "stack",
                  scale = "percent",
                  .drop = FALSE)

  my_cars %>%
    plot_variable(variable = "vs",
                  type = "bar",
                  .drop = FALSE)

  my_cars %>%
    plot_variable(variable = "vs",
                  type = "bubble",
                  .drop = FALSE)

  ## plots for numeric objects

  my_cars %>%
    plot_variable(variable = "mpg")

  my_cars %>%
    plot_variable(variable = "mpg",
                  type = "box")

  my_cars %>%
    plot_variable(variable = "mpg",
                  type = "histogram",
                  bins = 10)

  my_cars %>%
    plot_variable(variable = "mpg",
                  type = "density")

  my_cars %>%
    plot_variable(variable = "disp",
                  type = "qq")

  plot_lst <-
    list(variable = names(my_cars)) %>%
    pmap(plot_variable,
         data = my_cars,
         cust_theme = NULL) %>%
    set_names(names(my_cars))

# plots for data frame variables -------

  ## variable and a split factor

  plot_variable(data = my_cars,
                split_factor = "car_group",
                variable = "vs",
                type = "stack",
                scale = "percent",
                labeller = function(x) paste0("#", x),
                x_n_labs = FALSE,
                .drop = FALSE)

  plot_variable(data = my_cars,
                split_factor = "car_group",
                variable = "mpg",
                type = "box",
                labeller = function(x) paste0("#", x),
                x_n_labs = FALSE,
                fill_lab = "car group",
                x_lab = "car group")

  plot_variable(data = my_cars,
                split_factor = "car_pair",
                variable = "mpg",
                type = "paired",
                fill_lab = "car pair")

  plot_variable(data = my_cars,
                split_factor = "car_group",
                variable = "mpg",
                type = "histogram",
                labeller = function(x) paste0("#", x),
                x_n_labs = TRUE,
                facet = "horizontal",
                facet_space = "fixed",
                fill_lab = "car group",
                bins = 5)

  plot_variable(data = my_cars,
                split_factor = "car_group",
                variable = "mpg",
                type = "density",
                labeller = function(x) paste0("#", x),
                x_n_labs = TRUE,
                facet = "horizontal",
                facet_space = "fixed",
                fill_lab = "car group")

  plot_variable(data = my_cars,
                split_factor = "car_group",
                variable = "mpg",
                type = "qq",
                labeller = function(x) paste0("#", x),
                x_n_labs = TRUE,
                facet = "none",
                facet_space = "fixed")

  plot_variable(data = my_cars,
                split_factor = "car_group",
                variable = "mpg",
                plot_title = "MPG in car groups",
                plot_subtitle = "test sub-title")

  plot_variable(data = my_cars,
                split_factor = "car_group",
                variable = "vs",
                scale = "percent")

  ## two variables

  plot_two_variables(data = my_cars,
                     variable1 = "disp",
                     variable2 = "mpg",
                     line_alpha = 0.5,
                     point_alpha = 1,
                     x_lab = "displacemant",
                     y_lab = "miles per galon",
                     plot_title = "Engine size and economics",
                     method = "gam",
                     formula = y ~ s(x, bs = "cs", k = 3))

  plot_two_variables(data = my_cars,
                     variable1 = "car_group",
                     variable2 = "car_pair",
                     txt_style = "full",
                     type = "heat_map",
                     scale = "percent")

  plot_two_variables(data = my_cars,
                     variable1 = "car_group",
                     variable2 = "car_pair",
                     txt_style = "full",
                     type = "bubble",
                     scale = "percent")



# END --------
