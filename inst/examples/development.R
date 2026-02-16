
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

# END --------
