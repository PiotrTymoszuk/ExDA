
# Tools ----------

  library(tidyverse)
  library(exda)

# Builders --------

  eda(LETTERS)

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

# Numeric methods --------

  quantile(eda(c(1:100, NA)), plain = FALSE)

  mean(eda(c(1:100, NA)), plain = FALSE)

  median(eda(c(1:100, NA)), plain = FALSE)

  kurtosis(eda(c(-100:100, NA)), plain = FALSE)

  skewness(eda(c(-100:100, NA)), plain = FALSE)

  gini(eda(c(-100:100, NA)), plain = FALSE)

  hmean(eda(c(10:100, NA)), plain = FALSE)

  gmean(eda(c(10:100, NA)), plain = FALSE)
