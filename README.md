# ExDA

_R tool set for efficient exploratory data analysis._

## Description

The package offers a range of tools to retrieve: 

* descriptive statistics and plots of distribution of single numeric and factor 
vectors, and variables in a data frame (functions `eda()`, `frequency.eda()`, 
`summary.eda()`, `plot_eda()`, `explore.data.frame()`, `plot_variables()`)

* scatter, heat map, and bubble plots to visualize joint distribution of 
two variables in a data frame (`plot_two_variables()`)

* violin, box, histogram, density, and stack plots of multiple variables in 
a data fame (`plot_multi_variables()`)

* descriptive statistics and plots of distribution of numeric and factor 
variables in analysis groups 
(functions `explore.data.frame()`, `plot_variables()`, `plot_multi_variables()`)

* numeric tests to compare distributions, check normality and variance equality 
(`compare_distributions()`, `check_normality()`, `compare_variances()`)

* statistics of covariance and correlation as well as permutation and 
bootstrap tests for correlation (`cov_variables()`, `cor_variables()`, 
`correlate_variables()`)

* statistical hypothesis test for and effect sizes of differences in 
expected values/distribution between analysis groups (`compare_variables()`)

* publication-ready result summaries (`result_summary()`), `ggplot` themes 
(`eda_classic_theme()` and `eda_void_theme()`), and color palettes 
(`tableau10_colors()`, `tableau20_colors()`)


## Installation

You may easily fetch the package and its dependencies `microViz` 
and `fastTest` with `devtools`: 

```r

## dependencies

devtools::install_github("PiotrTymoszuk/microViz")
devtools::install_github("PiotrTymoszuk/fastTest")

## the package

devtools::install_github("PiotrTymoszuk/exDA/")

```

## Terms of use

The package is available under a
[GPL-3 license](https://github.com/PiotrTymoszuk/exDA/blob/main/LICENSE).

## Contact

The package maintainer is [Piotr Tymoszuk](mailto:piotr.s.tymoszuk@gmail.com).

## Acknowledgements

Package `exda` uses tools provided by the packages 
[rlang](https://rlang.r-lib.org/), 
[tidyverse](https://www.tidyverse.org/), 
[ggplot2](https://ggplot2.tidyverse.org/), 
[ggrepel](https://ggrepel.slowkow.com/), 
[generics](https://cran.r-project.org/web/packages/generics/index.html), and
[stringi](https://cran.r-project.org/web/packages/stringi/index.html). 
Many thanks to their Developers, Maintainers and Contributors.

## Usage

