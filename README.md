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

In the following examples of basic functionality of the package a modified 
`mtcars` data set available in base R will be used. 
For transformation of tabular data, we'll need `tidyverse` package bundle. 
`patchwork` package will be used to stitch plot panels.
In the examples, we will explore and compare characteristics of vehicles with 
4-, 6-, and 8-cylinder engines.

```r

  library(tidyverse)
  library(exda)
  
  library(patchwork)
  
  ## analysis data

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

```

```r
> my_cars

# A tibble: 32 × 12
     mpg cyl          disp    hp  drat    wt  qsec vs       am        gear   carb na_dummy
   <dbl> <fct>       <dbl> <dbl> <dbl> <dbl> <dbl> <fct>    <fct>     <fct> <dbl>    <dbl>
 1  21   6 cylinders  160    110  3.9   2.62  16.5 V-shaped manual    4         4       NA
 2  21   6 cylinders  160    110  3.9   2.88  17.0 V-shaped manual    4         4       NA
 3  22.8 4 cylinders  108     93  3.85  2.32  18.6 I-shaped manual    4         1       NA
 4  21.4 6 cylinders  258    110  3.08  3.22  19.4 I-shaped automatic 3         1       NA
 5  18.7 8 cylinders   NA    175  3.15  3.44  17.0 V-shaped automatic 3         2       NA
 6  18.1 NA           225    105  2.76  3.46  20.2 I-shaped automatic 3         1       NA
 7  14.3 8 cylinders  360    245  3.21  3.57  15.8 V-shaped automatic 3         4       NA
 8  24.4 4 cylinders  147.    62  3.69  3.19  20   I-shaped automatic 4         2       NA
 9  22.8 4 cylinders  141.    95  3.92  3.15  22.9 I-shaped automatic 4         2       NA
10  19.2 6 cylinders  168.   123  3.92  3.44  18.3 I-shaped automatic 4         4       NA
# ℹ 22 more rows
# ℹ Use `print(n = ...)` to see more rows

```

### Descriptive statistics

Function `explore.data.frame()` (an `explore()` method for data frames) is 
the most versatile package's tool to compute descriptive statistics of variables 
in analysis groups. 
By default, the descriptive statistics for numeric features are mean, standard 
deviation, median, interquartile range, range, and number of complete cases. 
For factor features, percentages and counts of observations are returned by default. 
In our case we will call it for the `my_cars` data set, and retrieve descriptive 
statistics for the entire data set (`split_factor = NULL`) and cars grouped by 
cylinder number (`split_factor = "cyl"`). 
By setting arguments `what = "table"` and `one_table = TRUE` (they are anyway the defaults), 
we let the function return a pre-formatted data frame, with descriptive statistics 
rounded and embedded in the text: 

```r

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

```
```r
> car_stats_whole_data

# A tibble: 13 × 2
   variable               statistic                                                                                   
 * <chr>                  <chr>                                                                                       
 1 observations, total, N "32"                                                                                        
 2 mpg                    "mean = 20 (SD: 6)\nmedian = 19 [IQR: 15 to 23]\nrange: 10 to 34\ncomplete: n = 32"         
 3 cyl                    "4 cylinders: 35% (11)\n6 cylinders: 19% (6)\n8 cylinders: 45% (14)\ncomplete: n = 31"      
 4 disp                   "mean = 230 (SD: 120)\nmedian = 170 [IQR: 120 to 310]\nrange: 71 to 470\ncomplete: n = 31"  
 5 hp                     "mean = 150 (SD: 69)\nmedian = 120 [IQR: 96 to 180]\nrange: 52 to 340\ncomplete: n = 32"    
 6 drat                   "mean = 3.6 (SD: 0.53)\nmedian = 3.7 [IQR: 3.1 to 3.9]\nrange: 2.8 to 4.9\ncomplete: n = 32"
 7 wt                     "mean = 3.2 (SD: 0.98)\nmedian = 3.3 [IQR: 2.6 to 3.6]\nrange: 1.5 to 5.4\ncomplete: n = 32"
 8 qsec                   "mean = 18 (SD: 1.8)\nmedian = 18 [IQR: 17 to 19]\nrange: 14 to 23\ncomplete: n = 32"       
 9 vs                     "I-shaped: 44% (14)\nV-shaped: 56% (18)\ncomplete: n = 32"                                  
10 am                     "automatic: 59% (19)\nmanual: 41% (13)\ncomplete: n = 32"                                   
11 gear                   "3: 47% (15)\n4: 38% (12)\n5: 16% (5)\ncomplete: n = 32"                                    
12 carb                   "mean = 2.8 (SD: 1.6)\nmedian = 2 [IQR: 2 to 4]\nrange: 1 to 8\ncomplete: n = 32"           
13 na_dummy                NA   

```

```r
> car_stats_cyl_groups

# A tibble: 12 × 4
   variable               `4 cylinders`                                                                  `6 cylinders` `8 cylinders`
   <chr>                  <chr>                                                                          <chr>         <chr>        
 1 observations, total, N "11"                                                                           "6"           "14"         
 2 mpg                    "mean = 27 (SD: 4.5)\nmedian = 26 [IQR: 23 to 30]\nrange: 21 to 34\ncomplete:… "mean = 20 (… "mean = 15 (…
 3 disp                   "mean = 110 (SD: 27)\nmedian = 110 [IQR: 79 to 120]\nrange: 71 to 150\ncomple… "mean = 180 … "mean = 350 …
 4 hp                     "mean = 83 (SD: 21)\nmedian = 91 [IQR: 66 to 96]\nrange: 52 to 110\ncomplete:… "mean = 130 … "mean = 210 …
 5 drat                   "mean = 4.1 (SD: 0.37)\nmedian = 4.1 [IQR: 3.8 to 4.2]\nrange: 3.7 to 4.9\nco… "mean = 3.7 … "mean = 3.2 …
 6 wt                     "mean = 2.3 (SD: 0.57)\nmedian = 2.2 [IQR: 1.9 to 2.6]\nrange: 1.5 to 3.2\nco… "mean = 3.1 … "mean = 4 (S…
 7 qsec                   "mean = 19 (SD: 1.7)\nmedian = 19 [IQR: 19 to 20]\nrange: 17 to 23\ncomplete:… "mean = 18 (… "mean = 17 (…
 8 vs                     "I-shaped: 91% (10)\nV-shaped: 9.1% (1)\ncomplete: n = 11"                     "I-shaped: 5… "V-shaped: 1…
 9 am                     "automatic: 27% (3)\nmanual: 73% (8)\ncomplete: n = 11"                        "automatic: … "automatic: …
10 gear                   "3: 9.1% (1)\n4: 73% (8)\n5: 18% (2)\ncomplete: n = 11"                        "3: 17% (1)\… "3: 86% (12)…
11 carb                   "mean = 1.5 (SD: 0.52)\nmedian = 2 [IQR: 1 to 2]\nrange: 1 to 2\ncomplete: n … "mean = 3.8 … "mean = 3.5 …
12 na_dummy                NA                                                                             NA            NA          

```
### Normality, equality of variances in the analysis groups

To conduct Shapiro-Wilk tests for normality of numeric variables in the cylinder number groups, 
you can call `check_normality()` and specify the `cyl` variable as the splitting 
factor: 

```r

  car_normality_cyl_groups <-
    check_normality(my_cars,
                    variables = num_variables,
                    split_factor = "cyl")

```

```r
> car_normality_cyl_groups

# A tibble: 24 × 25
   variable cyl    test  stat_name  stat     n df1   df2   estimate_name estimate lower_ci upper_ci p_cutoff p_value p_adjust_method
 * <chr>    <fct>  <chr> <chr>     <dbl> <dbl> <lgl> <lgl> <lgl>         <lgl>    <lgl>    <lgl>       <dbl>   <dbl> <chr>          
 1 mpg      4 cyl… Shap… W         0.912    11 NA    NA    NA            NA       NA       NA           0.05 0.261   none           
 2 mpg      6 cyl… Shap… W         0.904     6 NA    NA    NA            NA       NA       NA           0.05 0.397   none           
 3 mpg      8 cyl… Shap… W         0.932    14 NA    NA    NA            NA       NA       NA           0.05 0.323   none           
 4 disp     4 cyl… Shap… W         0.912    11 NA    NA    NA            NA       NA       NA           0.05 0.256   none           
 5 disp     6 cyl… Shap… W         0.683     6 NA    NA    NA            NA       NA       NA           0.05 0.00407 none           
 6 disp     8 cyl… Shap… W         0.893    13 NA    NA    NA            NA       NA       NA           0.05 0.107   none           
 7 hp       4 cyl… Shap… W         0.906    11 NA    NA    NA            NA       NA       NA           0.05 0.219   none           
 8 hp       6 cyl… Shap… W         0.682     6 NA    NA    NA            NA       NA       NA           0.05 0.00397 none           
 9 hp       8 cyl… Shap… W         0.898    14 NA    NA    NA            NA       NA       NA           0.05 0.105   none           
10 drat     4 cyl… Shap… W         0.884    11 NA    NA    NA            NA       NA       NA           0.05 0.117   none   

```
By a brief inspection of the normality testing data, we can identify potentially 
strongly non-normal distribution of `disp` and `hp` variables in the 6-cylinder group. 
We can investigate this phenomenon further with quantile - quantile plots (QQ plots) 
for those variables in the cylinder groups returned with `plot_variable()`: 

```r

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

```

<img width="765" height="753" alt="QQplots" src="https://github.com/user-attachments/assets/e185b1e6-c2e7-4c6e-9dac-4bf423d7bc1d" />

Equality of variance in the cylinder groups can be explored with Levene tests 
implemented by `compare_variances()`: 

```r

  car_eov_cyl_groups <-
      compare_variances(my_cars,
                        variables = num_variables,
                        split_factor = "cyl")

```
```r
> car_eov_cyl_groups

# A tibble: 7 × 24
  variable test        stat_name    stat     n   df1   df2 estimate_name estimate lower_ci upper_ci p_cutoff p_value p_adjust_method
* <chr>    <chr>       <chr>       <dbl> <dbl> <dbl> <dbl> <lgl>         <lgl>    <lgl>    <lgl>       <dbl>   <dbl> <chr>          
1 mpg      Levene test F         6.26       31     2    28 NA            NA       NA       NA           0.05 0.00565 none           
2 disp     Levene test F         4.56       30     2    27 NA            NA       NA       NA           0.05 0.0197  none           
3 hp       Levene test F         3.79       31     2    28 NA            NA       NA       NA           0.05 0.0350  none           
4 drat     Levene test F         0.00363    31     2    28 NA            NA       NA       NA           0.05 0.996   none           
5 wt       Levene test F         1.28       31     2    28 NA            NA       NA       NA           0.05 0.294   none           
6 qsec     Levene test F         0.391      31     2    28 NA            NA       NA       NA           0.05 0.680   none           
7 carb     Levene test F         1.29       31     2    28 NA            NA       NA       NA           0.05 0.290   none           
# ℹ 10 more variables: p_adjusted <dbl>, effect_name <chr>, effect_size <dbl>, raw_significance <chr>, significance <chr>,
#   n_txt <chr>, stat_txt <chr>, estimate_txt <lgl>, effect_size_txt <chr>, plot_caption <chr>

```

As evident from the results above, for variables `mpg` and `hp` their variances differ strongly (F-statistics > 3) 
and significantly between the cylinder groups. 
Differences in distribution of these two variables in the cylinder groups can be 
easily visualized in density plots with `plot_variable`. 
By the way, because all plotting tools of the package return `ggplot` graphics, they 
can be easily customized, e.g. by hiding the legends with `theme()`: 

```r

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

```

<img width="935" height="557" alt="density_plots" src="https://github.com/user-attachments/assets/96837291-8de0-4635-b255-d7bc84b97cd9" />

With the density plots, we can easily identify the inflated variance of 
`mpg` variable in the four-cylinder group, and the increased variance of `hp` in 
eight-cylinder vehicles. 

### Multi-variable plots

Function `plot_multi_variables()` is a versatile tool for visualizing distributions 
of multiple numeric or factor variables in analysis groups as e.g. violin, box, 
or stack plots. 
Withe the code presented below we'll create a panel of violin plots for the 
numeric variables in the `my_cars` data set. 
Prior to plotting, we'll convert values of these variables to Z-scores to make 
sure they are located in similar scales: 

```r

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

```

<img width="775" height="850" alt="violin_plots" src="https://github.com/user-attachments/assets/94deb7be-7e0c-48d2-b373-afabf6476032" />

Percentages of observations in categories of factor variables in 4-, 6-, and 8-cylinder 
cars can be presented in a stack plot panel: 

```r

  ## stack plot panel for factor variables

  factor_panel <-
    plot_multi_variables(my_cars,
                         variables = fct_variables,
                         split_factor = "cyl",
                         scale = "percent")


```

<img width="775" height="547" alt="factor_plots" src="https://github.com/user-attachments/assets/1cc1e260-985d-459f-968f-2e0917fc815e" />

### Pairwise correlations between numeric variables

To compute pairwise correlations between numeric variables in the `my_cars` data set, 
we'll call `correlate_variables()`. 
The function performs permutation or bootstrap correlation tests and returns them 
by default in a standardized data frame (`etest` object). 
With the code below, we'll calculate pairwise Kendall's TauB correlations, i.e. 
correlations with corrections for ties (`type = "kendallB"`). 
The statistical significance will be assessed by bootstrap (`test_method = "bootstrap"`) `
and the p-values will be adjusted for multiple testing with the Benjamini-Hochberg 
method (`adj_method = "BH"`): 

```r

  ## Kendall's TauB

  cor_test <-
    correlate_variables(my_cars,
                        variables = num_variables,
                        type = "kendallB",
                        test_method = "bootstrap",
                        adj_method = "BH")

```
```r
> cor_test

# A tibble: 21 × 26
   variable1 variable2 test             stat_name   stat     n df1   df2   estimate_name estimate lower_ci upper_ci p_cutoff p_value
 * <chr>     <chr>     <chr>            <chr>      <dbl> <dbl> <lgl> <lgl> <chr>            <dbl>    <dbl>    <dbl>    <dbl>   <dbl>
 1 mpg       disp      correlation, bo… τB        -0.802    31 NA    NA    τB              -0.802  -0.901   -0.648      0.05   0.001
 2 mpg       hp        correlation, bo… τB        -0.743    32 NA    NA    τB              -0.743  -0.856   -0.616      0.05   0.001
 3 mpg       drat      correlation, bo… τB         0.465    32 NA    NA    τB               0.465   0.280    0.639      0.05   0.001
 4 mpg       wt        correlation, bo… τB        -0.728    32 NA    NA    τB              -0.728  -0.832   -0.597      0.05   0.001
 5 mpg       qsec      correlation, bo… τB         0.315    32 NA    NA    τB               0.315   0.0630   0.499      0.05   0.005
 6 mpg       carb      correlation, bo… τB        -0.504    32 NA    NA    τB              -0.504  -0.657   -0.304      0.05   0.001
 7 disp      hp        correlation, bo… τB         0.674    31 NA    NA    τB               0.674   0.531    0.804      0.05   0.001
 8 disp      drat      correlation, bo… τB        -0.499    31 NA    NA    τB              -0.499  -0.677   -0.281      0.05   0.001
 9 disp      wt        correlation, bo… τB         0.761    31 NA    NA    τB               0.761   0.598    0.861      0.05   0.001
10 disp      qsec      correlation, bo… τB        -0.310    31 NA    NA    τB              -0.310  -0.521   -0.0841     0.05   0.008
# ℹ 11 more rows
# ℹ 12 more variables: p_adjust_method <chr>, p_adjusted <dbl>, effect_name <chr>, effect_size <dbl>, raw_significance <chr>,
#   significance <chr>, n_txt <chr>, stat_txt <chr>, estimate_txt <chr>, ci_text <chr>, effect_size_txt <chr>, plot_caption <chr>
# ℹ Use `print(n = ...)` to see more rows

```

The tabular representation of pairwise correlations may not be particularly practical in this case, 
because the number of correlations grows pretty fast with the increasing number of 
tested variables. 
To get an impression on significant and particularly strong correlations, we can 
call `plot()` method for the output of the `correlate_variables()` function, which 
creates a kind of Volcano plot with values of the correlation coefficients in the X axis 
and -log10 p values in the Y axis. 
The `n_top = 5` argument specifies that the top five strongest correlations (measured by 
absolute values of the correlation coefficients) will be labelled with variable names 
in the plot: 

```r
    plot(cor_test, n_top = 5)

```
<img width="775" height="547" alt="correlation_effect_sizes" src="https://github.com/user-attachments/assets/e85575f5-5e91-4f09-ba74-1713574a4c60" />

We can take a closer look at the strongest negative and the strongest positive 
correlation with scatter plots generated with function `plot_two_variables()`: 

```r

  ## scatter plots for MPG and DISP, and DISP and WT

  mpg_disp_plot <-
    plot_two_variables(my_cars,
                       variable1 = "disp",
                       variable2 = "mpg",
                       plot_title = "Engine size and efficacy", ### plot title
                       x_lab = "engine displacement", ### X axis title
                       y_lab = "efficacy, MPG") ### Y axis title

  mpg_wt_plot <-
    plot_two_variables(my_cars,
                       variable1 = "disp",
                       variable2 = "wt",
                       plot_title = "Engine size and car weight",
                       x_lab = "engine displacement",
                       y_lab = "weight, 1000 lbs")

  mpg_disp_plot + mpg_wt_plot

```


### Statistical tests for differences of expected values/distribution between analysis groups

With function `compare_variables()`, we can compare expected values of variables in a data 
frame between analysis groups defined by categories of a splitting factor 
(in our case `cyl` variable). 
With the code below we'll compare expected values of all variables except `cyl` in the `my_cars` 
data set (argument `variables = NULL` or skipped) between 4-, 6-, and 8-cylinder cars 
(`split_factor = "cyl"`). 
The function allows the user to choose the type of statistical hypothesis tests for 
every variable. 
When the test type is not specified, the function resorts to the following defaults: 
for factors chi-square tests are used, for numeric features Mann-Whitney (if the splitting 
factor has two categories) or Kruskal-Wallis test (if the splitting factor has more than 
two categories) utilized. 
Of note, the function returns not only test statistics, degrees of freedom, and p values, 
but also common effect size statistics, which is pretty unique in R! 
For chi-square tests, Cramer's V effect size metric is returned. 
For Mann-Whitney and Kruskal-Wallis test, respectively, biserial rank correlation 
coefficient r and eta-square are computed. 
By specifying `adj_method = "BH"`, the testing results will be adjusted for multiple 
testing with the Benjamini-Hochberg method.
The function's output is, like for correlations above, a standardized data frame 
(`etest` object). 
By launching the code below, a warning is expected: for `na_dummy` variable, 
consisting of NA values only, the testing failed:

```r

  cyl_group_tests <-
    compare_variables(my_cars,
                      split_factor = "cyl", 
                      adj_method = "BH")

```

```r
> cyl_group_tests

# A tibble: 10 × 24
   variable test         stat_name  stat     n   df1 df2   estimate_name estimate lower_ci upper_ci p_cutoff p_value p_adjust_method
 * <chr>    <chr>        <chr>     <dbl> <dbl> <dbl> <lgl> <chr>         <lgl>    <lgl>    <lgl>       <dbl>   <dbl> <chr>          
 1 mpg      Kruskal-Wal… h         25.0     31     2 NA    NA            NA       NA       NA           0.05 3.64e-6 BH             
 2 disp     Kruskal-Wal… h         24.8     30     2 NA    NA            NA       NA       NA           0.05 4.07e-6 BH             
 3 hp       Kruskal-Wal… h         24.5     31     2 NA    NA            NA       NA       NA           0.05 4.88e-6 BH             
 4 drat     Kruskal-Wal… h         15.6     31     2 NA    NA            NA       NA       NA           0.05 4.02e-4 BH             
 5 wt       Kruskal-Wal… h         22.3     31     2 NA    NA            NA       NA       NA           0.05 1.42e-5 BH             
 6 qsec     Kruskal-Wal… h         10.6     31     2 NA    NA            NA       NA       NA           0.05 4.95e-3 BH             
 7 vs       χ² test      χ²        19.3     31     2 NA    NA            NA       NA       NA           0.05 6.44e-5 BH             
 8 am       χ² test      χ²         7.71    31     2 NA    NA            NA       NA       NA           0.05 2.11e-2 BH             
 9 gear     χ² test      χ²        17.0     31     4 NA    NA            NA       NA       NA           0.05 1.91e-3 BH             
10 carb     Kruskal-Wal… h         15.0     31     2 NA    NA            NA       NA       NA           0.05 5.58e-4 BH             
# ℹ 10 more variables: p_adjusted <dbl>, effect_name <chr>, effect_size <dbl>, raw_significance <chr>, significance <chr>,
#   n_txt <chr>, stat_txt <chr>, estimate_txt <lgl>, effect_size_txt <chr>, plot_caption <chr>

```

As for the correlation testing, we can visualize the effect sizes and adjusted 
p-values in scatter/Volcano plots with the `plot()` method and let it label 
top three strongest differences with the variable names: 

```r

  cyl_group_effect_plots <-
    plot(cyl_group_tests,
         n_top = 3) %>%
    map(~.x + theme(legend.position = "bottom"))

  cyl_group_effect_plots[[1]] +
    cyl_group_effect_plots[[2]]

```

<img width="929" height="541" alt="comparison_effect_size_plots" src="https://github.com/user-attachments/assets/3bcc4276-b0e0-4ec3-8421-cedea995b063" />

With `plot_variables`, we can visualize the particularly interesting results, 
namely differences in `mpg` and `vs` between the cylinder groups. 
Please note that with additional arguments we can specify colors, scales, and 
labels: 

```r

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

```

<img width="775" height="547" alt="selected_variables" src="https://github.com/user-attachments/assets/cdd991d5-7126-4e9c-b2f3-ba19064202d1" />

### Result summary 

Finally with `result_summary()`, we can merge the descriptive statistics of 
variables in the `my_cars` data set with the results of statistical hypothesis 
testing. 
We'll also set nicely legible column names: 

```r

  cyl_summary <-
    result_summary(car_stats_cyl_groups,
                   cyl_group_tests) %>%
    set_names(c("Variable",
                "4 cylinders", "6 cylinders", "8 cylinders",
                "Test type",
                "p value, raw", "p value, BH-adjusted",
                "Effect size"))

```
```r
> cyl_summary

# A tibble: 12 × 8
   Variable               `4 cylinders`  `6 cylinders` `8 cylinders` `Test type` `p value, raw` `p value, BH-adjusted` `Effect size`
   <chr>                  <chr>          <chr>         <chr>         <chr>       <chr>          <chr>                  <chr>        
 1 observations, total, N "11"           "6"           "14"          NA          NA             NA                     NA           
 2 mpg                    "mean = 27 (S… "mean = 20 (… "mean = 15 (… Kruskal-Wa… p < 0.001      p < 0.001              η² = 0.82    
 3 disp                   "mean = 110 (… "mean = 180 … "mean = 350 … Kruskal-Wa… p < 0.001      p < 0.001              η² = 0.85    
 4 hp                     "mean = 83 (S… "mean = 130 … "mean = 210 … Kruskal-Wa… p < 0.001      p < 0.001              η² = 0.8     
 5 drat                   "mean = 4.1 (… "mean = 3.7 … "mean = 3.2 … Kruskal-Wa… p < 0.001      p < 0.001              η² = 0.49    
 6 wt                     "mean = 2.3 (… "mean = 3.1 … "mean = 4 (S… Kruskal-Wa… p < 0.001      p < 0.001              η² = 0.73    
 7 qsec                   "mean = 19 (S… "mean = 18 (… "mean = 17 (… Kruskal-Wa… p = 0.0049     p = 0.0055             η² = 0.31    
 8 vs                     "I-shaped: 91… "I-shaped: 5… "V-shaped: 1… χ² test     p < 0.001      p < 0.001              V = 0.79     
 9 am                     "automatic: 2… "automatic: … "automatic: … χ² test     p = 0.021      p = 0.021              V = 0.5      
10 gear                   "3: 9.1% (1)\… "3: 17% (1)\… "3: 86% (12)… χ² test     p = 0.0019     p = 0.0024             V = 0.37     
11 carb                   "mean = 1.5 (… "mean = 3.8 … "mean = 3.5 … Kruskal-Wa… p < 0.001      p < 0.001              η² = 0.46    
12 na_dummy                NA             NA            NA           NA          NA             NA                     NA        

```
