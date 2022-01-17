# Plotting utilities for factors -----

#' Bar plot.
#'
#'@description creates a bar plot for a factor-type EDA object.
#'@param eda_object an EDA object.
#'@return a ggplot object.

  plot_bar <- function(eda_object,
                       signif_digits = 2,
                       fill_color = 'steelblue',
                       cust_theme = ggplot2::theme_classic(),
                       txt_size = 2.75) {

    plotting_tbl <- count(na.omit(eda_object))

    plot_tag <- paste('Complete: n =', nobs(eda_object)[['n']][2])

    ggplot2::ggplot(plotting_tbl,
                    ggplot2::aes(x = .data[['category']],
                                 y = .data[['percent']])) +
      ggplot2::geom_bar(stat = 'identity',
                        fill = fill_color,
                        color = 'black') +
      ggplot2::geom_text(ggplot2::aes(label = paste0(signif(.data[['percent']], signif_digits), '%')),
                         hjust = 0.5,
                         vjust = -0.4,
                         size = txt_size) +
      cust_theme  +
      ggplot2::labs(tag = plot_tag,
                    x = 'Category',
                    y = '% complete observations')

  }

# Plotting utilities for numerics ------

#' Violin plot.
#'
#'@description creates a violin plot for a numeric-type EDA object.
#'@param eda_object an EDA object.
#'@return a ggplot object.

  plot_violin <- function(eda_object,
                          signif_digits = 2,
                          point_color = 'steelblue',
                          point_alpha = 0.5,
                          point_hjitter = 0.1,
                          point_wjitter = 0.1,
                          fill_color = 'steelblue',
                          cust_theme = ggplot2::theme_classic(), ...) {

    plotting_tbl <- as_tibble(na.omit(eda_object))

    median_tbl <- summary(eda_object)$statistic

    plot_tag <- summary(eda_object, pub_styled = TRUE, signif_digits = signif_digits)

    ggplot2::ggplot(plotting_tbl,
                    ggplot2::aes(x = '',
                                 y = .data[['variable']])) +
      ggplot2::geom_violin(fill = fill_color,
                           alpha = 0.15, ...) +
      ggplot2::geom_point(size = 2,
                          shape = 21,
                          alpha = point_alpha,
                          fill = point_color,
                          position = ggplot2::position_jitter(width = point_wjitter,
                                                              height = point_hjitter)) +
      ggplot2::geom_errorbar(ggplot2::aes(x = '',
                                          y = median_tbl$value[3],
                                          ymin = median_tbl$value[4],
                                          ymax = median_tbl$value[5]),
                             size = 0.75,
                             color = 'orangered3',
                             width = 0) +
      ggplot2::geom_point(ggplot2::aes(x = '',
                                       y = median_tbl$value[3]),
                          shape = 23,
                          size = 3,
                          fill = 'orangered3') +
      cust_theme +
      ggplot2::labs(x = 'Density',
                    y = 'Value',
                    tag = plot_tag)


  }

#' Histogram.
#'
#'@description creates a histogram for a numeric-type EDA object.
#'@param eda_object an EDA object.
#'@return a ggplot object.

  plot_histogram <- function(eda_object,
                             signif_digits = 2,
                             fill_color = 'steelblue',
                             cust_theme = ggplot2::theme_classic(), ...) {

    plotting_tbl <- as_tibble(na.omit(eda_object))

    median_tbl <- summary(eda_object)$statistic

    plot_tag <- summary(eda_object, pub_styled = TRUE, signif_digits = signif_digits)

    ggplot2::ggplot(plotting_tbl,
                    ggplot2::aes(x = variable)) +
      ggplot2::geom_histogram(fill = fill_color,
                              color = 'black', ...) +
      ggplot2::geom_vline(xintercept = median_tbl$value[3],
                          linetype = 'solid',
                          color = 'orangered3',
                          size = 0.75) +
      ggplot2::geom_vline(xintercept = median_tbl$value[4],
                          linetype = 'dashed',
                          color = 'orangered3',
                          size = 0.75) +
      ggplot2::geom_vline(xintercept = median_tbl$value[5],
                          linetype = 'dashed',
                          color = 'orangered3',
                          size = 0.75) +
      cust_theme +
      ggplot2::labs(x = 'Value',
                    y = 'Frequency of complete observations',
                    tag = plot_tag)

  }

#' Box plot.
#'
#'@description creates a box plot for a numeric-type EDA object.
#'@param eda_object an EDA object.
#'@return a ggplot object.

  plot_box <- function(eda_object,
                       signif_digits = 2,
                       point_color = 'steelblue',
                       point_alpha = 0.5,
                       point_hjitter = 0.1,
                       point_wjitter = 0.1,
                       fill_color = 'steelblue',
                       cust_theme = ggplot2::theme_classic(), ...) {

    plotting_tbl <- as_tibble(na.omit(eda_object))

    median_tbl <- summary(eda_object)$statistic

    plot_tag <- summary(eda_object, pub_styled = TRUE, signif_digits = signif_digits)

    ggplot2::ggplot(plotting_tbl,
                    ggplot2::aes(x = '',
                                 y = .data[['variable']])) +
      ggplot2::geom_boxplot(fill = fill_color,
                            alpha = 0.15, ...) +
      ggplot2::geom_point(size = 2,
                          shape = 21,
                          alpha = point_alpha,
                          fill = point_color,
                          position = ggplot2::position_jitter(width = point_wjitter,
                                                              height = point_hjitter)) +
      cust_theme +
      ggplot2::labs(x = '',
                    y = 'Value',
                    tag = plot_tag)


  }

#' QQ plot.
#'
#'@description creates a qq plot for a numeric-type EDA object.
#'@param eda_object an EDA object.
#'@return a ggplot object.

  plot_qq <- function(eda_object,
                      signif_digits = 2,
                      point_color = 'steelblue',
                      point_alpha = 0.5,
                      point_hjitter = 0.1,
                      point_wjitter = 0.1,
                      line_color = 'orangered',
                      cust_theme = ggplot2::theme_classic(), ...) {

    plotting_tbl <- as_tibble(na.omit(eda_object))

    median_tbl <- summary(eda_object)$statistic

    plot_tag <- summary(normality(eda_object),
                        pub_styled = TRUE,
                        signif_digits = signif_digits)

    plot_tag <- paste(plot_tag$test_stat,
                      plot_tag$significance,
                      paste('Complete: n =', nobs(eda_object)[['n']][2]),
                      sep = '\n')

    ggplot2::ggplot(plotting_tbl,
                    ggplot2::aes(sample = .data[['variable']])) +
      ggplot2::geom_qq(shape = 21,
                       size = 2,
                       alpha = point_alpha,
                       fill = point_color,
                       position = ggplot2::position_jitter(width = point_wjitter,
                                                           height = point_hjitter)) +
      ggplot2::geom_qq_line(color = line_color) +
      cust_theme +
      ggplot2::labs(x = 'Theoretical normal',
                    y = 'Observed',
                    tag = plot_tag)


  }
