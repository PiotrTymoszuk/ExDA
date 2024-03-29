# EDA objects ------

#' Plot method for EDA objects
#'
#' @description Plots distribution of the given EDA object.
#' @param x eda object, created by \code{\link{eda}}.
#' @param type plot type. 'default' is bar for factors and violin for numerics. 'bar' is available for factor-type EDAs.
#' 'violin', 'box', 'hist' and 'qq' are available for numeric-type objects.
#' @param point_color color of the plot points.
#' @param point_alpha alpha of the plot points.
#' @param point_hjitter point jitter height.
#' @param point_wjitter point jitter width.
#' @param fill_color color of the bars and violins.
#' @param line_color color of the qqline in the qq plot.
#' @param txt_size size of the plot label text.
#' @param signif_digits significant digits used for rounding of the statistic summary.
#' @param cust_theme custom ggplot2 theme.
#' @param plot_title text to be presented in the plot title.
#' @param plot_subtitle text to be presented in the plot subtitle.
#' @param ... extra arguments passed to \code{\link[ggplot]{geom_violin}}, \code{\link[ggplot]{geom_histogram}} or
#' \code{\link[ggplot]{geom_boxplot}}.
#' @details the summary of EDA is presented in the plot tag. The type conversion is done prior to plotting.
#' @return a ggplot.
#' @export plot.eda
#' @export

  plot.eda <- function(x,
                       type = c('default', 'bar', 'violin', 'box', 'hist', 'qq'),
                       point_color = 'steelblue',
                       point_alpha = 0.5,
                       point_hjitter = 0.05,
                       point_wjitter = 0.1,
                       fill_color = 'steelblue',
                       line_color = 'orangered3',
                       txt_size = 2.75,
                       signif_digits = 2,
                       cust_theme = ggplot2::theme_classic(),
                       plot_title = NULL,
                       plot_subtitle = NULL, ...) {

    ## entry control

    stopifnot(is_eda(x))
    stopifnot(is.numeric(point_alpha))
    stopifnot(is.numeric(point_hjitter))
    stopifnot(is.numeric(point_wjitter))
    stopifnot(is.numeric(signif_digits))
    stopifnot(is.numeric(txt_size))

    signif_digits <- as.integer(signif_digits)

    stopifnot(any(class(cust_theme) == 'theme'))

    type <- match.arg(type[1],
                      c('default', 'bar', 'violin', 'box', 'hist', 'qq'))

    ## plotting

    if(type == 'default') {

      gg_plot <- switch(x$type,
                        numeric = exda:::plot_violin(eda_object = x,
                                                     signif_digits = signif_digits,
                                                     point_color = point_color,
                                                     point_alpha = point_alpha,
                                                     point_hjitter = point_hjitter,
                                                     point_wjitter = point_wjitter,
                                                     fill_color = fill_color,
                                                     cust_theme = cust_theme, ...),
                        factor = exda:::plot_bar(eda_object = x,
                                                 signif_digits = signif_digits,
                                                 fill_color = fill_color,
                                                 cust_theme = cust_theme,
                                                 txt_size = txt_size))

    } else {

      gg_plot <- switch(type,
                        bar = exda:::plot_bar(eda_object = as_factor(x),
                                              signif_digits = signif_digits,
                                              fill_color = fill_color,
                                              cust_theme = cust_theme,
                                              txt_size = txt_size),
                        violin = exda:::plot_violin(eda_object = as_numeric(x),
                                                    signif_digits = signif_digits,
                                                    point_color = point_color,
                                                    point_alpha = point_alpha,
                                                    point_hjitter = point_hjitter,
                                                    point_wjitter = point_wjitter,
                                                    fill_color = fill_color,
                                                    cust_theme = cust_theme, ...),
                        box = exda:::plot_box(eda_object = as_numeric(x),
                                              signif_digits = signif_digits,
                                              point_color = point_color,
                                              point_alpha = point_alpha,
                                              point_hjitter = point_hjitter,
                                              point_wjitter = point_wjitter,
                                              fill_color = fill_color,
                                              cust_theme = cust_theme, ...),
                        hist = exda:::plot_histogram(eda_object = as_numeric(x),
                                                     signif_digits = signif_digits,
                                                     fill_color = fill_color,
                                                     cust_theme = cust_theme, ...),
                        qq = exda:::plot_qq(eda_object = as_numeric(x),
                                            signif_digits = signif_digits,
                                            point_color = point_color,
                                            point_alpha = point_alpha,
                                            point_hjitter = point_hjitter,
                                            point_wjitter = point_wjitter,
                                            line_color = line_color,
                                            cust_theme = cust_theme, ...))

    }

    gg_plot +
      ggplot2::labs(title = plot_title,
                    subtitle = plot_subtitle)

  }

# eTest objects ------

#' Plot effect size for the eTest object.
#'
#' @description Plots effect size statistic and the adjusted significance for
#' the given eTest object storing results of a statistic test. Plot tag contains
#' (range) n numbers of complete observations.
#' @param x eTest object, created e.g. by \code{\link{compare_variables}}.
#' @inheritParams plot_effect
#' @export plot.etest
#' @export

  plot.etest <- function(x,
                         point_alpha = 0.5,
                         point_hjitter = 0,
                         point_wjitter = 0,
                         point_size = 2,
                         point_color = c("gray60", "coral2"),
                         plot_title = NULL,
                         plot_subtitle = NULL,
                         cust_theme = ggplot2::theme_classic(),
                         show_labels = c("none", "all", "signif"),
                         txt_size = 2.75, ...) {

    exda::plot_effect(etest_object = x,
                      point_alpha = point_alpha,
                      point_hjitter = point_hjitter,
                      point_wjitter = point_wjitter,
                      point_size = point_size,
                      point_color = point_color, plot_title = plot_title,
                      plot_subtitle = plot_subtitle,
                      cust_theme = cust_theme,
                      show_labels = show_labels,
                      txt_size = txt_size, ...)

  }
