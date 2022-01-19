# Utilities for plotting multiple EDA objects

#' Plot two or more EDA objects.
#'
#' @description Plots values of two or more EDA objects as a classical bar, violin,
#' boxplot or correlation point plot representation.
#' @param ... numeric-type EDA objects, at least two, created by \code{\link{eda}}.
#' @param type type of the plot. 'default' plots violin for numeric EDAs and bars for factors.
#' 'bar' is available for factor-type EDAs. 'violin', 'box', 'hist', 'correlation' and 'paired'
#' are available for numeric-type objects.
#' @param eda_names a vector with names of the EDA objects.
#' @param scale the feature to be presented in factor bar plots. 'none' plots counts, 'percent' plots percentages,
#' 'fraction' presentes fraction fo complete observations.
#' @param point_alpha alpha of the plot points.
#' @param point_hjitter point jitter height.
#' @param point_wjitter point jitter width.
#' @param point_color color of the points in the correlation plot.
#' @param point_size size of the points in the plots.
#' @param line_color color of the trend line in the correlation plots or the connecting lines in the paired plots.
#' @param line_alpha opacity of the connecting lines in the paired plot.
#' @param cust_theme custom ggplot2 theme.
#' @param plot_title text to be presented in the plot title.
#' @param plot_subtitle text to be presented in the plot subtitle.
#' @param x_lab text to be presented in the X axis title.
#' @param y_lab text to be presented in the Y axis title.
#' @param show_trend logical, should a trend line with 95\% confidence intervals be presented in the correlation plots?
#' @param show_labels logical, should labels with count numbers, percentages or fractions be presented in bar plots?
#' @param signif_digits significant digits used for the label value rounding.
#' @param txt_size size of the text label.
#' @param bins bin number, passed to \code{\link[ggplot2]{histogram}}.
#' @param facet_hist 'none': histograms are overlaid, 'horizontal': horizontal or 'vertical': vertical faceting.
#' @details the particular EDA objects are color coded.
#' @export

  multiplot <- function(...,
                        eda_names = NULL,
                        type = c('default', 'bar', 'violin', 'box', 'hist', 'correlation', 'paired'),
                        scale = c('none', 'fraction', 'percent'),
                        point_alpha = 0.5,
                        point_hjitter = 0.05,
                        point_wjitter = 0.1,
                        point_color = 'steelblue',
                        point_size = 2,
                        line_color = 'black',
                        line_alpha = 0.25,
                        cust_theme = ggplot2::theme_classic(),
                        plot_title = NULL,
                        plot_subtitle = NULL,
                        x_lab = NULL,
                        y_lab = NULL,
                        show_trend = TRUE,
                        show_labels = TRUE,
                        signif_digits = 2,
                        txt_size = 2.75,
                        bins = NULL,
                        facet_hist = c('none', 'horizontal', 'vertical')) {

    ## entry control

    stopifnot(is.numeric(point_alpha))
    stopifnot(is.numeric(point_hjitter))
    stopifnot(is.numeric(point_wjitter))
    stopifnot(is.logical(show_trend))
    stopifnot(is.logical(show_labels))
    stopifnot(is.numeric(signif_digits))

    signif_digits <- as.integer(signif_digits)

    stopifnot(any(class(cust_theme) == 'theme'))

    type <- match.arg(type[1],
                      c('default', 'bar', 'violin', 'box', 'hist', 'correlation', 'paired'))

    scale <- match.arg(scale[1],
                       c('none', 'fraction', 'percent'))

    facet_hist <- match.arg(facet_hist[1],
                            c('none', 'horizontal', 'vertical'))

    edas <- rlang::list2(...)

    classes <- purrr::map_chr(edas, class)

    if(any(classes != 'eda')) stop('Please provide valid EDA objects.')

    types <- purrr::map_chr(edas, ~.x$type)

    if(!all(types == types[1])) stop('The provided EDA objects need to be of the same type.', call. = TRUE)

    if(type == 'correlation' & length(edas) > 2) {

      warning('Multiple EDA objects provided. The first two will be presented in the plot', call. = FALSE)

    }

    if(type == 'correlation' & length(edas[[1]]) != length(edas[[2]])) {

      stop('EDA objects with teh same length are required.', call. = FALSE)

    }

    ## plotting table and n numbers

    plotting_tbl <- switch(type,
                           bar = exda:::chi_tester(!!!edas, test_tbl = TRUE, coerce = TRUE),
                           violin = exda:::convert_eda(!!!edas, paired = FALSE),
                           box = exda:::convert_eda(!!!edas, paired = FALSE),
                           paired = exda:::convert_eda(!!!edas, paired = TRUE),
                           hist = exda:::convert_eda(!!!edas, paired = FALSE),
                           correlation = tibble(x = as_numeric(edas[[1]])$value,
                                                y = as_numeric(edas[[2]])$value))

    if(type == 'default' & types[1] == 'factor') {

      plotting_tbl <- exda:::chi_tester(!!!edas, test_tbl = TRUE, coerce = TRUE)

    }

    if(type == 'default' & types[1] == 'numeric') {

      plotting_tbl <- exda:::convert_eda(!!!edas, paired = FALSE)

    }

    if(!is.null(eda_names)) {

      if(length(edas) != length(eda_names)) {

        stop('The name vector has to be of the same length as the EDA object input', call. = FALSE)

      }

      if(type != 'correlation') {

        naming_vector <- rlang::set_names(eda_names,
                                          levels(plotting_tbl[['group']]))

        plotting_tbl <- dplyr::mutate(plotting_tbl,
                                      group = naming_vector[group])

      }

    }

    if(type != 'correlation') {

      if(type == 'bar' | (type == 'default' & types[1] == 'factor')) {

        n_numbers <- dplyr::summarise(dplyr::group_by(plotting_tbl, group),
                                      n = sum(n))

      } else {

        n_numbers <- dplyr::count(plotting_tbl, .data[['group']])

      }

      plot_tag <- purrr::map2_chr(n_numbers$group,
                                  n_numbers$n,
                                  paste, sep = ': n = ')

      plot_tag <- paste(plot_tag, collapse = '\n')

    } else {

      n_numbers <- sum(complete.cases(plotting_tbl))

      plot_tag <- paste('n =', n_numbers)

    }

    ##plotting for factors, bar plot

    if(type == 'bar' | (type == 'default' & types[1] == 'factor')) {

      scale <- switch(scale,
                      none = 'n',
                      fraction = 'fraction',
                      percent = 'percent')

      plotting_tbl <- dplyr::mutate(plotting_tbl,
                                    plot_lab = signif(.data[[scale]], signif_digits))

      if(is.null(y_lab)) {

        y_lab <- switch(scale,
                        none = 'Count',
                        fraction = 'Fraction of complete observations',
                        percent = '% of complete observations')

      }

      if(is.null(x_lab)) x_lab <- 'Category'

      gg_plot <- ggplot2::ggplot(plotting_tbl,
                                 ggplot2::aes(x = .data[['category']],
                                              y = .data[[scale]],
                                              fill = .data[['group']])) +
        ggplot2::geom_bar(stat = 'identity',
                          position = position_dodge(width = 0.9),
                          color = 'black') +
        ggplot2::labs(x = x_lab,
                      y = y_lab)

      if(show_labels) {

        gg_plot <- gg_plot +
          ggplot2::geom_text(ggplot2::aes(label = plot_lab),
                             size = txt_size,
                             hjust = 0.5,
                             vjust = -0.4,
                             position = position_dodge(width = 0.9))

      }

    } else if(type == 'correlation') {


      gg_plot <- ggplot2::ggplot(plotting_tbl,
                                 ggplot2::aes(x = .data[['x']],
                                              y = .data[['y']])) +
        ggplot2::geom_point(position = position_jitter(width = point_wjitter,
                                                       height = point_hjitter),
                            color = 'black',
                            fill = point_color,
                            size = point_size,
                            shape = 21,
                            alpha = point_alpha) +
        ggplot2::labs(x = x_lab,
                      y = y_lab)

      if(show_trend) {

        gg_plot <- gg_plot +
          ggplot2::geom_smooth(method = 'lm')

      }

    } else if(type %in% c('default', 'violin', 'box')) {

      geom <- switch(type,
                     box = geom_boxplot(alpha = 0.25,
                                        outlier.color = NA,
                                        show.legend = FALSE),
                     default = geom_violin(alpha = 0.25,
                                           show.legend = FALSE),
                     violin = geom_violin(alpha = 0.25,
                                          show.legend = FALSE))

      gg_plot <- ggplot2::ggplot(plotting_tbl,
                                 ggplot2::aes(x = .data[['group']],
                                              y = .data[['variable']],
                                              fill = .data[['group']])) +
        geom +
        ggplot2::geom_point(size = point_size,
                            shape = 21,
                            alpha = point_alpha,
                            position = position_jitter(width = point_wjitter,
                                                       height = point_hjitter)) +
        ggplot2::labs(x = x_lab,
                      y = y_lab)


      if(type %in% c('default', 'violin')) {

        median_tbl <- dplyr::summarise(dplyr::group_by(plotting_tbl, group),
                                       median = median(variable, na.rm = TRUE),
                                       perc25 = quantile(variable, 0.25, na.rm = TRUE),
                                       perc75 = quantile(variable, 0.75, na.rm = TRUE))

        gg_plot <- gg_plot +
          ggplot2::geom_errorbar(data = median_tbl,
                                 ggplot2::aes(y = .data[['median']],
                                              ymin = .data[['perc25']],
                                              ymax = .data[['perc75']]),
                                 size = 0.75,
                                 color = 'orangered3',
                                 width = 0) +
          ggplot2::geom_point(data = median_tbl,
                              ggplot2::aes(y = .data[['median']]),
                              size = 3,
                              fill = 'orangered3',
                              shape = 23)


      }

    } else if(type == 'hist') {

      gg_plot <- ggplot2::ggplot(plotting_tbl,
                                 ggplot2::aes(x = .data[['variable']],
                                              fill = .data[['group']])) +
        ggplot2::geom_histogram(bins = bins,
                                alpha = point_alpha,
                                color = 'black',
                                position = position_identity()) +
        ggplot2::labs(x = x_lab,
                      y = if(is.null(y_lab)) 'Count' else y_lab)

      gg_plot <- switch(facet_hist,
                        none = gg_plot,
                        horizontal = gg_plot + ggplot2::facet_grid(.~group),
                        vertical = gg_plot + ggplot2::facet_grid(group~.))

    } else if(type == 'paired') {

      gg_plot <- ggplot2::ggplot(plotting_tbl,
                                 ggplot2::aes(x = .data[['group']],
                                              y = .data[['variable']],
                                              fill = .data[['group']])) +
        ggplot2::geom_line(ggplot2::aes(group = .data[['id']]),
                           size = 0.25,
                           color = line_color,
                           alpha = line_alpha) +
        ggplot2::geom_point(size = point_size,
                            alpha = point_alpha,
                            shape = 21) +
        ggplot2::labs(x = x_lab,
                      y = y_lab)

    }

    gg_plot +
      cust_theme +
      ggplot2::labs(title = plot_title,
                    subtitle = plot_subtitle,
                    tag = plot_tag,
                    fill = NULL)

  }

