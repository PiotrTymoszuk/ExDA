# Utilities for plotting multiple EDA objects

#' Plot two or more EDA objects.
#'
#' @description Plots values of two or more EDA objects as a classical bar, violin,
#' boxplot or correlation point plot representation.
#' @param ... numeric-type EDA objects, at least two, created by \code{\link{eda}}.
#' @param type type of the plot. 'default' plots violin for numeric EDAs and bars for factors.
#' 'bar', 'bubble' or 'stack' (stack-bar plot) are available for factor-type EDAs.
#' 'violin', 'box', 'hist', 'correlation' and 'paired'
#' are available for numeric-type objects.
#' @param eda_names a vector with names of the EDA objects.
#' @param scale the feature to be presented in factor bar or bubble plots.
#' 'none' plots counts, 'percent' plots percentages, 'fraction' presents fraction fo complete observations.
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
#' @param txt_color color of the text label.
#' @param geom_label logical, should the text in the stacked bar plot be
#' presented as a ggplot's geom_label?
#' @param bins bin number, passed to \code{\link[ggplot2]{histogram}}.
#' @param facet_hist 'none': histograms are overlaid, 'horizontal': horizontal or 'vertical': vertical faceting.
#' @details the particular EDA objects are color coded.
#' @export

  multiplot <- function(...,
                        eda_names = NULL,
                        type = c('default', 'bar', 'violin', 'box', 'hist', 'correlation', 'paired', 'bubble', 'stack'),
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
                        txt_color = 'black',
                        geom_label = TRUE,
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
                      c('default', 'bar', 'violin', 'box', 'hist', 'correlation', 'paired', 'bubble', 'stack'))

    scale <- match.arg(scale[1],
                       c('none', 'fraction', 'percent'))

    facet_hist <- match.arg(facet_hist[1],
                            c('none', 'horizontal', 'vertical'))

    edas <- rlang::list2(...)

    edas <- purrr::map(edas, exda:::na.exclude.eda)

    classes <- purrr::map_lgl(edas, is_eda)

    if(any(!classes)) stop('Please provide valid EDA objects.', call. = TRUE)

    types <- purrr::map_chr(edas, ~.x$type)

    if(!all(types == types[1])) stop('The provided EDA objects need to be of the same type.', call. = TRUE)

    if(type %in% c('correlation', 'bubble') & length(edas) > 2) {

      warning('Multiple EDA objects provided. The first two will be presented in the plot', call. = FALSE)

    }

    if(type == 'correlation' & length(edas[[1]]) != length(edas[[2]])) {

      stop('EDA objects with the same length are required.', call. = FALSE)

    }

    ## plotting table and n numbers

    plotting_tbl <- switch(type,
                           bar = exda:::chi_tester(!!!edas,
                                                   test_tbl = TRUE,
                                                   coerce = TRUE),
                           stack = exda:::chi_tester(!!!edas,
                                                     test_tbl = TRUE,
                                                     coerce = TRUE),
                           violin = exda:::convert_eda(!!!edas, paired = FALSE),
                           box = exda:::convert_eda(!!!edas, paired = FALSE),
                           paired = exda:::convert_eda(!!!edas, paired = TRUE),
                           hist = exda:::convert_eda(!!!edas, paired = FALSE),
                           correlation = tibble(x = as_numeric(edas[[1]])$value,
                                                y = as_numeric(edas[[2]])$value))

    if(type == 'bubble') {

      if(!is.factor(edas[[1]]$value)) edas[[1]] <- exda::as.factor(edas[[1]])

      if(!is.factor(edas[[2]]$value)) edas[[2]] <- exda::as.factor(edas[[2]])

      plotting_tbl <- table(edas[[1]]$value,
                            edas[[2]]$value)

    }


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

      if(!type %in% c('correlation', 'bubble')) {

        naming_vector <- rlang::set_names(eda_names,
                                          levels(plotting_tbl[['group']]))

        plotting_tbl <- dplyr::mutate(plotting_tbl,
                                      group = naming_vector[group],
                                      group = factor(group,
                                                     levels = unname(naming_vector)))

      }

    }

    if(!type %in% c('correlation', 'bubble')) {

      if(type %in% c('bar', 'stack') | (type == 'default' & types[1] == 'factor')) {

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

    ##plotting for factors, bar and stack plot

    if(type %in% c('bar', 'stack') | (type == 'default' & types[1] == 'factor')) {

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

      if(type %in% c('bar', 'default')) {

        if(is.null(x_lab)) x_lab <- 'Category'

        gg_plot <- ggplot2::ggplot(plotting_tbl,
                                   ggplot2::aes(x = .data[['category']],
                                                y = .data[[scale]],
                                                fill = .data[['group']])) +
          ggplot2::geom_bar(stat = 'identity',
                            position = ggplot2::position_dodge(width = 0.9),
                            color = 'black')

        if(show_labels) {

          gg_plot <- gg_plot +
            ggplot2::geom_text(ggplot2::aes(label = plot_lab),
                               size = txt_size,
                               color = txt_color,
                               hjust = 0.5,
                               vjust = -0.4,
                               position = ggplot2::position_dodge(width = 0.9),
                               show.legend = FALSE)

        }

      } else {

        if(is.null(x_lab)) x_lab <- 'Strata'

        plotting_tbl <- dplyr::arrange(plotting_tbl, dplyr::desc(.data[['category']]))

        plotting_tbl <- dplyr::group_by(plotting_tbl, .data[['group']])

        plotting_tbl <- dplyr::mutate(plotting_tbl,
                                      y_pos = cumsum(.data[[scale]]) - 0.5 * .data[[scale]],
                                      plot_lab = ifelse(plot_lab == 0, NA, plot_lab))

        plotting_tbl <- dplyr::ungroup(plotting_tbl)

        gg_plot <- ggplot2::ggplot(plotting_tbl,
                                   ggplot2::aes(x = .data[['group']],
                                                y = .data[[scale]],
                                                fill = .data[['category']])) +
          ggplot2::geom_bar(stat = 'identity',
                            position = ggplot2::position_stack(),
                            color = 'black')

        if(show_labels) {

          if(geom_label) {

            gg_plot <- gg_plot +
              ggplot2::geom_label(ggplot2::aes(label = plot_lab,
                                               y = y_pos),
                                  size = txt_size,
                                  color = txt_color,
                                  hjust = 0.5,
                                  vjust = 0.5,
                                  show.legend = FALSE)

          } else {

            gg_plot <- gg_plot +
              ggplot2::geom_text(ggplot2::aes(label = plot_lab,
                                              y = y_pos),
                                 size = txt_size,
                                 color = txt_color,
                                 hjust = 0.5,
                                 vjust = 0.5,
                                 show.legend = FALSE)

          }

        }

      }

      gg_plot <- gg_plot +
        ggplot2::labs(x = x_lab,
                      y = y_lab)

    } else if(type == 'correlation') {


      gg_plot <- ggplot2::ggplot(plotting_tbl,
                                 ggplot2::aes(x = .data[['x']],
                                              y = .data[['y']])) +
        ggplot2::geom_point(position = ggplot2::position_jitter(width = point_wjitter,
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
                     box = ggplot2::geom_boxplot(alpha = 0.25,
                                                 outlier.color = NA,
                                                 show.legend = FALSE),
                     default = ggplot::geom_violin(alpha = 0.25,
                                                   show.legend = FALSE),
                     violin = ggplot2::geom_violin(alpha = 0.25,
                                                   show.legend = FALSE))

      gg_plot <- ggplot2::ggplot(plotting_tbl,
                                 ggplot2::aes(x = .data[['group']],
                                              y = .data[['variable']],
                                              fill = .data[['group']])) +
        geom +
        ggplot2::geom_point(size = point_size,
                            shape = 21,
                            alpha = point_alpha,
                            position = ggplot2::position_jitter(width = point_wjitter,
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
                                position = ggplot2::position_identity()) +
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

    } else if(type == 'bubble') {

      if(!is.null(eda_names)) {

        x_lab <- eda_names[1]
        y_lab <- eda_names[2]

      } else {

        x_lab <- 'G1'
        y_lab <- 'G2'

      }

      plot_tag <- switch(scale,
                         none = 'Count',
                         fraction = 'Fraction',
                         percent = '%')

      plot_tag <- paste(plot_tag, sum(plotting_tbl), sep = ', complete: n = ')

      plotting_tbl <- switch(scale,
                             none = plotting_tbl,
                             percent = plotting_tbl/sum(plotting_tbl)*100,
                             fraction = plotting_tbl/sum(plotting_tbl))

      plotting_tbl <- as.data.frame(plotting_tbl)

      if(scale == 'none') {

        plotting_tbl <- dplyr::mutate(plotting_tbl,
                                      plot_lab = Freq)

      } else {

        plotting_tbl <- dplyr::mutate(plotting_tbl,
                                      plot_lab = signif(Freq, signif_digits))

      }

      fill_lab <- switch(scale,
                         none = 'Count',
                         fraction = 'Fraction',
                         percent = 'Percent')

      gg_plot <- ggplot2::ggplot(plotting_tbl,
                                 ggplot2::aes(x = .data[['Var1']],
                                              y = .data[['Var2']],
                                              fill = .data[['Freq']],
                                              size = .data[['Freq']])) +
        ggplot2::geom_point(shape = 21,
                            alpha = point_alpha) +
        ggplot2::scale_fill_gradient2(low = 'steelblue',
                                      mid = 'white',
                                      high = 'firebrick',
                                      midpoint = mean(plotting_tbl$Freq)) +
        ggplot2::scale_size_area() +
        labs(x = x_lab,
             y = y_lab,
             fill = fill_lab,
             size = fill_lab)

      if(show_labels) {

        gg_plot <- gg_plot +
          ggplot2::geom_text(ggplot2::aes(label = plot_lab),
                             size = txt_size,
                             hjust = -1.8)

      }

    }

    gg_plot +
      cust_theme +
      ggplot2::labs(title = plot_title,
                    subtitle = plot_subtitle,
                    tag = plot_tag,
                    fill = NULL)

  }


