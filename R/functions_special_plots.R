# Special data representation. An expanding set of functions for visualization
# of the data distribution stats

# Data quantile panel ------

#' Draw data quantile ellipses.
#'
#' @description Draws median (diamond) and user-specified data quantiles
#' (ellipse).
#' @details employs \code{\link[ggforce]{stat_ellip}}.
#' @param data a data frame.
#' @param variables names of the variables to plot.
#' @param split_factor name of the splitting variable.
#' @param quantiles quantiles to be shown on the longer axis (a dimension)
#' of ellipses, a two element vector.
#' @param non_zero logical, should the zero values be removed?
#' @param ell_width length of the shorter ellipse axis (b dimension).
#' @param alpha alpha of the ellipses.
#' @param fill fill color of the ellipses, ignored when split_factor is provided.
#' @param median_color color of the median points,
#' ignored when split factor provided.
#' @param plot_title plot title.
#' @param plot_subtitle plot subtitle.
#' @param plot_tag plot tag.
#' @param x_lab x axis title.
#' @param cust_theme custom ggplot theme.
#' @param ... extra arguments passed to \code{\link[ggforce]{stat_ellip}}.
#' @return a ggplot object.
#' @export

  draw_quantile_elli <- function(data,
                                 variables,
                                 split_factor = NULL,
                                 quantiles = c(0.25, 0.75),
                                 non_zero = FALSE,
                                 ell_width = 0.5,
                                 alpha = 0.5,
                                 fill = 'cornsilk4',
                                 median_color = 'coral3',
                                 plot_title = NULL,
                                 plot_subtitle = NULL,
                                 plot_tag = NULL,
                                 x_lab = 'median with IQR',
                                 cust_theme = ggplot2::theme_classic(), ...) {

    ## entry control

    if(!is.data.frame(data)) {

      stop('Please provide a valid data frame as data argument.', call. = FALSE)

    }

    if(any(!variables %in% names(data))) {

      stop('At least one variable is missing from data.', call. = FALSE)

    }

    classes <- purrr::map_lgl(variables, ~is.numeric(data[[.x]]))

    if(any(!classes)) {

      stop('Variables need to be numeric.', call. = FALSE)

    }

    if(!is.null(split_factor)) {

      if(!split_factor %in% names(data)) {

        stop('split_factor missing from data', call. = FALSE)

      }

    }

    stopifnot(is.numeric(quantiles))

    if(length(quantiles) < 2) stop('At least two quantiles needed.', call. = FALSE)

    quantiles <- quantiles[1:2]

    stopifnot(is.logical(non_zero))
    stopifnot(is.numeric(ell_width))
    stopifnot(is.numeric(alpha))

    if(!any(class(cust_theme) == 'theme')) {

      stop('Please provide a valid ggplot theme as cust_theme.', call. = FALSE)

    }

    ## plotting stats

    if(is.null(split_factor)) {

      data <- data[variables]

      data <- purrr::map(data, as.numeric)

      if(non_zero) {

        data <- purrr::map(data,
                           ~.x[.x != 0])

      }

      stats_lst <- purrr::map(data,
                              quantile,
                              probs = c(0.5, quantiles),
                              na.rm = TRUE)

      plot_tbl <- purrr::map2_dfr(stats_lst, names(stats_lst),
                                 ~tibble::tibble(variable = .y,
                                                 median = .x[1],
                                                 lower = .x[2],
                                                 upper = .x[3]))

    } else {


      data <- data[c(split_factor, variables)]

      data <- plyr::dlply(data,
                          split_factor,
                          dplyr::select, -.data[[split_factor]])


      if(non_zero) {

        data <- purrr::map(data,
                           ~purrr::map(.x, ~.x[.x != 0]))

      }

      stats_lst <- purrr::map(data,
                              ~purrr::map(.x,
                                          quantile,
                                          probs = c(0.5, quantiles),
                                          na.rm = TRUE))

      plot_tbl <- purrr::map(stats_lst,
                             ~purrr::map2_dfr(.x, names(.x),
                                              ~tibble::tibble(variable = .y,
                                                              median = .x[1],
                                                              lower = .x[2],
                                                              upper = .x[3])))

      levs <- names(plot_tbl)

      plot_tbl <- purrr::map2_dfr(plot_tbl, names(plot_tbl),
                                  ~dplyr::mutate(.x,
                                                 !!split_factor := factor(.y,
                                                                          levels = levs)))

    }

    plot_tbl <- dplyr::mutate(plot_tbl,
                              ell_center = (upper - lower)/2 + lower,
                              ar = (upper - lower)/2)

    plot_tbl <- dplyr::arrange(plot_tbl, - median)

    plot_tbl <- dplyr::mutate(plot_tbl, plot_order = nrow(plot_tbl):1)

    ## plotting

    if(is.null(split_factor)) {

      pl <- ggplot2::ggplot(plot_tbl,
                            ggplot2::aes(x = median,
                                         y = reorder(variable, plot_order))) +
        ggforce::stat_ellip(ggplot2::aes(x0 = ell_center,
                                         y0 = plot_order,
                                         a = ar,
                                         b = ell_width,
                                         angle = 0),
                            geom = 'polygon',
                            alpha = alpha,
                            fill = fill, ...) +
        ggplot2::geom_point(size = 3,
                            shape = 18,
                            color = median_color)

    } else {

      pl <- ggplot2::ggplot(dplyr::mutate(plot_tbl,
                                          variable = factor(variable,
                                                            levels = unique(plot_tbl$variable))),
                            ggplot2::aes(x = median,
                                         y = variable,
                                         fill = .data[[split_factor]])) +
        ggforce::stat_ellip(ggplot2::aes(x0 = ell_center,
                                         y0 = as.numeric(variable),
                                         a = ar,
                                         b = ell_width,
                                         angle = 0),
                            geom = 'polygon',
                            alpha = alpha,
                            show.legend = FALSE, ...) +
        ggplot2::geom_point(size = 3,
                            shape = 18,
                            ggplot2::aes(color = .data[[split_factor]]))

    }

    pl +
      cust_theme +
      ggplot2::theme(axis.title.y = ggplot2::element_blank()) +
      ggplot2::labs(title = plot_title,
                    subtitle = plot_subtitle,
                    tag = plot_tag,
                    x = x_lab)

  }

# Violin plot panel ------

#' Draw a panel of violin or box plots.
#'
#' @description Draws violin  or box plots for the specified variable set.
#' Medians and IQR are presented as orange diamonds with whiskers.
#' Points represent single observations.
#' @param data a data frame.
#' @param variables names of the variables to plot.
#' @param split_factor name of the splitting variable.
#' @param distr_geom form of the distribution symbol: a violin or box plot.
#' @param non_zero logical, should the zero values be removed?
#' @param point_size size of data points.
#' @param point_rim_color color of the data point rim, defaults to 'black'.
#' @param point_alpha alpha of data points.
#' @param point_hjitter	point jitter height.
#' @param point_wjitter	point jitter width.
#' @param dodge_w dodge width.
#' @param fill fill color of the violins and data points,
#' ignored when split_factor is provided.
#' @param plot_title plot title.
#' @param plot_subtitle plot subtitle.
#' @param plot_tag plot tag.
#' @param x_lab x axis title.
#' @param cust_theme custom ggplot theme.
#' @param ... extra arguments passed to \code{\link[ggplot2]{geom_violin}} or
#' \code{\link[ggplot2]{geom_box}}.
#' @return a ggplot object.
#' @export

  draw_violin_panel <- function(data,
                                variables,
                                split_factor = NULL,
                                distr_geom = c('violin', 'box'),
                                non_zero = FALSE,
                                point_size = 2,
                                point_rim_color = 'black',
                                point_alpha = 0.5,
                                point_hjitter = 0.15,
                                point_wjitter = 0,
                                dodge_w = 0.75,
                                fill = 'cornsilk4',
                                plot_title = NULL,
                                plot_subtitle = NULL,
                                plot_tag = NULL,
                                x_lab = 'Variable value',
                                cust_theme = ggplot2::theme_classic(), ...) {

    ## entry control

    if(!is.data.frame(data)) {

      stop('Please provide a valid data frame as data argument.', call. = FALSE)

    }

    if(any(!variables %in% names(data))) {

      stop('At least one variable is missing from data.', call. = FALSE)

    }

    classes <- purrr::map_lgl(variables, ~is.numeric(data[[.x]]))

    if(any(!classes)) {

      stop('Variables need to be numeric.', call. = FALSE)

    }

    if(!is.null(split_factor)) {

      if(!split_factor %in% names(data)) {

        stop('split_factor missing from data', call. = FALSE)

      }

      if(!is.factor(data[[split_factor]])) {

        data <- dplyr::mutate(data,
                              !!split_factor := factor(.data[[split_factor]]))

      }

    }

    stopifnot(is.logical(non_zero))
    stopifnot(is.numeric(point_alpha))
    stopifnot(is.numeric(point_wjitter))
    stopifnot(is.numeric(point_hjitter))

    if(!any(class(cust_theme) == 'theme')) {

      stop('Please provide a valid ggplot theme as cust_theme.', call. = FALSE)

    }

    distr_geom <- match.arg(distr_geom, c('violin', 'box'))

    if(is.null(split_factor)) {

      back_geom <- switch(distr_geom,
                          violin = ggplot2::geom_violin(alpha = 0.25,
                                                        fill = fill, ...),
                          box = ggplot2::geom_boxplot(alpha = 0.25,
                                                      fill = fill, ...))

    } else {

      back_geom <-
        switch(distr_geom,
               violin = ggplot2::geom_violin(alpha = 0.25,
                                             position = ggplot2::position_dodge(width = dodge_w), ...),
               box = ggplot2::geom_boxplot(alpha = 0.25,
                                           position = ggplot2::position_dodge(width = dodge_w), ...))

    }

    ## plotting stats

    if(is.null(split_factor)) {

      stats_tbl <- data[variables]

      if(non_zero) {

        stats_tbl <- purrr::map(stats_tbl,
                                ~.x[.x != 0])

      }

      stats_tbl <- purrr::map(stats_tbl,
                              quantile,
                              probs = c(0.5, 0.25, 0.75),
                              na.rm = TRUE)

      stats_tbl <- purrr::map2_dfr(stats_tbl, names(stats_tbl),
                                   ~tibble::tibble(variable = .y,
                                                   value = .x[1],
                                                   lower = .x[2],
                                                   upper = .x[3]))

    } else {


      stats_tbl <- data[c(split_factor, variables)]

      stats_tbl <- plyr::dlply(stats_tbl,
                               split_factor,
                               dplyr::select, -.data[[split_factor]])


      if(non_zero) {

        stats_tbl <- purrr::map(stats_tbl,
                                ~purrr::map(.x, ~.x[.x != 0]))

      }

      stats_tbl <- purrr::map(stats_tbl,
                              ~purrr::map(.x,
                                          quantile,
                                          probs = c(0.5, 0.25, 0.75),
                                          na.rm = TRUE))

      stats_tbl <- purrr::map(stats_tbl,
                              ~purrr::map2_dfr(.x, names(.x),
                                               ~tibble::tibble(variable = .y,
                                                               value = .x[1],
                                                               lower = .x[2],
                                                               upper = .x[3])))

      levs <- names(stats_tbl)

      stats_tbl <- purrr::map2_dfr(stats_tbl, names(stats_tbl),
                                   ~dplyr::mutate(.x,
                                                  !!split_factor := factor(.y,
                                                                           levels = levs)))

    }

    ## long format data table

    data <- tidyr::gather(data,
                          key = 'variable',
                          value = 'value',
                          all_of(variables))

    ## plotting

    if(is.null(split_factor)) {

      pl <- ggplot2::ggplot(data,
                            ggplot2::aes(x = value,
                                         y = variable)) +
        back_geom +
        ggplot2::geom_point(fill = fill,
                            shape = 21,
                            size = point_size,
                            color = point_rim_color,
                            alpha = point_alpha,
                            position = ggplot2::position_jitter(width = point_wjitter,
                                                                height = point_hjitter))


      if(distr_geom == 'violin') {

        pl <- pl +
          ggplot2::geom_errorbarh(data = stats_tbl,
                                  ggplot2::aes(xmin = lower,
                                               xmax = upper),
                                  color = 'orangered3',
                                  size = 0.75,
                                  height = 0) +
          ggplot2::geom_point(data = stats_tbl,
                              size = 3,
                              fill = 'orangered3',
                              shape = 23)

      }


    } else {

      pl <- ggplot2::ggplot(data,
                            ggplot2::aes(x = value,
                                         y = variable,
                                         fill = .data[[split_factor]])) +
        back_geom +
        ggplot2::geom_point(shape = 21,
                            size = point_size,
                            color = point_rim_color,
                            alpha = point_alpha,
                            position = ggplot2::position_jitterdodge(jitter.width = point_wjitter,
                                                                     jitter.height = point_hjitter,
                                                                     dodge.width = dodge_w))

      if(distr_geom == 'violin') {

        pl <- pl +
          ggplot2::geom_errorbarh(data = stats_tbl,
                                  ggplot2::aes(xmin = lower,
                                               xmax = upper),
                                  color = 'orangered3',
                                  size = 0.75,
                                  height = 0,
                                  position = ggplot2::position_dodge(width = dodge_w)) +
          ggplot2::geom_point(data = stats_tbl,
                              size = 3,
                              fill = 'orangered3',
                              shape = 23,
                              position = ggplot2::position_dodge2(width = dodge_w))

      }

    }

    pl +
      cust_theme +
      ggplot2::theme(axis.title.y = ggplot2::element_blank()) +
      ggplot2::labs(title = plot_title,
                    subtitle = plot_subtitle,
                    tag = plot_tag,
                    x = x_lab)

  }

# Draw a bar plot panel -------

#' Draw a panel of statistic values.
#'
#' @description Draws a panel of specified distribution statistic (e.g. median
#' or mean) in form of bars or lines.
#' @param data a data frame.
#' @param variables names of the variables to plot.
#' @param split_factor name of the splitting variable.
#' @param stat distribution statistic, currently mean or median supported.
#' @param err_stat error/distribution width statistic, SD ('sd'), SEM ('se'),
#' 95% CI from the normal distribution (2se), IQR (iqr) or 95% percentile
#' ('95perc') are currently supported.
#' @param form for of the plot: bar (default) or line.
#' @param alpha alpha of the bar or distribution width ribbon.
#' @param fill fill color of the violins and data points,
#' ignored when split_factor is provided.
#' @param dodge_w dodge width.
#' @param plot_title plot title.
#' @param plot_subtitle plot subtitle.
#' @param plot_tag plot tag.
#' @param x_lab x axis title.
#' @param cust_theme custom ggplot theme.
#' @param ... extra arguments passed to \code{\link[ggplot2]{geom_ribbon}}
#' @return a ggplot object.
#' @export

  draw_stat_panel <- function(data,
                              variables,
                              split_factor = NULL,
                              stat = c('mean', 'median'),
                              err_stat = c('none', 'sd', 'se',
                                           '2se', 'iqr', '95perc'),
                              form = c('bar', 'line'),
                              alpha = 0.5,
                              fill = 'cornsilk4',
                              dodge_w = if(form == 'bar') 0.9 else 0,
                              plot_title = NULL,
                              plot_subtitle = NULL,
                              plot_tag = NULL,
                              x_lab = 'Mean',
                              cust_theme = ggplot2::theme_classic(), ...) {

    ## entry control

    if(!is.data.frame(data)) {

      stop('Please provide a valid data frame as data argument.', call. = FALSE)

    }

    if(any(!variables %in% names(data))) {

      stop('At least one variable is missing from data.', call. = FALSE)

    }

    classes <- purrr::map_lgl(variables, ~is.numeric(data[[.x]]))

    if(any(!classes)) {

      stop('Variables need to be numeric.', call. = FALSE)

    }

    if(!is.null(split_factor)) {

      if(!split_factor %in% names(data)) {

        stop('split_factor missing from data', call. = FALSE)

      }

      if(!is.factor(data[[split_factor]])) {

        data <- dplyr::mutate(data,
                              !!split_factor := factor(.data[[split_factor]]))

      }

    }

    if(!any(class(cust_theme) == 'theme')) {

      stop('Please provide a valid ggplot theme as cust_theme.', call. = FALSE)

    }

    stat <- match.arg(stat[1], c('mean', 'median'))

    form <- match.arg(form[1], c('bar', 'line'))

    err_stat <- match.arg(err_stat[1], c('none', 'sd', 'se',
                                         '2se', 'iqr', '95perc'))

    stat_fun <- switch(stat,
                       mean = function(x) mean(x, na.rm = TRUE),
                       median = function(x) median(x, na.rm = TRUE))

    err_fun <- switch(err_stat,
                      none = function(x) c(NA, NA),
                      sd = function(x) c(mean(x, na.rm = TRUE) - sd(x, na.rm = TRUE),
                                         mean(x, na.rm = TRUE) + sd(x, na.rm = TRUE)),
                      se = function(x) c(mean(x, na.rm = TRUE) - sciplot::se(x, na.rm = TRUE),
                                         mean(x, na.rm = TRUE) + sciplot::se(x, na.rm = TRUE)),
                      `2se` = function(x) c(mean(x, na.rm = TRUE)  + qnorm(0.025) * sciplot::se(x, na.rm = TRUE),
                                            mean(x, na.rm = TRUE) + qnorm(0.975) * sciplot::se(x, na.rm = TRUE)),
                      iqr = function(x) quantile(x, probs = c(0.25, 0.75), na.rm = TRUE),
                      `95perc` = function(x) quantile(x, probs = c(0.025, 0.975), na.rm = TRUE))

    ## statistic table

    if(is.null(split_factor)) {

      stats_tbl <- data[variables]

      stats_tbl <- purrr::map(stats_tbl, ~c(stat_fun(.x), err_fun(.x)))

      stats_tbl <- purrr::map2_dfr(stats_tbl, names(stats_tbl),
                                   ~tibble::tibble(variable = .y,
                                                   value = .x[1],
                                                   lower = .x[2],
                                                   upper = .x[3]))

    } else {

      stats_tbl <- data[c(split_factor, variables)]

      stats_tbl <- plyr::dlply(stats_tbl,
                               split_factor,
                               dplyr::select, -.data[[split_factor]])

      stats_tbl <- purrr::map(stats_tbl,
                              ~purrr::map(.x, ~c(stat_fun(.x), err_fun(.x))))

      stats_tbl <- purrr::map(stats_tbl,
                              ~purrr::map2_dfr(.x, names(.x),
                                               ~tibble::tibble(variable = .y,
                                                               value = .x[1],
                                                               lower = .x[2],
                                                               upper = .x[3])))

      levs <- names(stats_tbl)

      stats_tbl <- purrr::map2_dfr(stats_tbl, names(stats_tbl),
                                   ~dplyr::mutate(.x,
                                                  !!split_factor := factor(.y,
                                                                           levels = levs)))

    }

    ## drawing

    if(is.null(split_factor)) {

      pl <- ggplot2::ggplot(dplyr::mutate(stats_tbl, groupping = 'a'),
                            ggplot2::aes(x = value,
                                         y = variable))


      if(form == 'bar') {

        pl <- pl +
          ggplot2::geom_bar(stat = 'identity',
                            color = 'black',
                            fill = fill,
                            alpha = alpha) +
          ggplot2::geom_errorbarh(ggplot2::aes(xmin = lower,
                                               xmax = upper),
                                  color = 'black',
                                  height = 0.1)

      } else {

        pl <- pl +
          ggplot2::geom_ribbon(ggplot2::aes(x = value,
                                            xmin = lower,
                                            xmax = upper,
                                            group = groupping),
                               color = fill,
                               fill = fill,
                               alpha = alpha,
                               orientation = 'y') +
          ggplot2::geom_line(ggplot2::aes(group = groupping),
                             color = NA,
                             size = 0.75,
                             orientation = 'y')


      }

    } else {

      pl <- ggplot2::ggplot(stats_tbl,
                            ggplot2::aes(x = value,
                                         y = variable,
                                         fill = .data[[split_factor]],
                                         color = .data[[split_factor]]))

      if(form == 'bar') {

        pl <- pl +
          ggplot2::geom_bar(stat = 'identity',
                            color = 'black',
                            alpha = alpha,
                            position = ggplot2::position_dodge(width = dodge_w)) +
          ggplot2::geom_errorbarh(ggplot2::aes(xmin = lower,
                                               xmax = upper),
                                  color = 'black',
                                  height = 0.1,
                                  position = ggplot2::position_dodge(width = dodge_w))

      } else {

        pl <- pl +
          ggplot2::geom_ribbon(ggplot2::aes(x = value,
                                            xmin = lower,
                                            xmax = upper,
                                            group = .data[[split_factor]]),
                               color = NA,
                               orientation = 'y',
                               alpha = alpha,
                               position = ggplot2::position_dodge(width = dodge_w)) +
          ggplot2::geom_line(ggplot2::aes(x = value,
                                          group = .data[[split_factor]]),
                             orientation = 'y',
                             size = 0.75,
                             position = ggplot2::position_dodge(width = dodge_w))

      }

    }

    pl +
      cust_theme +
      ggplot2::labs(title = plot_title,
                    subtitle = plot_subtitle,
                    tag = plot_tag,
                    x = x_lab)

  }

# Draw a panel with frequencies --------

#' Draw panel with frequencies or counts
#'
#' @description Draws a panel of stacked or dodged bar plots with frequencies
#' or counts for the given categorical variables. Works best, when the variables
#' share the same levels.
#' @inheritParams draw_stat_panel
#' @param variables names of the variables to plot, they need to be factors.
#' @param split name of the splitting variable (optional). If provided,
#' the panel will be faceted by the variables and the split_factor presented
#' in the Y axis.
#' @param rm_na logical, should missing observations be removed prior
#' to plotting? If FALSE,
#' @param scale statistic to be plotted: percent (default), fraction or counts
#' of observations for each variable level.
#' @param form form of the bar plot: stacked bars (default) or dodged bars.
#' @param show_labels logical, should the data bars be labeled with the percent,
#' fraction or count value?
#' @param txt_size size of the label text, ignored if show_labels is FALSE.
#' @param geom_labe logical, should the bar label be presented as a geom_label?
#' If FALSE, plain text is displayed.
#' @param signif_digits significant digits to be shown in the bar labels.
#' @param y_lab Y axis title.
#' @param ... additional arguments passed to \code{\link[ggplot2]{facet_grid}}.
#' Ignored, if split_factor is NULL.
#' @return a ggplot object.
#' @export

  draw_freq_panel <- function(data,
                              variables,
                              split_factor = NULL,
                              rm_na = TRUE,
                              scale = c('percent', 'fraction', 'count'),
                              form = c('stack', 'dodge'),
                              show_labels = FALSE,
                              txt_size = 2.75,
                              geom_label = TRUE,
                              signif_digits = 2,
                              alpha = 1,
                              dodge_w = 0.9,
                              plot_title = NULL,
                              plot_subtitle = NULL,
                              plot_tag = NULL,
                              x_lab = '% of cohort',
                              y_lab = split_factor,
                              cust_theme = ggplot2::theme_classic(), ...) {

    ## entry control

    stopifnot(is.logical(rm_na))
    stopifnot(is.logical(show_labels))
    stopifnot(is.numeric(txt_size))
    stopifnot(is.logical(geom_label))
    stopifnot(is.numeric(signif_digits))
    stopifnot(is.numeric(alpha))
    stopifnot((alpha >= 0 & alpha <= 1))
    stopifnot(is.numeric(dodge_w))
    stopifnot(!is.null(variables))

    if(!is.data.frame(data)) {

      stop('Please provide a valid data frame as data argument.', call. = FALSE)

    }

    if(any(!variables %in% names(data))) {

      stop('At least one variable is missing from data.', call. = FALSE)

    }

    classes <- purrr::map_lgl(variables, ~is.factor(data[[.x]]))

    if(any(!classes)) {

      stop('Variables need to be factors.', call. = FALSE)

    }

    if(!is.null(split_factor)) {

      if(!split_factor %in% names(data)) {

        stop('split_factor missing from data', call. = FALSE)

      }

      if(!is.factor(data[[split_factor]])) {

        data <- dplyr::mutate(data,
                              !!split_factor := factor(.data[[split_factor]]))

      }

    }

    if(!any(class(cust_theme) == 'theme')) {

      stop('Please provide a valid ggplot theme as cust_theme.', call. = FALSE)

    }

    scale <- match.arg(scale[1], c('percent', 'fraction', 'count'))

    x_var <- switch(scale,
                    percent = 'perc',
                    fraction = 'frac',
                    count = 'n')

    form <- match.arg(form[1], c('stack', 'dodge'))

    pos <- switch(form,
                  stack = ggplot2::position_stack(),
                  dodge = ggplot2::position_dodge(width = dodge_w))

    ## frequency table

    if(is.null(split_factor)) {

      data <- data[variables]

      count_lst <- purrr::map(variables,
                              ~dplyr::count(data, .data[[.x]]))

      count_lst <- rlang::set_names(count_lst, variables)

      count_lst <- purrr::map(count_lst, rlang::set_names, c('level', 'n'))

      if(rm_na) {

        count_lst <- purrr::map(count_lst,
                                dplyr::filter,
                                !is.na(level))

      }

      count_lst <- purrr::map(count_lst,
                              dplyr::mutate,
                              frac = n/sum(n),
                              perc = frac * 100)

      plot_tbl <-
        purrr::map2_dfr(count_lst,
                        names(count_lst),
                        ~dplyr::mutate(.x,
                                       variable = factor(.y, levels = variables)))

      plot_tbl <- dplyr::arrange(plot_tbl, variable, desc(level))

      plot_tbl <-
        plyr::ddply(plot_tbl, 'variable',
                    dplyr::mutate,
                    label = signif(.data[[x_var]], signif_digits),
                    x_pos = cumsum(.data[[x_var]]) - 0.5 * .data[[x_var]])

      plot_tbl <- dplyr::arrange(plot_tbl, variable, level)

    } else {

      data <- data[c(split_factor, variables)]

      count_lst <-
        plyr::dlply(data, split_factor,
                    function(chunk) purrr::map(variables,
                                               ~dplyr::count(chunk,
                                                             .data[[.x]])))

      count_lst <- purrr::map(count_lst, rlang::set_names, variables)

      count_lst <- purrr::map(count_lst,
                              ~purrr::map(.x, rlang::set_names, c('level', 'n')))

      if(rm_na) {

        count_lst <- purrr::map(count_lst,
                                ~purrr::map(.x, dplyr::filter, !is.na(level)))

      }

      count_lst <- purrr::map(count_lst,
                              ~purrr::map(.x,
                                          mutate,
                                          frac = n/sum(n),
                                          perc = frac * 100))

      plot_tbl <-
        purrr::map(count_lst,
                   ~purrr::map2_dfr(.x, names(.x),
                                    ~dplyr::mutate(.x,
                                                   variable = factor(.y,
                                                                     levels = variables))))

      plot_tbl <- purrr::map2_dfr(plot_tbl, names(plot_tbl),
                                  ~dplyr::mutate(.x, !!split_factor := .y))

      plot_tbl <- dplyr::arrange(plot_tbl,
                                 .data[[split_factor]], variable, desc(level))

      plot_tbl <- plyr::ddply(plot_tbl,
                              c(split_factor, 'variable'),
                              dplyr::mutate,
                              label = signif(.data[[x_var]], signif_digits),
                              x_pos = cumsum(.data[[x_var]]) - 0.5 * .data[[x_var]])

      plot_tbl <- dplyr::arrange(plot_tbl,
                                 .data[[split_factor]], variable, level)

    }

    ## plotting

    if(is.null(split_factor)) {

      panel <- ggplot2::ggplot(plot_tbl,
                               ggplot2::aes(x = .data[[x_var]],
                                            y = variable,
                                            fill = level)) +
        ggplot2::geom_bar(stat = 'identity',
                          position = pos,
                          color = 'black',
                          alpha = alpha)

    } else {

      panel <- ggplot2::ggplot(plot_tbl,
                               ggplot2::aes(x = .data[[x_var]],
                                            y = .data[[split_factor]],
                                            fill = level)) +
        ggplot2::geom_bar(stat = 'identity',
                          position = pos,
                          color = 'black',
                          alpha = alpha) +
        ggplot2::facet_grid(variable ~ ., ...)

    }


    if(show_labels) {

      if(form == 'dodge') {

        panel <- panel +
          ggplot2::geom_text(ggplot2::aes(label = .data[['label']]),
                             size = txt_size,
                             hjust = -0.6,
                             vjust = 0.5,
                             position = pos,
                             show.legend = FALSE)

      } else {

        if(geom_label) {

          panel <- panel +
            ggplot2::geom_label(ggplot2::aes(label = .data[['label']],
                                             x = x_pos),
                                size = txt_size,
                                show.legend = FALSE)

        } else {

          panel <- panel +
            ggplot2::geom_text(ggplot2::aes(label = .data[['label']],
                                            x = x_pos),
                               size = txt_size,
                               show.legend = FALSE)

        }

      }

    }

    panel +
      cust_theme +
      ggplot2::labs(title = plot_title,
                    subtitle = plot_subtitle,
                    tag = plot_tag,
                    x = x_lab,
                    y = y_lab)

  }

# END ------
