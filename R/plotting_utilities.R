# Plotting utilities for single `eda` objects

#' Plot utilities for single `eda` objects.
#'
#' @description
#' Bar, stack, violin and box plots, histograms, violin and quantile - quantile
#' (QQ) plots for factor and numeric \code{\link{eda}} objects.
#'
#' @return
#' A `ggplot` object.
#'
#' @param eda_object and \code{\link{eda}} object.
#' @param type plot type.
#' @param .drop should empty levels of a factor be dropped.
#' @param scale type of frequency data to be presented in bar and stack plots:
#' none (counts) or percents,
#' @param signif_digits significant digits used to round numeric values displayed
#' in the plot (e.g. percentages).
#' @param shape_fill fill color of bars, boxes, violins, and histograms.
#' @param shape_color color of rims of bars, boxes, violins, and histograms.
#' @param shape_alpha alpha (opacity) of bars, boxes, violins, and histograms.
#' @param point_color color of data points.
#' @param point_size size of data points
#' @param point_alpha alpha (opacity) of data points.
#' @param point_hjitter horizontal jittering of data points
#' @param point_wjitter vertical jittering of data points.
#' @param line_color color of interquartile whiskers in violin plots, and
#' lines depicting statistics in histograms and QQ plots.
#' @param line_width width of interquartile whiskers in violin plots, and
#' lines depicting statistics in histograms and QQ plots.
#' @param show_txt logical, should text labels with frequencies be
#' displayed in the plot?
#' @param txt_size size of text labels in the plots.
#' @param show_stats logical, should vertical lines representing median (solid)
#' and interquartile range (dashed) be shown in the histogram?
#' @param plot_title plot title.
#' @param plot_subtitle plot subtitle; if `NULL` numbers of all and complete
#' observations are shown here.
#' @param x_txt text to be displayed in the X axis. Concerns stack, box,
#' and violin plots.
#' @param x_lab title of the X axis.
#' @param y_lab title of the Y axis.
#' @param cust_theme custom `ggplot`'s `theme` object.
#' @param ... additional arguments passed to \code{\link[ggplot2]{geom_boxplot}},
#' \code{\link[ggplot2]{geom_violin}}, \code{\link[ggplot2]{geom_histogram}}.

  plot_factor <- function(eda_object,
                          .drop = TRUE,
                          type = c("bar", "stack"),
                          scale = c("none", "percent"),
                          signif_digits = 2,
                          shape_fill = "steelblue",
                          shape_color = "black",
                          shape_alpha = 1,
                          cust_theme = theme_classic(),
                          show_txt = TRUE,
                          txt_size = 2.75,
                          plot_title = NULL,
                          plot_subtitle = NULL,
                          x_txt = "",
                          x_lab = "category", ...) {

    ## entry control -------

    stopifnot(is_eda(eda_object))
    stopifnot(is.factor(eda_object))

    stopifnot(is.logical(.drop))
    .drop <- .drop[1]

    if(.drop) eda_object <- droplevels(eda_object)

    type <- match.arg(type[1], c("bar", "stack"))

    scale <- match.arg(scale[1], c("none", "percent"))

    stopifnot(is.numeric(signif_digits))
    signif_digits <- as.integer(signif_digits[1])

    stopifnot(is.numeric(shape_alpha))
    stopifnot(is.numeric(txt_size))

    if(!is_theme(cust_theme)) {

      stop("`cust_theme` has to be a valid `ggplot` theme.", call. = FALSE)

    }

    stopifnot(is.logical(show_txt))
    show_txt <- show_txt[1]

    ## plotting data and meta-data -----

    plot_data <- frequency(eda_object)

    if(is.null(plot_subtitle)) {

      plot_subtitle <- paste0("total: n = ", plot_data[["n_total"]][1],
                              ", complete: n = ", plot_data[["n_complete"]][1])

    }

    plot_var <- switch(scale,
                       none = "n",
                       percent = "percent_complete")

    y_lab <- switch(scale,
                    none = "observations, N",
                    percent = "% of complete observations")

    category <- NULL

    if(scale == "none") {

      plot_data[["plot_label"]] <- plot_data[["n"]]

    } else {

      plot_data[["plot_label"]] <-
        paste0(signif(plot_data[["percent_complete"]], signif_digits), "%")

    }

    if(type == "stack") {

      ## position of the labels

      plot_data <- arrange(plot_data, desc(category))

      plot_data[["plot_pos"]] <-
        cumsum(plot_data[[plot_var]]) - 0.5 * plot_data[[plot_var]]

    }

    ## plot -------

    if(type == "bar") {

      fct_plot <- ggplot(plot_data,
                         aes(x = .data[["category"]],
                             y = .data[[plot_var]])) +
        geom_bar(stat = "identity",
                 fill = shape_fill,
                 color = shape_color,
                 alpha = shape_alpha) +
        labs(x = x_lab)

      if(show_txt) {

        fct_plot <- fct_plot +
          geom_text(aes(label = .data[["plot_label"]]),
                    hjust = 0.5,
                    vjust = -0.4,
                    size = txt_size)

      }

    } else {

      fct_plot <- ggplot(plot_data,
                         aes(x = x_txt,
                             y = .data[[plot_var]],
                             fill = .data[["category"]])) +
        geom_bar(stat = "identity",
                 color = shape_color,
                 alpha = shape_alpha)

      if(show_txt) {

        fct_plot <- fct_plot +
          geom_label(aes(label = .data[["plot_label"]],
                         y = .data[["plot_pos"]]),
                     color = shape_color,
                     size = txt_size,
                     show.legend = FALSE)

      }

    }

    fct_plot +
      cust_theme +
      labs(title = plot_title,
           subtitle = plot_subtitle,
           y = y_lab)

  }

#' @rdname plot_factor

  plot_numeric <- function(eda_object,
                           type = c("violin", "box"),
                           shape_fill = "steelblue",
                           shape_color = "black",
                           shape_alpha = 0.3,
                           point_color = 'gray40',
                           point_size = 2,
                           point_alpha = 0.75,
                           point_hjitter = 0,
                           point_wjitter = 0.1,
                           line_color = "orangered3",
                           line_width = 0.75,
                           cust_theme = theme_classic(),
                           plot_title = NULL,
                           plot_subtitle = NULL,
                           x_txt = "",
                           x_lab = "",
                           y_lab = "",
                           ...) {

    ## input controls --------

    stopifnot(is_eda(eda_object))
    stopifnot(is.numeric(eda_object))

    type <- match.arg(type[1], c("violin", "box"))

    stopifnot(is.numeric(shape_alpha))
    shape_alpha <- shape_alpha[1]

    stopifnot(is.numeric(point_size))
    point_size <- point_size[1]

    stopifnot(is.numeric(point_alpha))
    point_alpha <- point_alpha[1]

    stopifnot(is.numeric(point_hjitter))
    point_hjitter <- point_hjitter[1]

    stopifnot(is.numeric(point_wjitter))
    point_wjitter <- point_wjitter[1]

    if(!is_theme(cust_theme)) {

      stop("`cust_theme` has to be a valid `ggplot` theme.", call. = FALSE)

    }

    ## plotting data and metadata ---------

    n_numbers <- nobs(eda_object)

    if(is.null(plot_subtitle)) {

      plot_subtitle <- paste0("total: n = ", n_numbers[["n"]][1],
                              ", complete: n = ", n_numbers[["n"]][2])

    }

    plot_data <- as.data.frame(eda_object, col.names = "value")

    stat_data <- column_to_rownames(summary(eda_object,
                                            pub_styled = FALSE),
                                    "statistic")

    stat_data <-
      as.data.frame(t(stat_data[c("median", "perc_25", "perc_75"), "value", drop = FALSE]))

    ## plots ---------

    num_plot <- ggplot(plot_data,
                       aes(x = x_txt,
                           y = .data[["value"]]))

    if(type == "box") {

      num_plot <- num_plot +
        geom_boxplot(outlier.color = NA,
                     fill = shape_fill,
                     color = shape_color,
                     alpha = shape_alpha, ...)

    } else {

      num_plot <- num_plot +
        geom_violin(fill = shape_fill,
                    color = shape_color,
                    alpha = shape_alpha, ...) +
        geom_point(data = stat_data,
                   aes(x = x_txt,
                       y = .data[["median"]]),
                   shape = 23,
                   fill = line_color,
                   color = line_color,
                   size = point_size + 1) +
        geom_errorbar(data = stat_data,
                      aes(x = x_txt,
                          y = .data[["median"]],
                          ymin = .data[["perc_25"]],
                          ymax = .data[["perc_75"]]),
                      color = line_color,
                      linewidth = line_width,
                      width = 0)

    }

    num_plot +
      geom_point(position = position_jitter(width = point_wjitter,
                                            height = point_hjitter),
                 shape = 21,
                 size = point_size,
                 fill = point_color,
                 alpha = point_alpha) +
      cust_theme +
      labs(title = plot_title,
           subtitle = plot_subtitle,
           y = y_lab,
           x = x_lab)

  }

#' @rdname plot_factor

  plot_histogram <- function(eda_object,
                             shape_fill = "steelblue",
                             shape_color = "black",
                             shape_alpha = 0.75,
                             show_stats = TRUE,
                             line_color = "orangered3",
                             line_width = 0.75,
                             cust_theme = theme_classic(),
                             plot_title = NULL,
                             plot_subtitle = NULL,
                             x_lab = "value",
                             y_lab = "observations, N",
                             ...) {

    ## input controls --------

    stopifnot(is_eda(eda_object))

    stopifnot(is.numeric(shape_alpha))
    shape_alpha <- shape_alpha[1]

    if(!is_theme(cust_theme)) {

      stop("`cust_theme` has to be a valid `ggplot` theme.", call. = FALSE)

    }

    stopifnot(is.logical(show_stats))
    show_stats <- show_stats[1]

    ## plotting data --------

    n_numbers <- nobs(eda_object)

    if(is.null(plot_subtitle)) {

      plot_subtitle <- paste0("total: n = ", n_numbers[["n"]][1],
                              ", complete: n = ", n_numbers[["n"]][2])

    }

    plot_data <- as.data.frame(eda_object, col.names = "value")

    ## histogram --------

    hist_plot <- ggplot(plot_data,
                        aes(x = .data[["value"]])) +
      geom_histogram(color = shape_color,
                     fill = shape_fill,
                     alpha = shape_alpha, ...) +
      cust_theme +
      labs(title = plot_title,
           subtitle = plot_subtitle,
           x = x_lab,
           y = y_lab)

    if(show_stats) {

      hist_plot <- hist_plot +
        geom_vline(xintercept = median(eda_object,
                                       plain = TRUE),
                   color = line_color,
                   linewidth = line_width) +
        geom_vline(xintercept = quantile(eda_object,
                                         probs = 0.25,
                                         plain = TRUE),
                   color = line_color,
                   linewidth = line_width,
                   linetype = "dashed") +
        geom_vline(xintercept = quantile(eda_object,
                                         probs = 0.75,
                                         plain = TRUE),
                   color = line_color,
                   linewidth = line_width,
                   linetype = "dashed")

    }

    hist_plot

  }

#' @rdname plot_factor

  plot_qq <- function(eda_object,
                      point_color = 'steelblue',
                      point_size = 2,
                      point_alpha = 0.75,
                      point_hjitter = 0,
                      point_wjitter = 0,
                      line_color = "orangered3",
                      line_width = 0.75,
                      plot_title = NULL,
                      plot_subtitle = NULL,
                      x_lab = "quantiles, theoretical normal distribution",
                      y_lab = "quantiles, observed distribution",
                      cust_theme = theme_classic(), ...) {

    ## input control -------

    stopifnot(is_eda(eda_object))

    stopifnot(is.numeric(point_size))
    point_size <- point_size[1]

    stopifnot(is.numeric(point_alpha))
    point_alpha <- point_alpha[1]

    stopifnot(is.numeric(point_hjitter))
    point_hjitter <- point_hjitter[1]

    stopifnot(is.numeric(point_wjitter))
    point_wjitter <- point_wjitter[1]

    stopifnot(is.numeric(line_width))
    line_width <- line_width[1]

    if(!is_theme(cust_theme)) {

      stop("`cust_theme` has to be a valid `ggplot` theme.", call. = FALSE)

    }

    ## plotting data and meta-data --------

    n_numbers <- nobs(eda_object)

    if(is.null(plot_subtitle)) {

      plot_subtitle <- paste0("total: n = ", n_numbers[["n"]][1],
                              ", complete: n = ", n_numbers[["n"]][2])

    }

    plot_data <- as.data.frame(eda_object, col.names = "value")

    ## the QQ plot -------

    ggplot(plot_data,
           aes(sample = .data[["value"]])) +
      geom_qq(shape = 21,
              size = point_size,
              alpha = point_alpha,
              fill = point_color,
              position = position_jitter(width = point_wjitter,
                                         height = point_hjitter)) +
      geom_qq_line(color = line_color,
                   linewidth = line_width) +
      cust_theme +
      labs(title = plot_title,
           subtitle = plot_subtitle,
           x = x_lab,
           y = y_lab)

  }

# END --------

