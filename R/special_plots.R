# Special plots and plot panels for displaying distribution of multiple
# variables in one graphics

#' Panel with distribution of multiple variables in a data frame.
#'
#' @description
#' `ggplot` graphics with plots of distribution of multiple numeric or factor
#' variables in a data frame.
#'
#' @details
#' `draw_numeric_panel()` creates panel/compound graphs with distribution of
#' numeric variables presented in violins or box plots, or summary graphics of
#' centrality and distribution statistics in bar/whiskers plots, ribbon
#' and Forest plots.
#' The `central_stat` argument specifies which statistic of central tendency, mean
#' or median, will be presented in bars, ribbon plots (as thick line), and
#' Forest plots (as point).
#' The `distribution_stat` argument specifies which statistic of distribution/dispersion
#' will be presented as whiskers or ribbons (`"sem"`: standard error of the mean,
#' `"sd"`: standard deviation, `"iqr"`: interquartile range, `"95%"`: 95% quantile
#' of distribution/range where 95% of non-extreme observations are located,
#' `"2sem"`: double standard of the mean, `"2sd"`: double standard deviation,
#' `"none"`: no dispersion statistic is shown).
#'
#' @return a `ggplot` object.
#'
#' @inheritParams plot_df_factor
#' @param variables a character vector with variable names.
#' @param split_factor optional, a character string with the name of the factor
#' defining analysis groups. If `split_factor = NULL`, the distributions in
#' the whole data frame will be shown.
#' @param type plot type. In ribbon plots, N numbers of observations in
#' the `split_factor`'s categories won't be shown; the numbers displayed in the
#' Y axis when `n_labs = TRUE` refer to the sum of complete observations for the
#' given variable in the whole data frame.
#' @param central_stat statistic of centrality to be presented in bar, ribbon
#' and Forest plots. Ignored for other plot types.
#' @param distribution_stat statistic of distribution/dispersion to be presented
#' in bar, ribbon, and Forest plots as ribbons or whiskers.
#' Ignored for other plot types.
#' @param shape_fill fill color of bars, boxes, violins, etc. Ignored if
#' `split_factor` is provided.
#' @param point_color color of data points. Ignored if
#' `split_factor` is provided.
#' @param line_color color of lines in ribbon and Forest plots; ignored if
#' `split_factor` is provided. In violin plots: color of diamonds and whiskers
#' depicting medians with interquartile ranges.
#' @param line_alpha alpha/opacity of lines in ribbon and Forest plots.
#' @param n_labs logical. If TRUE, N numbers of complete observations are displayed
#' in the axis labels. Defaults to `TRUE`.
#' @param n_lab_sep separator between the category name and N number of observations.
#' Relevant for axis labels only if `n_labs = TRUE`.
#' @param labeller_fun a function used to transform names of variables presented
#' in the plot axes and facets. Defaults to identity.
#' @param show_stats logical. Violin plots: should diamonds and whiskers
#' representing median with interquartile range be displayed?
#' @param overlap_density logical, should histograms and density plots for particular
#' variables be overlaid? Concerns only histogram and density plots, when `split_factor`
#' is specified. If `TRUE`, N numbers of observations in the `split_factor`'s
#' categories won't be shown.
#' @param ribbon_line_width width of lines of the distribution ribbons in
#' ribbon plots.
#' @param ... additional arguments passed to `ggplot` `geom` objects.
#' For violin and box plots, the arguments are passed to
#' \code{\link[ggplot2]{geom_violin}} and \code{\link[ggplot2]{geom_boxplot}},
#' for bar plots they are passed to \code{\link[ggplot2]{geom_bar}}, for
#' ribbon plots the arguments are passed to \code{\link[ggplot2]{geom_line}},
#' and for Forest plots to \code{\link[ggplot2]{geom_point}}.
#'
#' @export

  draw_numeric_panel <- function(data,
                                 variables,
                                 split_factor = NULL,
                                 type = c("violin", "box",
                                          "histogram", "density",
                                          "bar", "ribbon", "forest"),
                                 central_stat = c("mean", "median"),
                                 distribution_stat = c("sem", "sd", "iqr", "95%",
                                                       "2sem", "2sd", "none"),
                                 palette = tableau10_colors(),
                                 shape_fill = "steelblue",
                                 shape_color = "black",
                                 shape_alpha = NULL,
                                 point_size = 2,
                                 point_color = shape_fill,
                                 point_alpha = NULL,
                                 point_wjitter = 0,
                                 point_hjitter = 0.1,
                                 line_color = NULL,
                                 line_width = 0.75,
                                 line_alpha = 1,
                                 ribbon_line_width = 0.5 * line_width,
                                 cust_theme = eda_classic_theme(),
                                 plot_title = NULL,
                                 plot_subtitle = NULL,
                                 x_lab = NULL,
                                 y_lab = NULL,
                                 fill_lab = NULL,
                                 n_labs = TRUE,
                                 n_lab_sep = "\n",
                                 labeller_fun = identity,
                                 overlap_density = FALSE,
                                 show_stats = TRUE,
                                 ...) {

    ## entry control -------------

    type <- match.arg(type[1],
                      c("violin", "box",
                        "histogram", "density",
                        "bar", "ribbon", "forest"))

    central_stat <- match.arg(central_stat[1], c("mean", "median"))

    distribution_stat <-
      match.arg(distribution_stat[1],
                c("sem", "sd", "iqr", "95%",
                  "2sem", "2sd", "none"))

    stopifnot(is.numeric(point_size))
    point_size <- point_size[1]

    stopifnot(is.numeric(point_wjitter))
    point_wjitter <- point_wjitter[1]

    stopifnot(is.numeric(point_hjitter))
    point_wjitter <- point_wjitter[1]

    stopifnot(is.numeric(line_width))
    line_width <- line_width[1]

    if(!is.null(cust_theme)) {

      if(!is_theme(cust_theme)) {

        stop("`cust_theme` has to be a valid `ggplot` theme.", call. = FALSE)

      }

    }

    stopifnot(is.logical(n_labs))
    n_labs <- n_labs[1]

    if(!is_function(labeller_fun)) {

      stop("`labeller_fun` has to be a function.", call. = FALSE)

    }

    stopifnot(is.logical(overlap_density))
    overlap_density <- overlap_density[1]

    stopifnot(is.logical(show_stats))
    show_stats <- show_stats[1]

    ## X and fill axis titles ------------

    if(is.null(x_lab)) {

      if(type %in% c("violin", "box")) {

        x_lab <- "value"

      } else {

        x_lab <- central_stat

        x_lab_suffix <-
          switch(distribution_stat,
                 none = "",
                 sem = ", SEM",
                 sd = ", SD",
                 iqr = ", IQR",
                 `95%` = ", 95th percentile",
                 `2sem` = ", 2\u00D7SEM",
                 `2sd` = ", 2\u00D7SEM")

        x_lab <- paste0(x_lab, x_lab_suffix)

      }

    }

    if(is.null(fill_lab)) fill_lab <- split_factor

    ## plotting data --------

    data <- validate_multi_df(data,
                              variables,
                              split_factor,
                              format = "numeric")

    if(is.null(split_factor)) {

      variables <- names(data)

    } else {

      variables <- names(data)[names(data) != split_factor]

    }

    if(is.null(plot_subtitle)) {

      plot_subtitle <-
        paste("total: n = ", attr(data, "n_numbers")["total"])

    }

    plot_data <- pivot_longer(data,
                              cols = all_of(variables),
                              names_to = "variable",
                              values_to = "value")

    n_data <- attr(data, "n_categories")

    if(is.null(split_factor)) {

      n_data <- data.frame(variable = names(n_data),
                           n = as.numeric(n_data))

      plot_data <- left_join(plot_data,
                             n_data,
                             by = "variable")

    } else {

      plot_data <- left_join(plot_data,
                             n_data,
                             by = c(split_factor, "variable"))

    }

    plot_data <- plot_data[!is.na(plot_data[["value"]]), ]

    plot_data[["variable"]] <-
      factor(plot_data[["variable"]], variables)

    ## centrality and distribution statistics --------

    if((type == "violin" & show_stats) |
       (type %in% c("bar", "ribbon", "forest"))) {

      stat_data <- split(plot_data, plot_data[["variable"]])

      variable <- NULL
      n <- NULL

      if(is.null(split_factor)) {

        stat_data <- map(stat_data,
                         ~summary(eda(.x$value),
                                  pub_styled = FALSE))

        n_data <- map_dbl(stat_data, ~.x[["n_complete"]][[1]])

        n_data <- data.frame(variable = names(n_data),
                             n = unname(n_data))

        stat_data <- map(stat_data,
                         ~column_to_rownames(.x[, c("statistic", "value")],
                                             "statistic"))
        stat_data <- map(stat_data,
                         ~as.data.frame(t(.x)))

        stat_data <- map2_dfr(stat_data, names(stat_data),
                              ~mutate(.x, variable = .y))

        stat_data <- left_join(stat_data,
                               n_data,
                               by = "variable")

      } else {

        stat_data <- map(stat_data,
                         ~split(.x, .x[[split_factor]]))

        stat_data <- map(stat_data,
                         map,
                         ~summary(eda(.x$value),
                                  pub_styled = FALSE))

        n_data <- map(stat_data, map_dbl, ~.x[["n_complete"]][[1]])

        n_data <- map(n_data,
                      ~tibble(!!split_factor := names(.x),
                                  n = .x))

        n_data <- map2_dfr(n_data, names(n_data),
                           ~mutate(.x, variable = .y))

        stat_data <- map(stat_data,
                         map,
                         ~column_to_rownames(.x[, c("statistic", "value")],
                                             "statistic"))

        stat_data <- map(stat_data,
                         map,
                         ~as.data.frame(t(.x)))

        stat_data <- map(stat_data,
                         ~map2_dfr(.x, names(.x),
                                   ~mutate(.x, !!split_factor := .y)))

        stat_data <- map2_dfr(stat_data, names(stat_data),
                              ~mutate(.x, variable = .y))

        stat_data <- left_join(stat_data,
                               n_data,
                               by = c(split_factor, "variable"))

        stat_data[[split_factor]] <-
          factor(stat_data[[split_factor]],
                 levels(data[[split_factor]]))

      }

      stat_data[["variable"]] <-
        factor(stat_data[["variable"]], variables)

      stat_data[["sem"]] <-
        stat_data[["sd"]]/sqrt(stat_data[["n"]])

    }

    ## violin, box, histogram, and density plots ---------

    if(type %in% c("violin", "box", "histogram", "density")) {

      ### defaults

      if(is.null(shape_alpha)) shape_alpha <- 0.25
      if(is.null(point_alpha)) point_alpha <- 0.75

      ### modification of the plotting data and axis labels

      plot_data <- add_labels(plot_data,
                              split_factor = split_factor,
                              n_labs = n_labs,
                              n_lab_sep = n_lab_sep,
                              labeller_fun = labeller_fun)

      ### bare plots, and distribution and point geoms

      if(is.null(split_factor)) {

        if(type %in% c("violin", "box")) {

          num_plot <- ggplot(plot_data,
                             aes(x = .data[["value"]],
                                 y = .data[["axis_label"]]))

        } else {

          num_plot <- ggplot(plot_data,
                             aes(x = .data[["value"]])) +
            facet_grid(axis_label ~ .,
                       scales = "free_y")

        }

        distr_geom <-
          switch(type,
                 box = geom_boxplot(outlier.color = NA,
                                    color = shape_color,
                                    fill = shape_fill,
                                    alpha = shape_alpha, ...),
                 violin = geom_violin(color = shape_color,
                                       fill = shape_fill,
                                       alpha = shape_alpha, ...),
                 histogram = geom_histogram(color = shape_color,
                                            fill = shape_fill,
                                            alpha = shape_alpha, ...),
                 density = geom_density(color = shape_color,
                                        fill = shape_fill,
                                        alpha = shape_alpha, ...))

        point_geom <-
          geom_point(shape = 21,
                     size = point_size,
                     color = "black",
                     fill = point_color,
                     alpha = point_alpha,
                     position = position_jitter(width = point_wjitter,
                                                height = point_hjitter))

      } else {

        if(type %in% c("violin", "box")) {

          num_plot <- ggplot(plot_data,
                             aes(x = .data[["value"]],
                                 y = .data[["axis_label"]],
                                 fill = .data[[split_factor]])) +
            facet_grid(facet_label ~ .,
                       scales = "free_y")

        } else {

          if(overlap_density) {

            facet_formula <- facet_label ~ .

          } else {

            facet_formula <- facet_label + axis_label ~ .

          }

          num_plot <- ggplot(plot_data,
                             aes(x = .data[["value"]],
                                 fill = .data[[split_factor]])) +
            facet_grid(facet_formula,
                       scales = "free_y")

        }

         num_plot <- num_plot +
          scale_fill_manual(values = palette) +
          labs(fill = fill_lab)

        distr_geom <-
          switch(type,
                 box = geom_boxplot(outlier.color = NA,
                                    color = shape_color,
                                    alpha = shape_alpha, ...),
                 violin = geom_violin(color = shape_color,
                                       alpha = shape_alpha, ...),
                 histogram = geom_histogram(color = shape_color,
                                            alpha = shape_alpha, ...),
                 density = geom_density(color = shape_color,
                                        alpha = shape_alpha, ...))

        point_geom <-
          geom_point(shape = 21,
                     size = point_size,
                     color = "black",
                     alpha = point_alpha,
                     position = position_jitter(width = point_wjitter,
                                                height = point_hjitter))

      }

      num_plot <- num_plot +
        distr_geom

      if(type %in% c("violin", "box")) num_plot <- num_plot + point_geom

      if(type == "violin" & show_stats) {

        if(is.null(line_color)) line_color <- "orangered3"

        stat_data <-
          stat_data[, c(split_factor, "variable",
                        "median", "perc_25", "perc_75")]

        meta_data <-
          select(plot_data,
                 any_of(c("variable", split_factor,
                          "facet_label", "axis_label")))

        if(is.null(split_factor)) {

          meta_data <- meta_data[!duplicated(meta_data[["variable"]]), ]

          stat_data <- left_join(stat_data,
                                 meta_data,
                                 by = "variable")

        } else {

          meta_data[["cond_id"]] <- interaction(meta_data[["variable"]],
                                                meta_data[[split_factor]])

          meta_data <- meta_data[!duplicated(meta_data[["cond_id"]]), ]

          meta_data[["cond_id"]] <- NULL

          stat_data <- left_join(stat_data,
                                 meta_data,
                                 by = c(split_factor, "variable"))

        }

        num_plot <- num_plot +
          geom_point(data = stat_data,
                     aes(x = .data[["median"]]),
                     shape = 23,
                     fill = line_color,
                     color = line_color,
                     size = point_size + 1) +
          geom_errorbarh(data = stat_data,
                         aes(x = .data[["median"]],
                             xmin = .data[["perc_25"]],
                             xmax = .data[["perc_75"]]),
                         width = 0,
                         color = line_color,
                         linewidth = line_width)

      }


    }

    ## bar and Forest plots of centrality/distribution statistics -------

    if(type %in% c("bar", "forest")) {

      ## defaults

      if(is.null(line_color)) line_color <- shape_fill
      if(is.null(shape_alpha)) {

        if(type == "forest") {

          shape_alpha <- 1

        } else {

          shape_alpha <- 0.35

        }

      }

      ## modification of the plotting data

      stat_data <- add_labels(stat_data,
                              split_factor = split_factor,
                              n_labs = n_labs,
                              n_lab_sep = n_lab_sep,
                              labeller_fun = labeller_fun)

      ## bare plot layouts, centrality and distribution geoms

      if(is.null(split_factor)) {

        num_plot <- ggplot(stat_data,
                           aes(x = .data[[central_stat]],
                               y = .data[["axis_label"]]))

        center_geom <-
          switch(type,
                 bar = geom_bar(stat = "identity",
                                color = shape_color,
                                fill = shape_fill,
                                alpha = shape_alpha, ...),
                 forest = geom_point(shape = 21,
                                     size = point_size,
                                     color = shape_fill,
                                     fill = shape_fill,
                                     alpha = shape_alpha, ...))

        if(type == "bar") {

          distr_color <- shape_color

        } else {

          distr_color <- line_color

        }

        distr_geom <-
          switch(distribution_stat,
                 sem = geom_errorbarh(aes(xmin = .data[[central_stat]] -
                                            .data[["sem"]],
                                          xmax = .data[[central_stat]] +
                                            .data[["sem"]]),
                                      color = distr_color,
                                      linewidth = line_width,
                                      alpha = line_alpha,
                                      width = 0),
                 sd = geom_errorbarh(aes(xmin = .data[[central_stat]] -
                                           .data[["sd"]],
                                         xmax = .data[[central_stat]] +
                                           .data[["sd"]]),
                                     color = distr_color,
                                     linewidth = line_width,
                                     alpha = line_alpha,
                                     width = 0),
                 iqr = geom_errorbarh(aes(xmin = .data[[central_stat]] -
                                            .data[["perc_25"]],
                                          xmax = .data[[central_stat]] +
                                            .data[["perc_75"]]),
                                      color = distr_color,
                                      linewidth = line_width,
                                      alpha = line_alpha,
                                      width = 0),
                 `95%` = geom_errorbarh(aes(xmin = .data[[central_stat]] -
                                              .data[["perc_025"]],
                                            xmax = .data[[central_stat]] +
                                              .data[["perc_975"]]),
                                        color = distr_color,
                                        linewidth = line_width,
                                        alpha = line_alpha,
                                        width = 0),
                 `2sem` = geom_errorbarh(aes(xmin = .data[[central_stat]] -
                                               qnorm(0.975) * .data[["sem"]],
                                             xmax = .data[[central_stat]] +
                                               qnorm(0.975) * .data[["sem"]]),
                                         color = distr_color,
                                         linewidth = line_width,
                                         alpha = line_alpha,
                                         width = 0),
                 `2sd` = geom_errorbarh(aes(xmin = .data[[central_stat]] -
                                              qnorm(0.975) * .data[["sd"]],
                                            xmax = .data[[central_stat]] +
                                              qnorm(0.975) * .data[["sd"]]),
                                        color = distr_color,
                                        linewidth = line_width,
                                        alpha = line_alpha,
                                        width = 0))

      } else {

        num_plot <- ggplot(stat_data,
                           aes(x = .data[[central_stat]],
                               y = .data[["axis_label"]],
                               fill = .data[[split_factor]],
                               color = .data[[split_factor]])) +
          facet_grid(facet_label ~ .,
                     scales = "free_y") +
          scale_fill_manual(values = palette) +
          scale_color_manual(values = palette) +
          labs(fill = fill_lab,
               color = fill_lab)

        center_geom <-
          switch(type,
                 bar = geom_bar(stat = "identity",
                                color = shape_color,
                                alpha = shape_alpha, ...),
                 forest = geom_point(shape = 21,
                                     size = point_size,
                                     alpha = shape_alpha, ...))

        distr_geom <-
          switch(distribution_stat,
                 sem = geom_errorbarh(aes(xmin = .data[[central_stat]] -
                                            .data[["sem"]],
                                          xmax = .data[[central_stat]] +
                                            .data[["sem"]]),
                                      linewidth = line_width,
                                      alpha = line_alpha,
                                      width = 0),
                 sd = geom_errorbarh(aes(xmin = .data[[central_stat]] -
                                           .data[["sd"]],
                                         xmax = .data[[central_stat]] +
                                           .data[["sd"]]),
                                     linewidth = line_width,
                                     alpha = line_alpha,
                                     width = 0),
                 iqr = geom_errorbarh(aes(xmin = .data[[central_stat]] -
                                            .data[["perc_25"]],
                                          xmax = .data[[central_stat]] +
                                            .data[["perc_75"]]),
                                      linewidth = line_width,
                                      alpha = line_alpha,
                                      width = 0),
                 `95%` = geom_errorbarh(aes(xmin = .data[[central_stat]] -
                                              .data[["perc_025"]],
                                            xmax = .data[[central_stat]] +
                                              .data[["perc_975"]]),
                                        linewidth = line_width,
                                        alpha = line_alpha,
                                        width = 0),
                 `2sem` = geom_errorbarh(aes(xmin = .data[[central_stat]] -
                                               qnorm(0.975) * .data[["sem"]],
                                             xmax = .data[[central_stat]] +
                                               qnorm(0.975) * .data[["sem"]]),
                                         linewidth = line_width,
                                         alpha = line_alpha,
                                         width = 0),
                 `2sd` = geom_errorbarh(aes(xmin = .data[[central_stat]] -
                                              qnorm(0.975) * .data[["sd"]],
                                            xmax = .data[[central_stat]] +
                                              qnorm(0.975) * .data[["sd"]]),
                                        linewidth = line_width,
                                        alpha = line_alpha,
                                        width = 0))

      }

      if(distribution_stat == "none") {

        num_plot <- num_plot + center_geom

      } else if(type == "bar") {

        num_plot <- num_plot + center_geom + distr_geom

      } else {

        num_plot <- num_plot + distr_geom + center_geom

      }

    }

    ## ribbon plots of centrality and distribution statistics ----------

    if(type == "ribbon") {

      ## defaults

      if(is.null(line_color)) line_color <- shape_fill
      if(is.null(line_alpha)) line_alpha <- 1
      if(is.null(shape_alpha)) shape_alpha <- 0.35

      ## modification of the plotting data

      stat_data <- add_labels(stat_data,
                              split_factor = split_factor,
                              labeller_fun = labeller_fun)

      if(is.null(split_factor)) {

        stat_data[["plot_order"]] <- as.numeric(stat_data[["axis_label"]])

        y_scale_breaks <- unique(stat_data[["plot_order"]])
        y_scale_labels <- as.character(stat_data[["axis_label"]])

        num_plot <- ggplot(stat_data,
                           aes(x = .data[[central_stat]],
                               y = .data[["plot_order"]])) +
          scale_y_continuous(breaks = y_scale_breaks,
                             labels = y_scale_labels)

        center_geom <-
          geom_path(color = line_color,
                    alpha = line_alpha,
                    linewidth = line_width, ...)

        distr_geom <-
          switch(distribution_stat,
                 sem = geom_ribbon(aes(xmin = .data[[central_stat]] -
                                         .data[["sem"]],
                                       xmax = .data[[central_stat]] +
                                         .data[["sem"]]),
                                   fill = shape_fill,
                                   linewidth = ribbon_line_width,
                                   alpha = shape_alpha),
                 sd = geom_ribbon(aes(xmin = .data[[central_stat]] -
                                        .data[["sd"]],
                                      xmax = .data[[central_stat]] +
                                        .data[["sd"]]),
                                  fill = shape_fill,
                                  linewidth = ribbon_line_width,
                                  alpha = shape_alpha),
                 iqr = geom_ribbon(aes(xmin = .data[[central_stat]] -
                                         .data[["perc_25"]],
                                       xmax = .data[[central_stat]] +
                                         .data[["perc_75"]]),
                                   fill = shape_fill,
                                   linewidth = ribbon_line_width,
                                   alpha = shape_alpha),
                 `95%` = geom_ribbon(aes(xmin = .data[[central_stat]] -
                                           .data[["perc_025"]],
                                         xmax = .data[[central_stat]] +
                                           .data[["perc_975"]]),
                                     fill = shape_fill,
                                     linewidth = ribbon_line_width,
                                     alpha = shape_alpha),
                 `2sem` = geom_ribbon(aes(xmin = .data[[central_stat]] -
                                            qnorm(0.975) * .data[["sem"]],
                                          xmax = .data[[central_stat]] +
                                            qnorm(0.975) * .data[["sem"]]),
                                      fill = shape_fill,
                                      linewidth = ribbon_line_width,
                                      alpha = shape_alpha),
                 `2sd` = geom_ribbon(aes(xmin = .data[[central_stat]] -
                                           qnorm(0.975) * .data[["sd"]],
                                         xmax = .data[[central_stat]] +
                                           qnorm(0.975) * .data[["sd"]]),
                                     fill = shape_fill,
                                     linewidth = ribbon_line_width,
                                     alpha = shape_alpha))

      } else {

        stat_data[["plot_order"]] <- as.numeric(stat_data[["facet_label"]])

        scale_data <- group_by(stat_data,
                               .data[["plot_order"]],
                               .data[["facet_label"]])

        scale_data <- summarise(scale_data, n = sum(n))

        scale_y_breaks <- scale_data[["plot_order"]]
        scale_y_labels <- scale_data[["facet_label"]]

        if(n_labs) {

          scale_y_labels <- paste(scale_y_labels,
                                  scale_data[["n"]],
                                  sep = paste0(n_lab_sep, "n = "))

        }

        num_plot <- ggplot(stat_data,
                           aes(x = .data[[central_stat]],
                               y = .data[["plot_order"]],
                               fill = .data[[split_factor]],
                               color = .data[[split_factor]])) +
          scale_y_continuous(breaks = scale_y_breaks,
                             labels = scale_y_labels) +
          scale_fill_manual(values = palette) +
          scale_color_manual(values = palette) +
          labs(fill = fill_lab,
               color = fill_lab)

        center_geom <-
          geom_path(alpha = line_alpha,
                    linewidth = line_width, ...)

        distr_geom <-
          switch(distribution_stat,
                 sem = geom_ribbon(aes(xmin = .data[[central_stat]] -
                                         .data[["sem"]],
                                       xmax = .data[[central_stat]] +
                                         .data[["sem"]]),
                                   linewidth = ribbon_line_width,
                                   alpha = shape_alpha),
                 sd = geom_ribbon(aes(xmin = .data[[central_stat]] -
                                        .data[["sd"]],
                                      xmax = .data[[central_stat]] +
                                        .data[["sd"]]),
                                  linewidth = ribbon_line_width,
                                  alpha = shape_alpha),
                 iqr = geom_ribbon(aes(xmin = .data[[central_stat]] -
                                         .data[["perc_25"]],
                                       xmax = .data[[central_stat]] +
                                         .data[["perc_75"]]),
                                   linewidth = ribbon_line_width,
                                   alpha = shape_alpha),
                 `95%` = geom_ribbon(aes(xmin = .data[[central_stat]] -
                                           .data[["perc_025"]],
                                         xmax = .data[[central_stat]] +
                                           .data[["perc_975"]]),
                                     linewidth = ribbon_line_width,
                                     alpha = shape_alpha),
                 `2sem` = geom_ribbon(aes(xmin = .data[[central_stat]] -
                                            qnorm(0.975) * .data[["sem"]],
                                          xmax = .data[[central_stat]] +
                                            qnorm(0.975) * .data[["sem"]]),
                                      linewidth = ribbon_line_width,
                                      alpha = shape_alpha),
                 `2sd` = geom_ribbon(aes(xmin = .data[[central_stat]] -
                                           qnorm(0.975) * .data[["sd"]],
                                         xmax = .data[[central_stat]] +
                                           qnorm(0.975) * .data[["sd"]]),
                                     linewidth = ribbon_line_width,
                                     alpha = shape_alpha))

      }

      if(distribution_stat == "none") {

        num_plot <- num_plot + center_geom

      } else {

        num_plot <- num_plot + distr_geom + center_geom

      }

    }

    ## final edits and output ------------

    if(!is.null(cust_theme)) num_plot <- num_plot + cust_theme

    num_plot +
      labs(title = plot_title,
           subtitle = plot_subtitle,
           x = x_lab,
           y = y_lab)

  }

#' @rdname draw_numeric_panel

  draw_factor_panel <- function(data,
                                variables,
                                split_factor = NULL,
                                palette = tableau10_colors(),
                                shape_color = "black",
                                shape_alpha = NULL,
                                show_txt = TRUE,
                                txt_size = 2.75,
                                txt_vjust = NULL,
                                txt_hjust = NULL,
                                cust_theme = eda_classic_theme(),
                                plot_title = NULL,
                                plot_subtitle = NULL,
                                x_lab = NULL,
                                y_lab = NULL,
                                fill_lab = NULL,
                                n_labs = TRUE,
                                n_lab_sep = "\n",
                                labeller_fun = identity,
                                ...) {

    ## input control ---------

    stopifnot(is.logical(show_txt))
    show_txt <- show_txt[1]

    if(!is.null(cust_theme)) {

      if(!is_theme(cust_theme)) {

        stop("`cust_theme` has to be a valid `ggplot` theme.", call. = FALSE)

      }

    }

    stopifnot(is.logical(n_labs))
    n_labs <- n_labs[1]

    if(!is_function(labeller_fun)) {

      stop("`labeller_fun` has to be a function.", call. = FALSE)

    }

    ## plotting data ------------

    data <- validate_multi_df(data,
                              variables,
                              split_factor,
                              format = "factor")

    if(is.null(split_factor)) {

      variables <- names(data)

    } else {

      variables <- names(data)[names(data) != split_factor]

    }

    return(data)

  }



