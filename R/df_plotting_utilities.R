# Plotting utilities for data frames

#' Plotting utilities for factor and numeric types.
#'
#' @description
#' Internal functions which generate `ggplot` plots for data frame's variables
#' of different data types.
#' For factors, stack, bar, and bubble plots are available with counts or
#' percentages of observations in categories of a splitting factor.
#' For numeric variables, violin and box plots, histograms, density plots,
#' and quantile - quantile (QQ) plots are implemented.
#'
#' @details
#' If `type = "paired"`, a before-after plot is generated and its assumed that
#' the plotting data are already arranged by the blocking or identified variable.
#'
#' @return a `ggplot` object.
#'
#' @inheritParams plot_factor
#' @param data a data frame.
#' @param variable name of the data frame's variable whose distribution will be
#' shown in the plot. Provided as a string.
#' @param split_factor name of the data frame's variable, whose categories will
#' be used to split values of `variable` in the plot. Provided as a string.
#' @param dodge_width spacing between bars in bar plots.
#' @param line_color color of interquartile whiskers in violin plots, and
#' lines depicting statistics in histograms.
#' @param line_width width of interquartile whiskers in violin plots, and
#' lines depicting statistics in histograms.
#' @param y_lab title of the Y axis, if `NULL` the title is set with a simple
#' heuristics (e.g. if `scale = "none"`, `"observations, N`).
#' @param fill_lab title of the fill scale.
#' @param x_n_labs logical. If `TRUE`, N numbers of complete observations in
#' strata of `variable` defined by the `split_factor`'s categories are displayed
#' in the X axis labels. Defaults to `TRUE`.
#' @param labeller_fun a function used to transform names of `split_factor`'s
#' categories presented in the plot axes. Defaults to `identity`, which means
#' that the category names are displayed as they are specified by levels of the
#' splitting factor.
#' @param facet "none": histograms/density plots are overlaid,
#' "horizontal": horizontal or "vertical": vertical faceting.
#' @param facet_scales specifies scales in the faceted histogram/density plot,
#' see: \code{\link[ggplot2]{facet_grid}} for details.
#' @param facet_space specifies space in the faceted histogram/density plot,
#' see: \code{\link[ggplot2]{facet_grid}} for details.
#' @param ... additional arguments passed to \code{\link[ggplot2]{geom_boxplot}},
#' \code{\link[ggplot2]{geom_violin}}, \code{\link[ggplot2]{geom_histogram}}.

  plot_df_factor <- function(data,
                             variable,
                             split_factor,
                             .drop = TRUE,
                             type = c("stack", "bar", "bubble"),
                             scale = c("none", "percent"),
                             signif_digits = 2,
                             shape_color = "black",
                             shape_alpha = 1,
                             dodge_width = 0.9,
                             cust_theme = theme_classic(),
                             show_txt = TRUE,
                             txt_size = 2.75,
                             txt_vjust = NULL,
                             txt_hjust = NULL,
                             plot_title = NULL,
                             plot_subtitle = NULL,
                             x_lab = NULL,
                             y_lab = NULL,
                             fill_lab = NULL,
                             x_n_labs = TRUE,
                             labeller_fun = identity, ...) {

    ## input control ---------

    type <- match.arg(type[1], c("stack", "bar", "bubble"))

    scale <- match.arg(scale[1], c("none", "percent"))

    stopifnot(is.numeric(signif_digits))
    signif_digits <- as.integer(signif_digits[1])

    stopifnot(is.numeric(shape_alpha))
    shape_alpha <- shape_alpha[1]

    stopifnot(is.numeric(txt_size))

    if(!is_theme(cust_theme)) {

      stop("`cust_theme` has to be a valid `ggplot` theme.", call. = FALSE)

    }

    stopifnot(is.logical(show_txt))
    show_txt <- show_txt[1]

    stopifnot(is.logical(x_n_labs))
    x_n_labs <- x_n_labs[1]

    if(!is_function(labeller_fun)) {

      stop("`labeller_fun` has to be a function.", call. = FALSE)

    }

    ## plotting data and meta-data ------

    ### validation of the data frame format and pre-processing

    data <- validate_df(data, variable, split_factor, .drop)

    if(!is.factor(data[[variable]])) {

      stop("`variable` has to specify a factor variable in the data frame.",
           call. = FALSE)

    }

    n_numbers <- attr(data, "n_numbers")

    if(is.null(plot_subtitle)) {

      plot_subtitle <- paste0("total: n = ", n_numbers[["total"]],
                              ", complete: n = ", n_numbers[["complete"]])

    }

    ### frequency variable to be presented in the plot, Y axis label

    plot_var <- switch(scale,
                       none = "n",
                       percent = "percent_complete")

    if(is.null(y_lab) & type %in% c("bar", "stack")) {

      y_lab <- switch(scale,
                      none = "observations, N",
                      percent = "% of complete observations")

    }

    ### frequency data to be displayed in the plot

    plot_data <- split(eda(data[[variable]], .drop = FALSE),
                       data[[split_factor]])

    plot_data <- map(plot_data, frequency, .drop = .drop)

    plot_data <-
      map2(plot_data, names(plot_data),
           ~mutate(.x,
                   !!split_factor := factor(.y,
                                            levels(data[[split_factor]]))))

    ### text labels with frequency and their position in the plot

    if(type == "stack") {

      plot_pos <- NULL

      plot_data <- map(plot_data, arrange, desc(.data[["category"]]))

      plot_data <- map(plot_data,
                       mutate,
                       plot_pos = cumsum(.data[[plot_var]]) - 0.5 * .data[[plot_var]])

    }

    plot_data <- reduce(plot_data, rbind)

    if(scale == "none") {

      plot_data[["plot_label"]] <- plot_data[["n"]]

    } else {

      plot_data[["plot_label"]] <-
        paste0(signif(plot_data[["percent_complete"]], signif_digits),
               "%")

    }

    ### numbers of complete observations in the splitting factor categories

    plot_data[["axis_label"]] <- labeller_fun(plot_data[[split_factor]])

    if(x_n_labs) {

      plot_data[["axis_label"]] <-
        paste(plot_data[["axis_label"]],
              plot_data[["n_complete"]],
              sep = "\nn = ")

    }

    scale_labels <- filter(plot_data, !duplicated(.data[[split_factor]]))

    scale_labels <- set_names(plot_data[["axis_label"]],
                              as.character(plot_data[[split_factor]]))

    ## plots ---------

    if(type == "bar") {

      if(is.null(x_lab)) x_lab <- variable
      if(is.null(fill_lab)) fill_lab <- split_factor

      fct_plot <- ggplot(plot_data,
                         aes(x = .data[["category"]],
                             y = .data[[plot_var]],
                             fill = .data[[split_factor]])) +
        geom_bar(stat = "identity",
                 position = position_dodge(width = dodge_width),
                 color = shape_color,
                 alpha = shape_alpha) +
        scale_fill_brewer(labels = scale_labels) +
        labs(fill = fill_lab,
             x = x_lab,
             y = y_lab)

      if(show_txt) {

        if(is.null(txt_hjust)) txt_hjust <- 0.5
        if(is.null(txt_vjust)) txt_vjust <- -0.4

        fct_plot <- fct_plot +
          geom_text(aes(label = .data[["plot_label"]]),
                    position = position_dodge(width = dodge_width),
                    hjust = txt_hjust,
                    vjust = txt_vjust,
                    size = txt_size)

      }

    } else if(type == "stack") {

      if(is.null(x_lab)) x_lab <- split_factor
      if(is.null(fill_lab)) fill_lab <- variable

      fct_plot <- ggplot(plot_data,
                         aes(x = .data[[split_factor]],
                             y = .data[[plot_var]],
                             fill = .data[["category"]])) +
        geom_bar(stat = "identity",
                 color = shape_color,
                 alpha = shape_alpha) +
        scale_x_discrete(labels = scale_labels) +
        labs(y = y_lab,
             x = x_lab,
             fill = fill_lab)

      if(show_txt) {

        fct_plot <- fct_plot +
          geom_label(aes(label = .data[["plot_label"]],
                         y = .data[["plot_pos"]]),
                     color = shape_color,
                     size = txt_size,
                     show.legend = FALSE)

      }

    } else {

      if(is.null(x_lab)) x_lab <- split_factor
      if(is.null(y_lab)) y_lab <- variable

      if(is.null(fill_lab)) {

        fill_lab <-
          switch(scale,
                 none = "observations, N",
                 percent = "% of complete observations")

      }

      fct_plot <- ggplot(plot_data,
                         aes(x = .data[[split_factor]],
                             y = .data[["category"]],
                             fill = .data[[plot_var]],
                             size = .data[[plot_var]])) +
        geom_point(shape = 21,
                   color = shape_color) +
        scale_x_discrete(labels = scale_labels) +
        scale_fill_gradient() +
        scale_size_area() +
        guides(size = guide_legend(),
               fill = guide_legend()) +
        labs(x = x_lab,
             size = fill_lab,
             fill = fill_lab)

      if(show_txt) {

        if(is.null(txt_hjust)) txt_hjust <- 0.5
        if(is.null(txt_vjust)) txt_vjust <- -1.2

        fct_plot <- fct_plot +
          geom_text(aes(label = .data[["plot_label"]]),
                    hjust = txt_hjust,
                    vjust = txt_vjust,
                    size = txt_size)

      }

    }

    fct_plot +
      cust_theme +
      labs(title = plot_title,
           subtitle = plot_subtitle)

  }

#' @rdname plot_df_factor

  plot_df_numeric <- function(data,
                              variable,
                              split_factor,
                              type = c("violin", "box", "paired"),
                              show_stats = TRUE,
                              shape_color = "black",
                              shape_alpha = 0.3,
                              point_color = 'gray40',
                              point_size = 2,
                              point_alpha = 0.75,
                              point_hjitter = 0,
                              point_wjitter = 0.1,
                              line_color = NULL,
                              line_width = 0.75,
                              cust_theme = theme_classic(),
                              plot_title = NULL,
                              plot_subtitle = NULL,
                              x_lab = NULL,
                              y_lab = NULL,
                              fill_lab = NULL,
                              x_n_labs = TRUE,
                              labeller_fun = identity, ...) {

    ## input control ---------

    type <- match.arg(type[1], c("violin", "box", "paired"))

    stopifnot(is.logical(show_stats))
    show_stats <- show_stats[1]

    stopifnot(is.numeric(shape_alpha))
    shape_alpha <- shape_alpha[1]

    stopifnot(is.numeric(point_size))
    point_size <- point_size[1]

    stopifnot(is.numeric(point_alpha))
    point_alpha <- point_alpha[1]

    stopifnot(is.numeric(point_wjitter))
    point_wjitter <- point_wjitter[1]

    stopifnot(is.numeric(point_hjitter))
    point_hjitter <- point_hjitter[1]

    stopifnot(is.numeric(line_width))
    line_width <- line_width[1]

    if(!is_theme(cust_theme)) {

      stop("`cust_theme` has to be a valid `ggplot` theme.", call. = FALSE)

    }

    if(!is_theme(cust_theme)) {

      stop("`cust_theme` has to be a valid `ggplot` theme.", call. = FALSE)

    }

    stopifnot(is.logical(x_n_labs))
    x_n_labs <- x_n_labs[1]

    if(!is_function(labeller_fun)) {

      stop("`labeller_fun` has to be a function.", call. = FALSE)

    }

    ## plotting data -------

    ### variables of interest, plot subtitles

    data <- validate_df(data, variable, split_factor)

    if(!is.numeric(data[[variable]])) {

      stop("`variable` has to specify a numeric variable in the data frame.",
           call. = FALSE)

    }

    n_numbers <- attr(data, "n_numbers")

    if(is.null(plot_subtitle)) {

      plot_subtitle <- paste0("total: n = ", n_numbers[["total"]],
                              ", complete: n = ", n_numbers[["complete"]])

    }

    ### medians with interquartile ranges

    if(type == "violin" & show_stats) {

      stat_data <-
        map(split(eda(data[[variable]]),
                  data[[split_factor]]),
            summary, pub_styled = FALSE)

      cat_names <- names(stat_data)

      stat_data <- map(stat_data,
                      column_to_rownames, "statistic")

      stat_data <-
        map_dfr(stat_data,
                ~as.data.frame(t(.x[c("median", "perc_25", "perc_75"),
                                    "value",
                                    drop = FALSE])))

      stat_data[[split_factor]] <-
        factor(cat_names, cat_names)

    }

    ### numbers of observations in categories of the splitting factor

    if(!x_n_labs) {

      scale_labels <-
        set_names(labeller_fun(levels(data[[split_factor]])),
                  levels(data[[split_factor]]))

    } else {

      n_categories <- table(data[[split_factor]])

      scale_labels <-
        map2_chr(names(n_categories),
                 n_categories,
                 ~paste(labeller_fun(.x), .y, sep = "\nn = "))

      scale_labels <- set_names(scale_labels, names(n_categories))

    }

    ### axis and fill scale titles

    if(is.null(x_lab)) x_lab <- split_factor
    if(is.null(fill_lab)) fill_lab <- split_factor

    if(is.null(y_lab)) y_lab <- paste(variable, "value")

    ## plotting data for paired/before - after plots

    if(type == "paired") {

      .block_id <- NULL

      data <- split(data, data[[split_factor]])

      ## checking if all parts are of equal lengths

      pair_check <- map_dbl(data, nrow)

      if(any(pair_check != pair_check[1])) {

        stop(paste("Categories defined by `split_factor` are of different",
                   "sizes. Are you sure the observations are matched?"),
             call. = FALSE)

      }

      data <-
        map_dfr(data,
                ~mutate(.x, .block_id = 1:nrow(.x)))

    }

    ## plots --------

    if(type %in% c("box", "violin")) {

      num_plot <-
        ggplot(data,
               aes(x = .data[[split_factor]],
                   y = .data[[variable]],
                   fill = .data[[split_factor]]))

      if(type == "box") {

        num_plot <- num_plot +
          geom_boxplot(outlier.color = NA,
                       color = shape_color,
                       alpha = shape_alpha, ...)

      } else {

        if(is.null(line_color)) line_color <- "orangered3"

        num_plot <- num_plot +
          geom_violin(color = shape_color,
                      alpha = shape_alpha, ...)

        if(show_stats) {

          num_plot <- num_plot +
            geom_point(data = stat_data,
                       aes(y = .data[["median"]]),
                       shape = 23,
                       fill = line_color,
                       color = line_color,
                       size = point_size + 1) +
            geom_errorbar(data = stat_data,
                          aes(y = .data[["median"]],
                              ymin = .data[["perc_25"]],
                              ymax = .data[["perc_75"]]),
                          color = line_color,
                          linewidth = line_width,
                          width = 0)

        }

      }

      num_plot <- num_plot +
        geom_point(position = position_jitter(width = point_wjitter,
                                              height = point_hjitter),
                   shape = 21,
                   size = point_size,
                   alpha = point_alpha)

    } else {

      if(is.null(line_color)) line_color <- "gray60"

      num_plot <- ggplot(data,
                         aes(x = .data[[split_factor]],
                             y = .data[[variable]],
                             fill = .data[[split_factor]])) +
        geom_line(aes(group = .data[[".block_id"]]),
                  linewidth = line_width,
                  color = line_color) +
        geom_point(shape = 21,
                   size = point_size,
                   alpha = point_alpha)

    }

    num_plot +
      scale_x_discrete(labels = scale_labels) +
      cust_theme +
      labs(title = plot_title,
           subtitle = plot_subtitle,
           y = y_lab,
           x = x_lab)

  }

#' @rdname plot_df_factor

  plot_df_histogram <- function(data,
                                variable,
                                split_factor,
                                type = c("histogram", "density"),
                                show_stats = TRUE,
                                shape_color = "black",
                                shape_alpha = 0.3,
                                line_color = "orangered3",
                                line_width = 0.75,
                                cust_theme = theme_classic(),
                                plot_title = NULL,
                                plot_subtitle = NULL,
                                x_lab = NULL,
                                y_lab = NULL,
                                fill_lab = NULL,
                                x_n_labs = TRUE,
                                labeller_fun = identity,
                                facet = c("none",
                                          "horizontal",
                                          "vertical"),
                                facet_scales = "free",
                                facet_space = "free", ...) {

    ## input control ---------

    type <- match.arg(type[1], c("histogram", "density"))

    stopifnot(is.logical(show_stats))
    show_stats <- show_stats[1]

    stopifnot(is.numeric(shape_alpha))
    shape_alpha <- shape_alpha[1]

    stopifnot(is.numeric(line_width))
    line_width <- line_width[1]

    if(!is_theme(cust_theme)) {

      stop("`cust_theme` has to be a valid `ggplot` theme.", call. = FALSE)

    }

    if(!is_theme(cust_theme)) {

      stop("`cust_theme` has to be a valid `ggplot` theme.", call. = FALSE)

    }

    stopifnot(is.logical(x_n_labs))
    x_n_labs <- x_n_labs[1]

    if(!is_function(labeller_fun)) {

      stop("`labeller_fun` has to be a function.", call. = FALSE)

    }

    facet <- match.arg(facet[1], c("none", "horizontal", "vertical"))

    ## plotting data ----------

    ### variables of interest, plot subtitles

    data <- validate_df(data, variable, split_factor)

    if(!is.numeric(data[[variable]])) {

      stop("`variable` has to specify a numeric variable in the data frame.",
           call. = FALSE)

    }

    n_numbers <- attr(data, "n_numbers")

    if(is.null(plot_subtitle)) {

      plot_subtitle <- paste0("total: n = ", n_numbers[["total"]],
                              ", complete: n = ", n_numbers[["complete"]])

    }

    ### medians with interquartile ranges

    if(show_stats) {

      stat_data <-
        map(split(eda(data[[variable]]),
                  data[[split_factor]]),
            summary, pub_styled = FALSE)

      cat_names <- names(stat_data)

      stat_data <- map(stat_data,
                       column_to_rownames, "statistic")

      stat_data <-
        map_dfr(stat_data,
                ~as.data.frame(t(.x[c("median", "perc_25", "perc_75"),
                                    "value",
                                    drop = FALSE])))

      stat_data[[split_factor]] <-
        factor(cat_names, cat_names)

    }

    ### numbers of observations in categories of the splitting factor

    if(!x_n_labs) {

      scale_labels <-
        set_names(labeller_fun(levels(data[[split_factor]])),
                  levels(data[[split_factor]]))

    } else {

      n_categories <- table(data[[split_factor]])

      scale_labels <-
        map2_chr(names(n_categories),
                 n_categories,
                 ~paste(labeller_fun(.x), .y, sep = "\nn = "))

      scale_labels <- set_names(scale_labels, names(n_categories))

    }

    ### axis and fill scale labels

    if(is.null(fill_lab)) fill_lab <- split_factor

    if(is.null(x_lab)) x_lab <- variable

    ## plots --------

    hist_plot <- ggplot(data,
                        aes(x = .data[[variable]],
                            fill = .data[[split_factor]]))

    if(type == "histogram") {

      if(is.null(y_lab)) y_lab <- "observations, N"

      hist_plot <- hist_plot  +
        geom_histogram(color = shape_color,
                       alpha = shape_alpha, ...)

    } else {

      if(is.null(y_lab)) y_lab <- "observation density"

      hist_plot <- hist_plot  +
        geom_density(color = shape_color,
                     alpha = shape_alpha, ...)

    }

    if(facet == "none") {

      hist_plot <- hist_plot +
        scale_fill_brewer(labels = scale_labels)

    } else {

      if(facet == "horizontal") {

        split_formula <- paste(". ~", split_factor)

      } else {

        split_formula <- paste(split_factor, "~ .")

      }

      hist_plot <- hist_plot +
        facet_grid(as.formula(split_formula),
                   scales = facet_scales,
                   space = facet_space,
                   labeller = as_labeller(scale_labels))

    }

    if(show_stats) {

      hist_plot <- hist_plot +
        geom_vline(data = stat_data,
                   aes(xintercept = .data[["median"]]),
                   color = line_color,
                   linewidth = line_width) +
        geom_vline(data = stat_data,
                   aes(xintercept = .data[["perc_25"]]),
                   color = line_color,
                   linewidth = line_width,
                   linetype = "dashed") +
        geom_vline(data = stat_data,
                   aes(xintercept = .data[["perc_75"]]),
                   color = line_color,
                   linewidth = line_width,
                   linetype = "dashed")

    }

    hist_plot +
      cust_theme +
      labs(title = plot_title,
           subtitle = plot_subtitle,
           x = x_lab,
           y = y_lab,
           fill = fill_lab)


  }

#' @rdname plot_df_factor

  plot_df_qq <- function(data,
                         variable,
                         split_factor,
                         point_size = 2,
                         point_alpha = 0.75,
                         point_hjitter = 0,
                         point_wjitter = 0,
                         line_width = 0.75,
                         cust_theme = theme_classic(),
                         plot_title = NULL,
                         plot_subtitle = NULL,
                         x_lab = "quantiles, theoretical normal distribution",
                         y_lab = "quantiles, observed distribution",
                         fill_lab = NULL,
                         x_n_labs = TRUE,
                         labeller_fun = identity,
                         facet = c("none",
                                   "horizontal",
                                   "vertical"),
                         facet_scales = "free",
                         facet_space = "free", ...) {

    ## input control ---------

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

    if(!is_theme(cust_theme)) {

      stop("`cust_theme` has to be a valid `ggplot` theme.", call. = FALSE)

    }

    stopifnot(is.logical(x_n_labs))
    x_n_labs <- x_n_labs[1]

    if(!is_function(labeller_fun)) {

      stop("`labeller_fun` has to be a function.", call. = FALSE)

    }

    facet <- match.arg(facet[1], c("none", "horizontal", "vertical"))

    ## plotting data ----------

    ### variables of interest, plot subtitles

    data <- validate_df(data, variable, split_factor)

    if(!is.numeric(data[[variable]])) {

      stop("`variable` has to specify a numeric variable in the data frame.",
           call. = FALSE)

    }

    n_numbers <- attr(data, "n_numbers")

    if(is.null(plot_subtitle)) {

      plot_subtitle <- paste0("total: n = ", n_numbers[["total"]],
                              ", complete: n = ", n_numbers[["complete"]])

    }

    ### numbers of observations in categories of the splitting factor

    if(!x_n_labs) {

      scale_labels <-
        set_names(labeller_fun(levels(data[[split_factor]])),
                  levels(data[[split_factor]]))

    } else {

      n_categories <- table(data[[split_factor]])

      scale_labels <-
        map2_chr(names(n_categories),
                 n_categories,
                 ~paste(labeller_fun(.x), .y, sep = "\nn = "))

      scale_labels <- set_names(scale_labels, names(n_categories))

    }

    ### axis and fill scale labels

    if(is.null(fill_lab)) fill_lab <- split_factor

    ## plots -----------

    qq_plot <-  ggplot(data,
                       aes(sample = .data[[variable]],
                           fill = .data[[split_factor]],
                           color = .data[[split_factor]])) +
      geom_qq(shape = 21,
              color = "black",
              size = point_size,
              alpha = point_alpha,
              position = position_jitter(width = point_wjitter,
                                         height = point_hjitter)) +
      geom_qq_line(linewidth = line_width) +
      cust_theme +
      labs(title = plot_title,
           subtitle = plot_subtitle,
           x = x_lab,
           y = y_lab)

    if(facet == "none") {

      qq_plot <- qq_plot +
        scale_fill_brewer(labels = scale_labels) +
        scale_color_brewer(labels = scale_labels)

    } else {

      if(facet == "horizontal") {

        split_formula <- paste(". ~", split_factor)

      } else {

        split_formula <- paste(split_factor, "~ .")

      }

      qq_plot <- qq_plot +
        facet_grid(as.formula(split_formula),
                   scales = facet_scales,
                   space = facet_space,
                   labeller = as_labeller(scale_labels))

    }

    qq_plot

  }

# END ---------
