# Plotting method for the `etest` class object.

#' Plotting of effect size and significance for `etest` class object.
#'
#' @description
#' The `plot()` method for `etest` data frames with results of statistical
#' hypothesis testing.
#' The function generates a list of scatter plots (one for each test type)
#' with effect size statistic values on the X axis and raw p values or p values
#' adjusted for multiple testing on the Y axis.
#' Single variables are represented by points; point color codes for statistical
#' significance, and optionally, regulation sign.
#'
#' @return a list of `ggplot` objects.
#'
#' @inheritParams plot_factor
#' @inheritParams plot_numeric
#'
#' @param x an object of \code{\link{etest}} class.
#' @param p_variable name of the variable storing p values to be presented in the
#' plot. If `p_variable = "p_value"`, raw p values will be presented.
#' @param n_top number of top effects (absolute value of effect size) to be
#' labeled with variable names in the plot.
#' @param label_type type of the labels of the top strongest effects: text
#' generated with \code{\link[ggrepel]{geom_text_repel}} or labels created with
#' \code{\link[ggrepel]{geom_label_repel}}.
#' @param show_lines logical: should lines representing cutoffs of statistical
#' significance and null effect size be displayed in the plots?
#' `TRUE` by default.
#' @param line_type type of the lines representing the significance
#' and effect size cutoffs. Dashed by default.
#' @param line_alpha alpha/opacity of the lines representing the significance
#' and effect size cutoffs. Dashed by default.
#' @param fill_lab title for the fill scale.
#' @param labeller_fun a function used to transform names of variables
#' presented in data point labels.
#' @param plot_subtitle plot subtitle. If `NULL`, range of N numbers of observations
#' used in the statistical test will appear there.
#' @param point_hjitter height of jittering of data points. If `NULL`, jittering
#' will be determined with a simple heuristics
#' (10% of standard deviation of the plotting variable).
#' @param point_wjitter width of jittering of data points. If `NULL`, jittering
#' will be determined with a simple heuristics
#' (10% of standard deviation of the plotting variable).
#' @param ... additional arguments passed to \code{\link[ggplot2]{geom_point}}.
#'
#' @export plot.etest
#' @export

  plot.etest <- function(x,
                         p_variable = c("p_adjusted", "p_value"),
                         show_txt = TRUE,
                         txt_size = 2.75,
                         n_top = 10,
                         label_type = c("text", "label"),
                         labeller_fun = identity,
                         palette = etest_colors(),
                         fill_lab = "",
                         point_size = 2,
                         point_alpha = 1,
                         point_wjitter = 0,
                         point_hjitter = 0,
                         show_lines = TRUE,
                         line_type = "dashed",
                         line_width = 0.75,
                         line_alpha = 1,
                         plot_title = NULL,
                         plot_subtitle = NULL,
                         x_lab = NULL,
                         y_lab = NULL,
                         cust_theme = eda_classic_theme(), ...) {

    ## input controls -------

    stopifnot(is_etest(x))

    if(all(is.na(x[["effect_size"]]))) {

      stop("No effect size information in the object.", call. = FALSE)

    }

    if(all(is.na(x[["p_value"]])) | all(is.na(x[["p_adjusted"]]))) {

      stop("No significance information in the object.", call. = FALSE)

    }

    p_variable <- match.arg(p_variable, c("p_adjusted", "p_value"))

    stopifnot(is.logical(show_txt))
    show_txt <- show_txt[1]

    stopifnot(is.numeric(txt_size))
    txt_size <- txt_size[1]

    stopifnot(is.numeric(n_top))
    n_top <- as.integer(n_top[1])

    label_type <- match.arg(label_type[1], c("text", "label"))

    if(!is_function(labeller_fun)) {

      stop("`labeller_fun` has to be a function.", call. = FALSE)

    }

    stopifnot(is.character(palette))

    stopifnot(is.numeric(point_size))
    point_size <- point_size[1]

    stopifnot(is.numeric(point_alpha))
    point_alpha <- point_alpha[1]

    stopifnot(is.logical(show_lines))
    show_lines <- show_lines[1]

    stopifnot(is.numeric(line_width))
    line_width <- line_width[1]

    stopifnot(is.numeric(line_alpha))
    line_alpha <- line_alpha[1]

    if(!is.null(cust_theme)) {

      if(!is_theme(cust_theme)) {

        stop("`cust_theme` has to be a valid `ggplot` theme.", call. = FALSE)

      }

    }

    ## the plotting data and meta-data ---------

    ### list with results for particular tests

    plot_data <-
      select(x,
             any_of(c("variable",
                      "variable1",
                      "variable2")),
             all_of(c("test",
                      "n",
                      "p_cutoff",
                      "p_value",
                      "p_adjusted",
                      "effect_name",
                      "effect_size")))

    plot_data <- split(plot_data, f = plot_data[["test"]])

    plot_data <-
      map(plot_data,
          find_effect,
          p_variable)

    plot_label <- NULL

    if("variable" %in% names(x)) {

      plot_data <- map(plot_data,
                       ~mutate(.x,
                               plot_label = labeller_fun(.data[["variable"]])))

    } else {

      plot_data <- map(plot_data,
                       ~mutate(.x,
                               plot_label = paste(labeller_fun(.data[["variable1"]]),
                                                  labeller_fun(.data[["variable2"]]),
                                                  sep = "\n")))

    }

    ### plotting coordinates, jittering

    plot_data <- map(plot_data,
                     jitter_coordinates,
                     x_variable = "effect_size",
                     y_variable = p_variable,
                     x_sd = point_wjitter,
                     y_sd = point_hjitter)

    ### top largest effects: to be labelled with variable names in the plot

    top_effects <- map(plot_data,
                       slice_max,
                       abs(.data[["effect_size"]]),
                       n = n_top)

    ### plotting meta-data

    if(is.null(plot_title)) plot_title <- "Effect size and significance"

    if(is.null(plot_subtitle)) {

      n_numbers <- map(plot_data, ~range(.x[["n"]]))

      n_numbers <- map(n_numbers,
                       function(x){

                         if(length(unique(x)) > 1) return(paste(x, collapse = " to "))
                         return(unique(x))

                       })

      plot_subtitle <- map(n_numbers,
                           ~paste("observations: n =", .x))

      test_names <- names(plot_data)

      plot_subtitle <- map2(test_names, plot_subtitle, paste, sep = "\n")

    }

    if(is.null(x_lab)) {

      x_lab <- map_chr(plot_data, ~.x[["effect_name"]][[1]])

      x_lab <- paste("effect size,", x_lab)

    }

    if(is.null(y_lab)) y_lab <- "significance, -log\u2081\u2080 p value"

    p_cutoff <- x[["p_cutoff"]][1]
    effect_cutoff <- 0

    ## plots -----------

    plot_lst <-
      pmap(list(x = plot_data,
                y = plot_title,
                v = plot_subtitle,
                u = x_lab,
                z = y_lab),
           function(x, y, v, u, z) ggplot(x,
                                          aes(x = .data[[".x"]],
                                              y = -log10(.data[[".y"]]),
                                              fill = .data[["regulation"]])) +
             geom_point(shape = 21,
                        size = point_size,
                        alpha = point_alpha, ...) +
             scale_fill_manual(values = palette,
                               drop = FALSE) +
             scale_color_manual(values = palette,
                                drop = FALSE) +
             labs(title = y,
                  subtitle = v,
                  x = u,
                  y = z,
                  fill = fill_lab))

    if(show_lines) {

      plot_lst <-
        map(plot_lst,
            ~.x +
              geom_vline(xintercept = effect_cutoff,
                         linewidth = line_width,
                         alpha = line_alpha,
                         linetype = line_type) +
              geom_hline(yintercept = -log10(p_cutoff),
                         linewidth = line_width,
                         alpha = line_alpha,
                         linetype = line_type))

    }

    if(!is.null(cust_theme)) plot_lst <- map(plot_lst, ~.x + cust_theme)

    if(show_txt) {

      if(label_type == "text") {

        plot_lst <-
          map2(plot_lst,
               top_effects,
               ~.x +
                 geom_text_repel(data = .y,
                                 aes(label = .data[["plot_label"]],
                                     color = .data[["regulation"]]),
                                 size = txt_size,
                                 alpha = point_alpha,
                                 show.legend = FALSE))

      } else {

        plot_lst <-
          map2(plot_lst,
               top_effects,
              ~.x +
                geom_label_repel(data = .y,
                                 aes(label = .data[["plot_label"]]),
                                 size = txt_size,
                                 alpha = point_alpha,
                                 show.legend = FALSE))

      }

    }

    return(plot_lst)

  }
