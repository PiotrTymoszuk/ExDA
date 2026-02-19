# Special plots and plot panels for displaying distribution of multiple
# variables in one graphics

#' Panel with distribution of multiple variables in a data frame.
#'
#' @description
#' `ggplot` graphics with plots of distribution of multiple numeric or factor
#' variables in a data frame.
#'
#' @return a `ggplot` object.
#'
#' @inheritParams plot_df_factor
#' @param variables a character vector with variable names
#' @param central_stat statics of centrality to be presented in ribbon and Forest
#' plots. Ignored for other plot types.
#'
#'
#' @export

  draw_numeric_panel <- function(data,
                                 variables,
                                 split_factor = NULL,
                                 type = c("violin", "box", "bar",
                                          "ribbon", "forest"),
                                 central_stat = c("mean", "median"),
                                 distribution_stat = c("sem", "sd", "iqr",
                                                       "95%", "2sem", "2sd",
                                                       "none"),
                                 palette = tableau10_colors(),
                                 shape_fill = "steelblue",
                                 shape_color = "black",
                                 shape_alpha = 0.25,
                                 point_size = 2,
                                 point_color = "steelblue",
                                 point_slpha = 0.75,
                                 point_wjitter = 0,
                                 point_hjitter = 0.1,
                                 line_color = "steelblue",
                                 line_width = 0.75,
                                 line_alpha = 1,
                                 cust_theme = eda_classic_theme(),
                                 plot_title = NULL,
                                 plot_subtitle = NULL,
                                 x_lab = NULL,
                                 y_lab = NULL,
                                 ...) {

  }
