# Functions for plotting of variables in a data frame.

# Distribution plots --------

#' Plot variable distribution.
#'
#' @description
#' `ggplot` plots with distribution of a variable in analysis groups.
#' For factor variables, stack, bar, and dot plots with counts or percentages of
#' observations in the analysis groups.
#' For numeric variables, violin and box plots, paired/matched plots ("before/after"),
#' histograms, density plots, and quantile - quantile (QQ) plots are available.
#'
#' @details
#' If `type = "paired"`, a before-after plot is generated and its assumed that
#' the plotting data are already arranged by the blocking or identified variable.
#'
#' @return a `ggplot` object.
#'
#' @inheritParams plot_df_factor
#' @param type plot type: "stack", "bar", "violin", "box", "paired",
#' "histogram", "density", or "qq".
#' If not provided, a stack plot is returned for factors and a violin plot is
#' returned for numeric objects.
#' @param ... additional arguments passed to internal plotting functions
#' (bar and stack plots: \code{\link{plot_df_factor}}; violin, box and paired
#' plots: \code{\link{plot_df_numeric}}, histograms: \code{\link{plot_df_histogram}},
#' QQ plots: \code{\link{plot_df_qq}}).
#' They specify, among others, colors and opacity of
#' points and shapes, point sizes, text labels, and `ggplot` themes.
#'
#' @export

  plot_variable <- function(data,
                            variable,
                            split_factor,
                            type = NULL, ...) {

    ## input control ----------

    ### detailed checks are done by internal plotting functions

    factor_types <- c("bar", "stack", "bubble")
    numeric_types <- c("violin", "box", "paired", "histogram", "density", "qq")

    if(!is.null(type)) {

      if(!type %in% c(factor_types, numeric_types)) {

        stop(paste("Type must be one of:",
                   paste(c(factor_types, numeric_types),
                         collapse = ", ")),
             call. = FALSE)

      }

    } else {

      validate_df(data, variable, split_factor)

      if(is.factor(data[[variable]])) {

        type <- "stack"

      } else {

        type <- "violin"

      }

    }

    ## plotting --------

    switch(type,
           bar = plot_df_factor(data, variable, split_factor,
                                type = "bar", ...),
           stack = plot_df_factor(data, variable, split_factor,
                                  type = "stack", ...),
           bubble = plot_df_factor(data, variable, split_factor,
                                   type = "bubble", ...),
           violin = plot_df_numeric(data, variable, split_factor,
                                    type = "violin", ...),
           box = plot_df_numeric(data, variable, split_factor,
                                 type = "box", ...),
           paired = plot_df_numeric(data, variable, split_factor,
                                    type = "paired", ...),
           histogram = plot_df_histogram(data, variable, split_factor,
                                         type = "histogram", ...),
           density = plot_df_histogram(data, variable, split_factor,
                                       type = "density", ...),
           qq = plot_df_qq(data, variable, split_factor, ...))

  }

# END ----------
