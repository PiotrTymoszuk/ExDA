# Functions for plotting of variables in a data frame.

# Distribution plots --------

#' Plot distribution of a variable in a data frame.
#'
#' @description
#' `plot_variable()` creates `ggplot` plots with distribution of a variable:
#' alone or in an analysis.
#' For factor variables, stack, bar, and dot plots with counts or percentages of
#' observations in the analysis groups.
#' For numeric variables, violin and box plots, paired/matched plots ("before/after"),
#' histograms, density plots, and quantile - quantile (QQ) plots are available.
#'
#' @details
#' If `split_factor = NULL`, distribution plots for the requested variable
#' in the whole data frame are generated.
#' If `type = "paired"`, a before-after plot is generated and its assumed that
#' the plotting data are already arranged by the blocking or identified variable;
#' please note that this type is available only if `split_factor` is not `NULL`.
#' The dots (`...`) pass additional arguments to internal plotting functions.
#' These internal plottin functions are:
#'
#' * if `split_factor` is `NULL`.
#' Bar, stack and bubble plots: \code{\link{plot_factor}}.
#' Violin and box plots: \code{\link{plot_numeric}}.
#' Histograms: \code{\link{plot_histogram}}.
#' QQ plots: \code{\link{plot_qq}}.
#'
#' * if `split_factor` is not `NULL`.
#' Bar, stack and bubble plots: \code{\link{plot_df_factor}}.
#' Violin, box and paired plots: \code{\link{plot_df_numeric}}.
#' Histograms: \code{\link{plot_df_histogram}}.
#' QQ plots: \code{\link{plot_df_qq}}.
#'
#' @return a `ggplot` object.
#'
#' @inheritParams plot_df_factor
#' @param type plot type: "stack", "bar", "violin", "box", "paired",
#' "histogram", "density", or "qq".
#' If not provided, a stack plot is returned for factors and a violin plot is
#' returned for numeric objects.
#' @param ... additional arguments passed to internal plotting functions.
#' They specify, among others, type of plotted frequency data, colors and opacity of
#' points and shapes, point sizes, text labels, and `ggplot` themes.
#' See Details.
#'
#' @md
#' @export

  plot_variable <- function(data,
                            variable,
                            split_factor = NULL,
                            type = NULL, ...) {

    ## plotting: distribution in the analysis groups --------

    if(!is.null(split_factor)) {

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

        } else if(is.numeric(data[[variable]])) {

          type <- "violin"

        } else {

          stop("`variable` must specify a numeric of factor variable.",
               call. = FALSE)

        }

      }

      dots <- list2(...)

      if(!"plot_title" %in% names(dots)) {

        return(
          switch(type,
                 bar = plot_df_factor(data,
                                      variable,
                                      split_factor,
                                      type = "bar",
                                      plot_title = variable, ...),
                 stack = plot_df_factor(data,
                                        variable,
                                        split_factor,
                                        type = "stack",
                                        plot_title = variable, ...),
                 bubble = plot_df_factor(data,
                                         variable,
                                         split_factor,
                                         type = "bubble",
                                         plot_title = variable, ...),
                 violin = plot_df_numeric(data,
                                          variable,
                                          split_factor,
                                          type = "violin",
                                          plot_title = variable, ...),
                 box = plot_df_numeric(data,
                                       variable,
                                       split_factor,
                                       type = "box",
                                       plot_title = variable, ...),
                 paired = plot_df_numeric(data,
                                          variable,
                                          split_factor,
                                          type = "paired",
                                          plot_title = variable, ...),
                 histogram = plot_df_histogram(data,
                                               variable,
                                               split_factor,
                                               type = "histogram", ...),
                 density = plot_df_histogram(data,
                                             variable,
                                             split_factor,
                                             type = "density",
                                             plot_title = variable, ...),
                 qq = plot_df_qq(data,
                                 variable,
                                 split_factor,
                                 plot_title = variable, ...))
        )

      } else {

        return(
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
        )

      }

    }

    ## plotting: distribution in the entire data set -------

    if(!is.null(type)) {

      if(type == "paired") {

        stop("`type = 'paired'` is available only is `split_factor` is specified.",
             call. = FALSE)

      }

    }

    if(!is.data.frame(data)) stop("`data` has to be a data frame.", call. = FALSE)

    if(!variable %in% names(data)) stop("`variable` absent from the data frame.", call. = FALSE)

    dots <- list2(...)

    if(!"plot_title" %in% names(dots)) {

      return(plot(eda(data[[variable]],
                      .drop = FALSE),
                  type = type,
                  plot_title = variable, ...))

    } else {

      return(plot(eda(data[[variable]],
                      .drop = FALSE),
                  type = type, ...))

    }



  }

#' Plot distribution of two variables in a data frame.
#'
#' @description
#' Functions `plot_two_variables()` and `plot_correlation()` generate
#' `ggplot` plots of distribution of two variables.
#' For a pair of two numeric variables, a scatter plot is generated.
#' Frequencies of observations (counts or percentages of complete cases)
#' for a pair of factors are displayed in a heat map or a bubble plot.
#'
#' @details
#' The functions work for two numeric or two factor variables; if the variables'
#' types are mixed, and error is thrown.
#' In such cases, the user is better served by \code{\link{plot_variable}} function.
#' Function `plot_correlation()` is kept only for compatibility with previous
#' versions of the package and will be removed in future.
#' Please use `plot_two_variables()` instead,
#'
#' @return a `ggplot` object.
#'
#' @inheritParams plot_2df_numeric
#' @param variables a character vector with names of the two variables to be
#' presented in the plot.
#' @param type plot type: "scatter" for a pair of numeric variables, "heat_map"
#' or "bubble" for a pair of factors. If `NULL`, a scatter plot is generated
#' for numeric, and a heat map is plotted for factor variables.
#' @param ... additional arguments passed to internal plotting functions
#' (numeric variables: \code{\link{plot_2df_numeric}},
#' factor variables: \code{\link{plot_2df_factor}}).
#' They specify, among others, type of plotted frequency data, colors and opacity of
#' points and shapes, point sizes, text labels, and `ggplot` themes.
#'
#' @export

  plot_two_variables <- function(data,
                                 variable1,
                                 variable2,
                                 type = NULL, ...) {

    ## input control ---------

    ### detailed checks are done by internal functions

    plot_types <- c("scatter", "heat_map", "bubble")

    if(!is.null(type)) {

      if(!type %in% plot_types) {

        stop(paste("Available plot types are:",
                   paste(plot_types, collapse = ", ")),
             call. = FALSE)

      }

    } else {

      validate_2df(data, variable1, variable2)

      if(is.factor(data[[variable1]])) {

        type <- "heat_map"

      } else if(is.numeric(data[[variable1]])) {

        type <- "scatter"

      } else {

        stop("`variable1` and `variable2` must specify factor or numeric variables.",
             call. = FALSE)

      }

    }

    ## plots --------

    dots <- list2(...)

    if(!"plot_title" %in% names(dots)) {

      title_txt <- paste(variable1, variable2, sep = " and ")

      return(
        switch(type,
               scatter = plot_2df_numeric(data,
                                          variable1,
                                          variable2,
                                          plot_title = title_txt, ...),
               heat_map = plot_2df_factor(data,
                                          variable1,
                                          variable2,
                                          type = "heat_map",
                                          plot_title = title_txt, ...),
               bubble = plot_2df_factor(data,
                                        variable1,
                                        variable2,
                                        type = "bubble",
                                        plot_title = title_txt, ...))
        )

    } else {

     return(
       switch(type,
              scatter = plot_2df_numeric(data,
                                         variable1,
                                         variable2, ...),
              heat_map = plot_2df_factor(data,
                                         variable1,
                                         variable2,
                                         type = "heat_map", ...),
              bubble = plot_2df_factor(data,
                                       variable1,
                                       variable2,
                                       type = "bubble", ...))
       )

    }

  }

#' @rdname plot_two_variables
#' @export

  plot_correlation <- function(data,
                               variables,
                               type = NULL, ...) {

    ## warning and re-routing --------

    warning("`plot_correlation()` is deprecated. Please use `plot_two_variables()` instead.",
            call. = FALSE)

    stopifnot(is.character(variables))

    if(length(variables) < 2) {

      stop("`variables` must be a character vector with at least two elements.",
           call. = FALSE)

    }

    if(length(variables) > 2) {

      stop("`variables` has > 2 elements; the first two will be used.",
           call. = FALSE)

    }

    plot_two_variables(data,
                       variables[1],
                       variables[2],
                       type = type, ...)

  }

# END ----------
