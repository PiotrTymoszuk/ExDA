# General methods of `eda` and `etest` objects

# Printing -----------

#' Appearance/printing of `eda` and `etest` objects

#' @description
#' Printing methods.
#'
#' @param x an object.
#' @param ... arguments for methods.
#'
#' @return invisibly returns the object.
#' @export

  print.eda <- function(x, ...) NextMethod()

#' @rdname print.eda
#' @export

  print.etest <- function(x, ...) NextMethod()

# NA removal ---------

#' `NA` removal from `eda` objects.
#'
#' @description removes `NA` values from the `eda` objects.
#'
#' @param object an \code{\link{eda}} object.
#' @param ... arguments for methods.
#'
#' @return an `eda` object without `NA` values.
#'
#' @export

  na.omit.eda <- function(object, ...) {

    stopifnot(is_eda(object))

    new_vals <- object[!is.na(object)]

    eda(new_vals)

  }

#' @rdname na.omit.eda
#' @export

  na.exclude.eda <- function(object, ...) {

    stopifnot(is_eda(object))

    new_vals <- object[!is.na(object)]

    eda(new_vals)

  }

# Numbers of observations --------

#' Number of (complete) observations in an `eda` object.
#'
#' @description counts the number of all (complete if na.rm is set to TRUE)
#' observations in the EDA object.
#'
#' @param object an \code{\link{eda}} object.
#' @param plain logical, should the values be returned as a numeric vector.
#' @param ... additional arguments passed to methods, currently none.
#'
#' @return a tibble or numeric vector with the number of all and complete
#' observations.
#'
#' @export

  nobs.eda <- function(object, plain = FALSE, ...) {

    stopifnot(is_eda(object))

    observations <- NULL
    n <- NULL

    lens <- c("total" = length(object),
              "complete" = length(na.omit(object)))

    if(plain) return(lens)

    tibble(observations = names(lens),
           n = lens)

  }

# EDA object summary -----

#' Summary of `eda` objects.
#'
#' @description Summary of numeric and factor `eda` objects.
#'
#' @details The `pub_styled` argument allows for generating a 'nicer' output
#' accepted by multiple journals.
#' A tibble with the column 'statistic' is returned.
#'
#' @param object \code{\link{eda}} object.
#' @param pub_styled logical, should the output be publication-ready formatted?
#' See the details.
#' @param signif_digits number of significant digits used for rounding in
#' the publication-style output.
#' @param ... additional arguments for methods.
#'
#' @export
#' @export summary.eda

  summary.eda <- function(object,
                          pub_styled = FALSE,
                          signif_digits = 2, ...) {

    ## input controls --------

    stopifnot(is_eda(object))
    stopifnot(is.logical(pub_styled))
    stopifnot(is.numeric(signif_digits))

    signif_digits <- as.integer(signif_digits[1])

    total_observations <- length(object)
    complete_observations <- length(object[!is.na(object)])

    statistic <- NULL

    ## handling of a special case: only NA values -------

    if(all(is.na(object))) {

      if(!pub_styled) return(NA)

      return(tibble(statistic = NA))

    }

    ## summary for factors --------

    if(is.factor(object)) {

      if(!pub_styled) {

        return(frequency(object))

      } else {

        counts <- as.table(object)
        perc_complete <- as.table(object, scale = "percent")
        levs <- names(counts)

        cat_string <-
          pmap_chr(list(x = levs,
                        y = perc_complete,
                        z = counts),
                   function(x, y, z) paste0(x, ": ", signif(y, signif_digits),
                                            "% (", z, ")"))

        cat_string <- paste(cat_string, collapse = "\n")

        cat_string <- paste(cat_string,
                            complete_observations,
                            sep = "\ncomplete: n = ")

        return(tibble(statistic = cat_string))

        }

    }

    ## summary for numeric objects ---------

    stat_funs <- list(mean,
                      sd,
                      median,
                      function(x) quantile(x, c(0.25, 0.75)),
                      function(x) quantile(x, c(0, 1)))

    stat_names <-
      c("mean", "sd", "median", "perc_25", "perc_75", "min", "max")

    stats <- map_dfr(stat_funs, ~.x(object))

    if(!pub_styled) {

      stats[["statistic"]] <- stat_names

      stats[["n_complete"]] <- complete_observations
      stats[["n_total"]] <- total_observations

      return(stats[, c("statistic", "value", "n_complete", "n_total")])

    }

    stat_string <- paste0("mean = ", signif(stats$value[1], signif_digits),
                          " (SD: ", signif(stats$value[2], signif_digits), ")",
                          "\nmedian = ", signif(stats$value[3], signif_digits),
                          " [IQR: ", signif(stats$value[4], signif_digits),
                          " - ", signif(stats$value[5], signif_digits), "]",
                          "\nrange: ", signif(stats$value[6], signif_digits),
                          " - ", signif(stats$value[7], signif_digits))

    stat_string <- paste(stat_string,
                         complete_observations,
                         sep = "\ncomplete: n = ")

    tibble(statistic = stat_string)

  }

# conversion methods ---------

#' Conversion methods for `eda` objects.
#'
#' @description
#' Conversion methods for `eda` objects.
#'
#' @details See: \code{\link[base]{factor}}, \code{\link[base]{as.numeric}},
#' \code{\link[base]{as.integer}}, and \code{\link[base]{as.data.frame}}.
#' While converting a numeric object into a factor one, the levels corrspüond to
#' the sorted unique numeric values of the object.
#' Not that this behavior is different that the R's standard one.
#'
#' @param x an \code{\link{eda}} object.
#' @param row.names row names, see \code{\link[base]{as.data.frame}}.
#' @param optional logical, is diversion from R's naming scheme allowed?
#' See \code{\link[base]{as.data.frame}}
#' @param col.names of the data frame variable which stores
#' the `eda` object values.
#' @param levels levels to be set during factor conversion.
#' @param ... additional arguments passed to methods
#'
#' @export

  as.numeric.eda <- function(x, ...) {

    stopifnot(is_eda(x))

    if(is.numeric(x)) return(x)

    eda(base::as.numeric(unclass(x)))

  }

#' @rdname as.numeric.eda
#' @export

  as.integer.eda <- function(x, ...) {

    stopifnot(is_eda(x))

    if(is.integer(x)) return(x)

    eda(base::as.integer(unclass(x)))

  }

#' @rdname as.numeric.eda
#' @export

  factor.eda <- function(x, levels = NULL, ...) {

    stopifnot(is_eda(x))

    if(is.null(levels)) {

      if(is.factor(x)) {

        levs <- attr(x, "levels")

      } else {

        levs <- as.character(sort(x))

      }

    } else {

      levs <- levels

    }

    if(!is.null(levs)) {

      return(eda(base::factor(as.character(x), levels = levs, ...)))

    }

    eda(base::factor(as.character(x), ...))

  }

#' @rdname as.numeric.eda
#' @export

  as.data.frame.eda <- function(x,
                                row.names = NULL,
                                optional = FALSE,
                                col.names = "variable", ...) {

    stopifnot(is_eda(x))

    stopifnot(is.character(col.names))

    col.names <- col.names[1]

    set_names(as.data.frame(unclass(x),
                            row.names = row.names,
                            optional = optional, ...),
              col.names)

  }

# Splitting ------------

#' Splitting of `eda` objects by levels of a factor.
#'
#' @description
#' Splitting of an `eda` object by levels of a factor (a plain factor or
#' an `eda` object).
#'
#' @return a list of \code{\link{eda}} objects.
#'
#' @details
#' The lengths of `x` and `f` vectors have to be equal.
#' If there are any `NA` values in the `f` vector, the corresponding values in
#' `x` are removed prior to splitting.#'
#'
#' @param x an \code{\link{eda}} object.
#' @param f a factor or a factor \code{\link{eda}} object.
#' @param drop logical, should empty levels be dropped?
#' @param ... extra arguments passed to methods.
#'
#' @export

  split.eda <- function(x, f, drop = TRUE, ...) {

    ## entry control --------

    stopifnot(is_eda(x))

    if(!is.factor(f)) {

      stop("'f' has to be a plain factor or a factor `eda` object.",
           call. = FALSE)

    }

    stopifnot(is.logical(drop))

    if(drop) f <- droplevels(f)

    if(length(x) != length(f)) {

      stop("Lengths of `x` and `f` must be equal.", call. = FALSE)

    }

    complete_idx <- !is.na(f)

    f <- f[complete_idx]
    x <- x[complete_idx]

    ## splitting ---------

    obj_lst <- base::split.default(x, f)

    map(obj_lst, eda)

  }

# plotting ---------

#' Plot distribution.
#'
#' @description
#' Distribution plots for `eda` objects storing factors (bar and stack plots
#' with frequencies of observations in the categories) and numeric values
#' (violin and box plots, histograms, density plots,
#' and quantile - quantile/QQ plots).
#'
#' @return a `ggplot` object.
#'
#' @param x \code{\link{eda}} object.
#' @param type plot type: "stack", "bar", "violin", "box", "histogram", "density",
#' or "qq".
#' If not provided, a stack plot is returned for factors and a violin plot is
#' returned for numeric objects.
#' @param ... additional arguments passed to internal plotting functions
#' (bar and stack plots: \code{\link{plot_factor}}, violin and box plots:
#' \code{\link{plot_numeric}}, histograms: \code{\link{plot_histogram}}, QQ plots:
#' \code{\link{plot_qq}}). They specify, among others, colors and opacity of
#' points and shapes, point sizes, text labels, and `ggplot` themes.
#'
#' @export plot.eda
#' @export

  plot.eda <- function(x,
                       type = NULL, ...) {

    ## input control ---------

    stopifnot(is_eda(x))

    if(all(is.na(x))) {

      warning("No data to plot: NA-only object.", call. = FALSE)

      return(NULL)

    }

    factor_types <- c("bar", "stack", "bubble")
    numeric_types <- c("violin", "box", "histogram", "density", "qq")

    if(!is.null(type)) {

      if(!type %in% c(factor_types, numeric_types)) {

        stop(paste("Type must be one of:",
                   paste(c(factor_types, numeric_types),
                         collapse = ", ")),
             call. = FALSE)

      }

      if(is.factor(x) & type %in% numeric_types) {

        stop(paste("Available types of plots for factors are:",
                   paste(factor_types, collapse = ", ")))

      }

      if(is.numeric(x) & type %in% factor_types) {

        stop(paste("Available types of plots for numeric objects are:",
                   paste(numeric_types, collapse = ", ")))

      }

    } else {

      if(is.factor(x)) {

        type <- "stack"

      } else {

        type <- "violin"

      }

    }

    ## plotting -------

    switch(type,
           bar = plot_factor(x, type = "bar", ...),
           stack = plot_factor(x, type = "stack", ...),
           bubble = plot_factor(x, type = "bubble", ...),
           violin = plot_numeric(x, type = "violin", ...),
           box = plot_numeric(x, type = "box", ...),
           histogram = plot_histogram(x, type = "histogram", ...),
           density = plot_histogram(x, type = "density", ...),
           qq = plot_qq(x, ...))

  }

# END -------
