# This script contains constructor functions to generate EDA objects.

# EDA class objects ----

#' Generate EDA objects.
#'
#' @description Generates EDA objects from multiple inputs such as vectors or data frame variables.
#' @param x and object to be converted to EDA.
#' @return an EDA object.
#' @export eda.default
#' @export

  eda.default <- function(x, ...) {

    eda_value <- try(as.vector(x), silent = TRUE)

    if(any(class(eda_value) == 'try-error')) stop('EDA object cannot be created from the provided input.', call. = FALSE)

    eda_obj <- switch(class(eda_value),
                      numeric = eda.numeric(eda_value),
                      character = eda.character(eda_value),
                      logical = eda.logical(eda_value))

    if(!is.null(eda_obj)) {

      eda_obj

    } else {

      stop('EDA object cannot be created from the provided input.', call. = FALSE)

    }

  }

#' Generate EDA objects from factors.
#'
#' @description Generates EDA objects from a factor.
#' @param x an object to be converted to EDA.
#' @return an EDA object.
#' @export

  eda.factor <- function(x, ...) {

    stopifnot(is.factor(x))

    structure(list(value = x,
                   type = 'factor',
                   levels = levels(x)),
              class = 'eda')

  }

#' Generate EDA objects from numeric vectors.
#'
#' @description Generates EDA objects from a numeric vector.
#' @param x an object to be converted to EDA.
#' @return an EDA object.
#' @export

  eda.numeric <- function(x, ...) {

    stopifnot(is.numeric(x))

    structure(list(value = x,
                   type = 'numeric',
                   levels = NULL),
              class = 'eda')

  }

#' Generate EDA objects from character vectors.
#'
#' @description Generates EDA objects from a character vector.
#' @param x an object to be converted to eda.
#' @return an EDA object.
#' @export

  eda.character <- function(x, ...) {

    stopifnot(is.character(x))

    message(paste('Converting the input character vector to a factor with the following levels:',
                  paste(levels(factor(x)), collapse = ', ')))

    eda(as.factor(x))

  }

#' Generate EDA objects from logical vectors.
#'
#' @description Generates EDA objects from a logical vector.
#' @param x an object to be converted to EDA.
#' @return an EDA object.
#' @export

  eda.logical <- function(x, ...) {

    stopifnot(is.logical(x))

    message(paste('Converting the input logical vector to a factor with the following levels:',
                  paste(levels(factor(x)), collapse = ', ')))

    eda(as.factor(x))

  }

#' Generate an EDA object from a data frame variable.
#'
#' @description Generates EDA objects from a data frame variable.
#' @param data and object to be converted to EDA.
#' @param variable a name or reference (quoted or unquoted) to a data frame variable.
#' @return an EDA object.
#' @export eda.data.frame
#' @export

  eda.data.frame <- function(data, variable, ...) {

    stopifnot(is.data.frame(data))

    var_ex <- rlang::enexpr(variable)

    eda_value <- try(eval(var_ex, envir = data), silent = TRUE)

    if(any(class(eda_value) == 'try-error')) {

      eda_value <- try(data[[variable]], silent = TRUE)

      if(any(class(eda_value) == 'try-error')) stop('Variable absent from the provided data frame.', call. = FALSE)

    }

    eda(eda_value)

  }

# Test class objects ----

#' Generate eTest objects.
#'
#' @description Generates eTest objects storing statistic test results in
#' a standardized tibble.
#' @param test statistical test name.
#' @param stat_name name of the test statistic.
#' @param stat value of the test statistic.
#' @param df1 = degrees of freedom.
#' @param df2 degrees of freedom.
#' @param estimate_name name of the test estimate.
#' @param estimate value of the test estimate.
#' @param lower_ci value of the lower confidence interval limit.
#' @param upper_ci value of the upper confidence interval limit.
#' @param p_value p value.
#' @param n number of complete observations used for testing.
#' @return an EDA object.
#' @export

  etest <- function(test,
                    stat_name = NA,
                    stat = NA,
                    df1 = NA,
                    df2 = NA,
                    estimate_name = NA,
                    estimate = NA,
                    lower_ci = NA,
                    upper_ci = NA,
                    p_value = NA,
                    n = NA) {

    x <- tibble::tibble(test = test,
                        stat_name = stat_name,
                        stat = stat,
                        df1 = df1,
                        df2 = df2,
                        estimate_name = estimate_name,
                        estimate = estimate,
                        lower_ci = lower_ci,
                        upper_ci = upper_ci,
                        p_value = p_value,
                        n = n)

    structure(x, class = c('etest', class(x)))

  }



# Class testers -----

#' Test the EDA class.
#'
#' @description Tests if the object is an instance of the EDA class.
#' @param x an object.
#' @return a logical value.
#' @export

  is_eda <- function(x) {

    any(class(x) == 'eda')

  }

  #' Test the eTest class.
  #'
  #' @description Tests if the object is an instance of the eTest class.
  #' @param x an object.
  #' @return a logical value.
  #' @export

  is_etest <- function(x) {

    any(class(x) == 'etest')

  }
