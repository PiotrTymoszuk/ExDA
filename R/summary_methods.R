# EDA object summary -----

#' Summary for EDA objects.
#'
#' @description Summary of numeric- and factor-type EDA objects.
#' @param eda_object eda object, created by \code{\link{eda}}.
#' @param pub_styled logical, should the output be publication-ready formatted? See the details.
#' @param signif_digits significant digits used for rounding in the publication-style output.
#' @details The pub_style argument allows for generating a 'nicer' output accepted by multiple journals.
#' A tibble with the column 'statistic' is returned.
#' @export summary.eda
#' @export

  summary.eda <- function(eda_object,
                          pub_styled = FALSE,
                          signif_digits = 2) {

    stopifnot(all(class(eda_object) == 'eda'))
    stopifnot(is.logical(pub_styled))
    stopifnot(is.numeric(signif_digits))

    signif_digits <- as.integer(signif_digits)

    counts <- nobs(eda_object)

    eda_object <- na.omit(eda_object)

    if(eda_object$type == 'factor') {

      if(!pub_styled) {

        return(list(statistic = count(eda_object),
                    observations = counts))

      } else {

        cat_string <- count(eda_object)

        cat_string <- purrr::map_chr(1:nrow(cat_string),
                                     ~paste0(cat_string$category[.x], ': ',
                                             signif(cat_string$percent[.x], signif_digits), '% (',
                                             cat_string$n[.x], ')'))

        cat_string <- paste(cat_string, collapse = '\n')

        n_string <- paste('Complete: n =', nobs(eda_object)[['n']][2])

        return(tibble::tibble(statistic = paste(cat_string, n_string, sep = '\n')))

      }

    } else {

      stats <- list(mean, sd,
                    median, function(x) quantile(x, c(0.25, 0.75)),
                    function(x) quantile(x, c(0, 1)))

      stats <- purrr::map_dfr(stats,
                              ~.x(eda_object))

      stats <- dplyr::mutate(stats,
                             statistic = c('mean', 'sd', 'median', 'perc_25', 'perc_75', 'min', 'max'))

      if(!pub_styled) {

        return(list(statistic = stats[c('statistic', 'value')],
                    observations = counts))

      } else {

        stat_string <- paste0('Mean = ', signif(stats$value[1], signif_digits),
                              ' (SD: ', signif(stats$value[2], signif_digits), ')',
                              '\nMedian = ', signif(stats$value[3], signif_digits),
                              ' [IQR: ', signif(stats$value[4], signif_digits),
                              ' - ', signif(stats$value[5], signif_digits), ']',
                              '\nRange: ', signif(stats$value[6], signif_digits),
                              ' - ', signif(stats$value[7], signif_digits))

        n_string <- paste('Complete: n =', nobs(eda_object)[['n']][2])

        return(tibble::tibble(statistic = paste(stat_string, n_string, sep = '\n')))

      }

    }

  }

# eTest object summary -----

#' Summary for the eTest objects.
#'
#' @description Returns a tibble representation of the statistical testing for differences between EDA objects.
#' @param etest_object an eTest object.
#' @param pub_styled logical, should the output be publication-ready formatted? See the details.
#' @param signif_digits significant digits used for rounding in the publication-style output.
#' @param simplify_p logical, should p_values < 0.001 be presented in a p < 0.001 form?
#' @details The pub_style argument allows for generating a 'nicer' output accepted by multiple journals.
#' A tibble with the columns 'test', 'test_stat', 'p_value', 'significance', 'effect size' and 'n' is returned.
#' @export summary.etest
#' @export

  summary.etest <- function(etest_object, pub_styled = FALSE, signif_digits = 2, simplify_p = TRUE) {

    stopifnot(any(class(etest_object) == 'etest'))
    stopifnot(is.logical(pub_styled))
    stopifnot(is.numeric(signif_digits))

    signif_digits <- as.integer(signif_digits)

    if(!pub_styled) return(etest_object)

    if(simplify_p) {

      nice_test <- dplyr::mutate(etest_object,
                                 significance = ifelse(p_value < 0.001,
                                                       'p < 0.001',
                                                       ifelse(p_value < 0.05,
                                                              paste('p =', signif(p_value, signif_digits)),
                                                              paste0('ns (p = ', signif(p_value, signif_digits), ')'))))

    } else {

      nice_test <- dplyr::mutate(etest_object,
                                 significance = ifelse(p_value < 0.05,
                                                       paste('p =', signif(p_value, signif_digits)),
                                                       paste0('ns (p = ', signif(p_value, signif_digits), ')')))

    }

    nice_test <- dplyr::mutate(nice_test,
                               stat_name = stringi::stri_replace(stat_name, fixed = ' squared', replacement = '\u00B2'),
                               estimate_name = stringi::stri_replace(estimate_name, fixed = ' squared', replacement = '\u00B2'),
                               stat_name = stringi::stri_replace(stat_name, fixed = 'chi ', replacement = '\u03C7'),
                               stat_name = stringi::stri_replace(stat_name, fixed = 'eta ', replacement = '\u03B7'),
                               stat_name = stringi::stri_replace(stat_name, fixed = 'kappa', replacement = '\u03BA'),
                               stat_name = stringi::stri_replace(stat_name, fixed = 'rho ', replacement = '\u03C1'),
                               stat_name = stringi::stri_replace(stat_name, fixed = 'tau ', replacement = '\u03C4'),
                               estimate_name = stringi::stri_replace(estimate_name, fixed = 'chi', replacement = '\u03C7'),
                               estimate_name = stringi::stri_replace(estimate_name, fixed = 'eta', replacement = '\u03B7'),
                               estimate_name = stringi::stri_replace(estimate_name, fixed = 'kappa', replacement = '\u03BA'),
                               estimate_name = stringi::stri_replace(estimate_name, fixed = 'rho ', replacement = '\u03C1'),
                               estimate_name = stringi::stri_replace(estimate_name, fixed = 'tau ', replacement = '\u03C4'),
                               estimate_name = stringi::stri_replace(estimate_name, fixed = 'median difference', replacement = '\u0394 median'),
                               estimate_name = stringi::stri_replace(estimate_name, fixed = 'mean difference', replacement = '\u0394 median'),
                               test_stat = ifelse(is.na(df1),
                                                  paste(stat_name, '=', signif(stat, signif_digits)),
                                                  ifelse(is.na(df2),
                                                         paste0(stat_name, '(', df1, ') = ', signif(stat, signif_digits)),
                                                         paste0(stat_name, '(', df1, ', ', df2, ') = ', signif(stat, signif_digits)))),
                               eff_size = ifelse(is.na(estimate),
                                                 NA,
                                                 ifelse(is.na(lower_ci),
                                                        paste(estimate_name, '=', signif(estimate, signif_digits)),
                                                        paste0(estimate_name, ' = ', signif(estimate, signif_digits),
                                                               ' [', signif(lower_ci, signif_digits),
                                                               ' - ', signif(upper_ci, signif_digits), ']'))))

    nice_test[c('test', 'test_stat', 'p_value', 'significance', 'eff_size', 'n')]


  }
