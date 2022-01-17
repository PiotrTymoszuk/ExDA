# Analysis utils -------

#'A universal statistic extractor.
#'
#' @description calculates the requested statistic for a numeric vector.
#' NAs are removed prior to analysis
#' @param eda_object an eda object.
#' @param fun a statistic calculation function, should return a single numeric.
#' @param plain logical, should the output be coerced to a single vector?
#' @param stat_name name of the statistic included in the output data frame.
#' @param ... extra argument passed to fun.
#' @return a tibble or a numeric value.

   stat_extract <- function(eda_object, fun, stat_name, plain = FALSE, ...) {

     stopifnot(all(class(eda_object) == 'eda'))

     if(eda_object$type == 'factor') {

       warning('Statistic is not available for factor-type EDA objects.', call. = FALSE)

       return(NULL)

     }

     val <- fun(eda_object$value, ...)

     if(plain) {

       val

     } else {

       tibble::tibble(statistic = stat_name,
                      value = val)

     }


   }

# Distribution utils -------

#' Calculate expected values for the given variable.
#'
#' @param vector a numeric vector.
#' @param fun a function used for generation of the distribution, \code{\link[stats]{qnorm}} by default.
#' @param seed seed to be set for random number generation. If NULL, no seed is set.
#' @param na.rm should NA's be removed from the input vector?
#' @param ... extra arguments passed to fun.
#' @details credits to: https://stackoverflow.com/questions/43217104/coloring-points-in-a-geom-qq-plot.
#' @return A numeric vector of the same length as vector with the expected values.

   calc_expected_ <- function(vector, fun = stats::qnorm, seed = NULL, na.rm = TRUE, ...) {

     if(!is.null(seed)) set.seed(seed)

     if(na.rm) vector <- vector[!is.na(vector)]

     stopifnot(is.numeric(vector))

     fun(stats::ppoints(length(vector)), ...)

   }

# Chi square tester -----

#' Multi-sample Chi squared test.
#'
#' @description Performs a multi-sample Chi squared test for EDA objects as specified
#' in (\code{\link[stats]{chisq.test}}).
#' @param ... EDA objects.
#' @param test_mtx logical, should a test matrix be returned instead of test results?
#' @param coerce logial, coerce the input to factors prior to analysis?
#' @return a tibble with Chi squared, DF and p values.

   chi_tester <- function(..., test_mtx = FALSE, coerce = FALSE) {

     ## entry testing

     stopifnot(is.logical(test_mtx))
     stopifnot(is.logical(coerce))

     edas <- rlang::list2(...)

     classes <- purrr::map_chr(edas, ~class(.x)[1])

     if(!all(classes == 'eda')) stop('Factor-type EDA objects are required.', call. = FALSE)

     if(!coerce) {

        types <- purrr::map_chr(edas, ~.x$type[1])

        if(!all(types == 'factor')) stop('Factor-type EDA objects are required.', call. = FALSE)

     } else {

       edas <- purrr::map(edas, as_factor)

     }

     ## testing

     levs <- purrr::map(edas, ~.x$levels)

     levs <- purrr::reduce(levs, union)

     tst_mtx <- purrr::map(edas,
                           relevel,
                           newlevel = levs)

     tst_mtx <- purrr::map(tst_mtx, na.omit)

     tst_mtx <- purrr::map(tst_mtx, ~count(.x, .drop = FALSE)[['n']])

     tst_mtx <- purrr::reduce(tst_mtx, cbind)

     if(test_mtx) {

        tst_mtx

     } else {

        test_res <- stats::chisq.test(tst_mtx)

        exda::etest(test = 'Chi-squared',
                    stat_name = 'chi sqared',
                    stat = test_res[['statistic']],
                    df1 = test_res[['parameter']],
                    p_value = test_res[['p.value']],
                    n = sum(tst_mtx))

     }

   }

# List converter function for statistical testing -----

#' Convert multiple EDA objects to a tibble with grouping information.
#'
#' @description Tests the class and coerces multiple EDA objects to numerics.
#' @param ... EDA objects, at least two.
#' @param paired logical, are the observations paired?
#' @return a tibble with the factor variable 'group' and the EDAs values stored under 'variable'.
#' If paired is set to TRUE, the tibble contains the 'id' variable coding for sample pairing as well.

   convert_eda <- function(..., paired = FALSE) {

      ## entry testing

      edas <- rlang::list2(...)

      classes <- purrr::map_chr(edas, ~class(.x)[1])

      if(!all(classes == 'eda')) stop('Valid EDA objects are required.', call. = FALSE)
      if(length(edas) < 2) stop('At least two EDA objects are required.', call. = FALSE)

      edas <- purrr::map(edas, as_numeric)

      grouping <- factor(paste0('G', 1:length(edas)))

      if(paired) {

         lengths <- purrr::map_dbl(edas, length)

         if(any(lengths != lengths[1])) stop('Paired analysis requires EDA objects with the equal lengths.', call. = FALSE)

         ids <- factor(paste0('ID', 1:lengths[1]))

         purrr::map2_dfr(edas,
                         grouping,
                         ~dplyr::mutate(as_tibble(.x, newname = 'variable'),
                                        group = .y,
                                        id = ids))

      } else {

         purrr::map2_dfr(edas,
                         grouping,
                         ~dplyr::mutate(as_tibble(.x, newname = 'variable'),
                                        group = .y))

      }

   }

# Testing the
