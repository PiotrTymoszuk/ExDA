# Normality testing ------

#' Test normality of an EDA object.
#'
#' @description tests normality of an EDA object values with Shapiro-Wilk test. For factor-type ones
#' NULL is returned and a warning generated.
#' @param eda_object an EDA object, created by \code{\link{eda}}.
#' @details a wrapper around \code{\link[stats]{shapiro.test}}.
#' @return an eTest object with the W statistic and p value.
#' @export

  normality <- function(eda_object) {

    stopifnot(is_eda(eda_object))

    if(eda_object$type == 'factor') {

      warning('Statistic is not available for factor-type EDA objects.', call. = FALSE)

      return(NULL)

    }

    test_res <- stats::shapiro.test(eda_object$value)

    exda::etest(test = 'Shapiro-Wilk test',
                stat_name = 'W',
                stat = test_res[['statistic']],
                p_value = test_res[['p.value']],
                n = length(na.omit(eda_object)))

  }

# Comparing distributions ------

#' Compare distributions of two EDA objects or with a theoretical distribution.
#'
#' @description For numeric-type EDAs: compares distributions of two EDA objects or of an EDA object with
#' a user-specified theoretical distribution with Kolmorgov-Smirnov test (\code{\link[stats]{ks.test}}).
#' For factor-type EDA objects: the distribution is compared via Chi squared test (\code{\link[stats]{chisq.test}}).
#' @param eda_object an EDA object, created by \code{\link{eda}}.
#' @param y form comparison of numeric distributions: an EDA object
#' or a numeric vector form comparing two factors: another EDA object with the same levels.
#' @param ... extra arguments passed to \code{\link[stats]{ks.test}}.
#' @return an eTest object with the D statistic and p value for numeric distribution
#' or an eTest object with the Chi-squared, df and p value for categorical EDAs.
#' @export

  distribution <- function(eda_object, y, ...) {

    stopifnot(is_eda(eda_object))
    if(all(class(y) != 'eda') | is.numeric(y)) stop('y has to be an EDA object or a numeric vector.', call. = FALSE)

    if(eda_object$type == 'factor') {

      if(y$type != 'factor') stop('y has to be a factor-type EDA object.', call. = FALSE)

      exda:::chi_tester(eda_object, y)

    } else {

      if(y$type != 'numeric' | is.numeric(y)) stop('y has to be a numeric-type EDA object or a numeric vector.', call. = FALSE)

      if(is.numeric(y)) {

        test_res <- stats::ks.test(x = eda_object$value,
                                   y = y, ...)

      } else {

        test_res <- stats::ks.test(x = eda_object$value,
                                   y = y$value, ...)

      }

      n <- length(eda_object) + length(y)

      exda::etest(test = 'Kolmorgov-Smirnov test',
                  stat_name = 'D',
                  stat = test_res[['statistic']],
                  p_value = test_res[['p.value']],
                  n = n)

    }

  }

# Correlating two EDA values ------

#' Correlate two EDA objects.
#'
#' @description Correlates two EDA objects of the same length. For numeric-type EDAs,
#' Pearson, Spearman and Kendall TauB correlations are available. Factor-type analyses can be specifically
#' adressed by Cohen's kappa ('kappa'). EDA objects are converted to the respective type prior to analysis.
#' @param eda_object an EDA object, created by \code{\link{eda}}.
#' @param y an EDA object, created by \code{\link{eda}}.
#' @param type which correlation is to be calculated: Pearson (pearson), Spearman (spearman),
#' Kendall TauB (kendall) or Cohen's Kappa (kappa). Defaults to 'pearson'.
#' @param ci logical, should 95\% confidence intervals be computed?
#' @param .... extra arguments passed to the correlation computing functions.
#' @details Pearson correlation is computed with \code{\link[stats]{cor.test}},
#' Spearman correlation is calculated with \code{\link[DescTools]{SpearmanRho}},
#' Kendall TauB is returned by \code{\link[DescTools]{KendallTauB}}. Kappa is calculated with
#' \code{\link[vcd]{Kappa}}. Correlation failures raise a warning and NULL is returned.
#' @return an eTest object with test statistics, correlation coefficients and p values.
#' @export

  correlation <- function(eda_object, y,
                          type = c('pearson', 'spearman', 'kendall', 'kappa'),
                          ci = TRUE, ...) {

    ## entry control

    if(!is_eda(eda_object)) stop('Please provide a valid EDA class object.', call. = FALSE)
    if(!is_eda(y)) stop('Please provide a valid EDA class object.', call. = FALSE)
    stopifnot(is.logical(ci))

    type <- match.arg(type[1], c('pearson', 'spearman', 'kendall', 'kappa'))

    if(length(eda_object) != length(y)) stop('EDA objects have to be of the same length', call. = FALSE)

    if(type == 'kappa') {

      if(!is.factor(eda_object$value)) eda_object <- as_factor(eda_object)

      if(!is.factor(y$value)) y <- as_factor(y)

      tst_tbl <- table(eda_object$value, y$value)

      test_res <- vcd::Kappa(tst_tbl)

      test_ci <- confint(test_res)

      exda::etest(test = 'Cohen Kappa',
                  stat_name = 'z',
                  stat = test_res[[1]][1]/test_res[[1]][2],
                  estimate_name = 'kappa',
                  estimate = test_res[[1]][1],
                  lower_ci = if(ci) test_ci[1, 1] else NA,
                  upper_ci = if(ci) test_ci[1, 2] else NA,
                  p_value = dnorm(test_res[[1]][1]/test_res[[1]][2]),
                  n = sum(tst_tbl))

    } else {

      eda_object <- as_numeric(eda_object)
      y <- as_numeric(y)

      n <- data.frame(eda_object$value, y$value)
      n <- nrow(dplyr::filter(n, complete.cases(n)))

      if(type == 'pearson') {

        test_res <- stats::cor.test(eda_object$value, y$value, method = 'pearson', conf.level = 0.95, ...)

        exda::etest(test = 'Pearson correlation',
                    stat_name = 't',
                    stat = test_res[['statistic']],
                    estimate_name = 'r',
                    df1 = test_res[['parameter']],
                    estimate = test_res[['estimate']],
                    lower_ci = if(ci) test_res[['conf.int']][1] else NA,
                    upper_ci = if(ci) test_res[['conf.int']][2] else NA,
                    p_value = test_res[['p.value']],
                    n = n)


      } else if(type == 'spearman') {

        test_res <- DescTools::SpearmanRho(eda_object$value, y$value, conf.level = if(ci) 0.95 else NA, ...)
        test_p <- stats::cor.test(eda_object$value, y$value, method = 'spearman', conf.level = 0.95)

        exda::etest(test = 'Spearman correlation',
                    stat_name = 's',
                    stat = test_p[['statistic']],
                    estimate_name = 'rho',
                    estimate = test_res[[1]],
                    lower_ci = if(ci) test_res[[2]] else NA,
                    upper_ci = if(ci) test_res[[3]] else NA,
                    p_value = test_p[['p.value']],
                    n = n)

      } else {

        test_res <- DescTools::KendallTauB(eda_object$value, y$value, conf.level = if(ci) 0.95 else NA, ...)
        test_p <- stats::cor.test(eda_object$value, y$value, method = 'kendall', conf.level = 0.95)

        exda::etest(test = 'Kendall correlation',
                    stat_name = 'z',
                    stat = test_p[['statistic']],
                    estimate_name = 'tau',
                    estimate = test_res[[1]],
                    lower_ci = if(ci) test_res[[2]] else NA,
                    upper_ci = if(ci) test_res[[3]] else NA,
                    p_value = test_p[['p.value']],
                    n = n)

      }

    }

  }

# Variance homogeneity -----

#' Compare variances of EDA objects.
#'
#' @description Compares variances of two or more numeric-type EDA objects with Levene test.
#' Raises an error is factor-type EDAs provided.
#' @details A wrapper around \code{\link[DescTools]{LeveneTest}}.
#' @param ... numeric-type EDA objects, at least two, created by \code{\link{eda}}.
#' @return an eTest object with the the test statistics and p values.
#' @export

  variance <- function(...) {

    ## entry testing

    edas <- rlang::list2(...)

    classes <- purrr::map_lgl(edas, is_eda)

    if(any(!classes)) stop('Please provide valid EDA objects.', call. = FALSE)

    types <- purrr::map_chr(edas, ~.x$type[1])

    if(!all(types == 'numeric')) stop('Numeric-type EDA objects are required.', call. = FALSE)

    ## testing

    tst_tbl <- exda:::convert_eda(...)

    test_res <- DescTools::LeveneTest(variable ~ group,
                                      data = tst_tbl)

    n <- length(tst_tbl[['variable']][!is.na(tst_tbl[['variable']])])

    etest(test = 'Levene',
          stat_name = 'F',
          stat = test_res[['F value']][1],
          df1 = test_res[['Df']][1],
          df2 = test_res[['Df']][2],
          p_value = test_res[['Pr(>F)']][1],
          n = n)

  }

# Variable covariance of two EDA values -----

#' Covariance of two EDA objects.
#'
#' @description Calculates covariance of two EDA objects.
#' For numeric-type EDAs,Pearson, Spearman and Kendall covariances are available.
#' Factor-type EDA objects are converted to the numeric ones prior to analysis.
#' @param eda_object an EDA object, created by \code{\link{eda}}.
#' @param y an EDA object, created by \code{\link{eda}}.
#' @param type which covariance is to be calculated: Pearson (pearson), Spearman (spearman),
#' or Kendall TauB (kendall). Defaults to 'pearson'.
#' @param .... extra arguments passed to the correlation computing function (\code{\link[stats]{cov}}).
#' @return an eTest object with the covariance coefficient.
#' @export

  covariance <- function(eda_object, y, type = c('pearson', 'spearman', 'kendall'), ...) {

    ## entry control

    if(!is_eda(eda_object)) stop('Please provide a valid EDA class object.', call. = FALSE)
    if(!is_eda(y)) stop('Please provide a valid EDA class object.', call. = FALSE)

    type <- match.arg(type[1], c('pearson', 'spearman', 'kendall'))

    if(length(eda_object) != length(y)) stop('EDA objects have to be of the same length', call. = FALSE)

    eda_object <- as_numeric(eda_object)
    y <- as_numeric(eda_object)

    ## testing

    test_res <- cov(eda_object$value, y$value, method = type, ...)

    n <- data.frame(eda_object$value, y$value)
    n <- nrow(dplyr::filter(n, complete.cases(n)))

    exda::etest(test = switch(type,
                              pearson = 'Pearson covariance',
                              spearman = 'Spearman covariance',
                              kendall = 'Kendall covariance'),
                stat_name = 'cov',
                stat = test_res,
                n = n)

  }
