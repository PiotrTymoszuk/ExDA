% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{stat_extract}
\alias{stat_extract}
\title{A universal statistic extractor.}
\usage{
stat_extract(eda_object, fun, stat_name, plain = FALSE, ...)
}
\arguments{
\item{eda_object}{an eda object.}

\item{fun}{a statistic calculation function, should return a single numeric.}

\item{stat_name}{name of the statistic included in the output data frame.}

\item{plain}{logical, should the output be coerced to a single vector?}

\item{...}{extra argument passed to fun.}
}
\value{
a tibble or a numeric value.
}
\description{
calculates the requested statistic for a numeric vector.
NAs are removed prior to analysis
}
