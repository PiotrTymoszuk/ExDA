% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting_utils.R
\name{plot_violin}
\alias{plot_violin}
\title{Violin plot.}
\usage{
plot_violin(
  eda_object,
  signif_digits = 2,
  point_color = "steelblue",
  point_alpha = 0.5,
  point_hjitter = 0.1,
  point_wjitter = 0.1,
  fill_color = "steelblue",
  cust_theme = ggplot2::theme_classic(),
  ...
)
}
\arguments{
\item{eda_object}{an EDA object.}
}
\value{
a ggplot object.
}
\description{
creates a violin plot for a numeric-type EDA object.
}
