% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/export-MissingData.R
\name{plot_missing_data}
\alias{plot_missing_data}
\title{Plot Missing Group Combinations}
\usage{
plot_missing_data(
  data,
  x_var,
  y_var,
  row_var = NULL,
  col_var = NULL,
  facet_labeller = NULL,
  x_breaks = NULL,
  y_breaks = NULL,
  title = "Missing data",
  subtitle = NULL,
  x_lab = NULL,
  y_lab = NULL
)
}
\arguments{
\item{data}{Data frame.}

\item{x_var}{Variable for the x-axis.}

\item{y_var}{Variable for the y-axis.}

\item{row_var}{Grouping variable in \code{data} for facet rows (optional).}

\item{col_var}{Grouping variable for facet columns (optional).}

\item{facet_labeller}{A \code{labeller} function (e.g. \code{\link[ggplot2]{labeller}}),
or a named list where names match facet variables and values are named
vectors/lists mapping levels to labels (optional).}

\item{x_breaks}{Vector of values for x-axis breaks (optional).}

\item{y_breaks}{Vector of values for y-axis breaks (optional).}

\item{title}{Character string for the plot title. Defaults to "Missing data".}

\item{subtitle}{Character string for the plot subtitle. Defaults to NULL.}

\item{x_lab}{Character string for the x-axis label. Defaults to the name of \code{x_var}.}

\item{y_lab}{Character string for the y-axis label. Defaults to the name of \code{y_var}.}
}
\value{
A ggplot object, or NULL if no missing combinations found.
}
\description{
Creates a tile plot highlighting combinations of grouping variables
that are expected but missing from the data. Allows for faceting.
}
