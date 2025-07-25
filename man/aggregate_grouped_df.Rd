% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/export-DataPreprocessing.R
\name{aggregate_grouped_df}
\alias{aggregate_grouped_df}
\title{Aggregate grouped data using aggregate_df}
\usage{
aggregate_grouped_df(
  data,
  by,
  gaussian = NULL,
  gaussian.precision.scales = NULL,
  binomial = NULL
)
}
\arguments{
\item{data}{Data frame to be grouped and aggregated.}

\item{by}{Columns in \code{data} for which to group data by.}

\item{gaussian}{Gaussian columns in \code{grouped_data} to be aggregated. \cr
\strong{Defaults to \code{NULL} (optional).}}

\item{gaussian.precision.scales}{Scales for the precision of Gaussian observations. \cr
See \code{\link{aggregate_df}} documentation for format details, and \code{agaussian} in \code{INLA} for more details.
\strong{Defaults to NULL.}}

\item{binomial}{Binomial columns in \code{grouped_data} to be aggregated. \cr
\strong{Defaults to \code{NULL} (optional).}}
}
\value{
Aggregated data frame (tibble), with one row per group, containing
grouping variables, count \code{n} per group, and aggregated list-columns for
specified variables as returned by \code{\link{aggregate_df}}.
}
\description{
Aggregates a grouped data frame into summarizing statistics within groups by
applying the \code{\link{aggregate_df}} function to each group.
Aggregation is performed according to sufficient statistics for the specified
distribution of the columns to be aggregated.
}
\keyword{internal}
