% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/internal-gaussian_aggregation.R
\name{agaussian}
\alias{agaussian}
\title{Aggregate Gaussian data}
\usage{
agaussian(data, precision.scale = NULL)
}
\arguments{
\item{data}{Gaussian data, must be a numeric vector.}

\item{precision.scale}{Scales for the precision of each Gaussian observation. \cr
\strong{Defaults to vector of 1s (no scaling).}}
}
\value{
Aggregated Gaussian data.
}
\description{
Aggregates Gaussian data using sufficient statistics for Gaussian samples.
For a sample \eqn{\boldsymbol{y} = \{y_1, \dots, y_n\}} with \eqn{y_i \sim \mathcal{N(\mu, (s_i \tau)^{-1})}}, the sample is aggregated into the sufficient statistic \cr \cr
\eqn{(v, \frac{1}{2} \sum_{i=1}^n \log(s_i), m, n, \bar{y})}, \cr \cr
with \cr \cr
\eqn{m = \sum_{i=1}^n s_i} \eqn{\quad}
\eqn{\bar{y} = \frac{1}{m} \sum{i=1}^n s_iy_i} \eqn{\quad}
\eqn{v = \frac{1}{m} \sum_{i=1}^n s_i y_i^2 - \bar{y}^2}. \cr \cr
For a short derivation of the sufficient statistic, attach the \code{R-INLA} package (\code{library(INLA)}) and run \code{inla.doc("agaussian")}.
}
\keyword{internal}
