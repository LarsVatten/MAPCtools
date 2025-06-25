##############################################################################
## Agaussian

#' Aggregate Gaussian data
#'
#' Aggregates Gaussian data using sufficient statistics for Gaussian samples.
#' For a sample \eqn{\boldsymbol{y} = \{y_1, \dots, y_n\}} with \eqn{y_i \sim \mathcal{N(\mu, (s_i \tau)^{-1})}}, \eqn{i=1, \dots, n}, the sample is aggregated into the sufficient statistic \cr \cr
#' \eqn{(v, \frac{1}{2} \sum_{i=1}^n \log(s_i), m, n, \bar{y})}, \cr \cr
#' with \cr \cr
#' \eqn{m = \sum_{i=1}^n s_i} \eqn{\quad}
#' \eqn{\bar{y} = \frac{1}{m} \sum{i=1}^n s_iy_i} \eqn{\quad}
#' \eqn{v = \frac{1}{m} \sum_{i=1}^n s_i y_i^2 - \bar{y}^2}. \cr \cr
#' For a short derivation of the sufficient statistic, attach the \pkg{INLA} package (\code{library(INLA)}) and run \code{inla.doc("agaussian")}.
#'
#' @param data Gaussian data, must be a numeric vector.
#' @param precision.scale Scales for the precision of each Gaussian observation. \cr
#'    **Defaults to vector of 1s (no scaling).**
#'
#' @return Aggregated Gaussian data, in an \code{\link[INLA]{inla.mdata}} object, which is compatible with the \code{agaussian} family in \pkg{INLA}.
#' @keywords internal
agaussian <- function(data, precision.scale = NULL) {
  if (!is.numeric(data) || !is.atomic(data)) {
    stop("'data' must be a numeric vector.")
  }
  if (anyNA(data)) {
    stop("'data' contains missing values. Please remove or impute them.")
  }

  n <- length(data)
  if (is.null(precision.scale)) {
    precision.scale <- rep(1, n)
  } else {

    precision.scale <- as.numeric(precision.scale)

    if (!is.vector(precision.scale) || !is.atomic(precision.scale)) {
      stop("'precision.scale' must be an atomic vector.")
    }
    if (length(precision.scale) != n) {
      stop("'precision.scale' must be the same length as 'data'")
    }
  }
  m = sum(precision.scale)
  ybar = 1/m * sum(precision.scale * data)
  v = 1/m * sum(precision.scale * data^2) - ybar^2
  aggregated.data <- data.frame(Y1 = v, Y2 = 0.5*sum(log(precision.scale)), Y3 = m, Y4 = n, Y5 = ybar)
  return(aggregated.data)
}

#' Aggregate Gaussian data
#'
#' Aggregates numerical data frame column using sufficient statistics for Gaussian samples, into an \code{inla.mdata} object compatible with the\code{agaussian} likelihood in \code{INLA}.
#' Uses \code{\link{agaussian}}.
#'
#' @param y Gaussian column.
#' @param precision.scale Scales for the precision of each Gaussian observation. \cr
#'    **Defaults to vector of 1s (no scaling).**
#'
#' @return Aggregated Gaussian column as an \code{\link[INLA]{inla.mdata}} object.
#'
#' @keywords internal

gaussian_aggregator <- function(y, precision.scale = NULL) {
  if (!requireNamespace("INLA", quietly = TRUE)) {
    stop("This function requires the INLA package.")
  }
  if (!is.numeric(y)) {
    stop("'y' must be a numeric vector.")
  }
  stats <- agaussian(y, precision.scale = precision.scale)
  return(INLA::inla.mdata(stats))
}
