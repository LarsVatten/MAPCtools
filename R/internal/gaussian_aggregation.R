##############################################################################
## Agaussian

#' Aggregate Gaussian data
#'
#' Aggregates Gaussian data using sufficient statistics for Gaussian samples.
#' For a sample \eqn{\boldsymbol{y} = \{y_1, \dots, y_n\}} with \eqn{y_i \sim \mathcal{N(\mu, (s_i \tau)^{-1})}}, the sample is aggregated into the sufficient statistic \cr \cr
#' \eqn{(v, \frac{1}{2} \sum_{i=1}^n \log(s_i), m, n, \bar{y})}, \cr \cr
#' with \cr \cr
#' \eqn{m = \sum_{i=1}^n s_i} \eqn{\quad}
#' \eqn{\bar{y} = \frac{1}{m} \sum{i=1}^n s_iy_i} \eqn{\quad}
#' \eqn{v = \frac{1}{m} \sum_{i=1}^n s_i y_i^2 - \bar{y}^2}. \cr \cr
#' For a short derivation of the sufficient statistic, attach the \code{R-INLA} package (\code{library(INLA)}) and run \code{inla.doc("agaussian")}.
#'
#' @param data Gaussian data, must be a numeric vector.
#' @param precision.scale Scales for the precision of each Gaussian observation. \cr
#'    **Defaults to vector of 1s (no scaling).**
#'
#' @return Aggregated Gaussian data.
#' @keywords internal

agaussian <- function(data, precision.scale=NULL) {
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
    if (!is.vector(precision.scale)) {
      stop("'precision.scale' must be a vector, with the same length as the data vector.")
    }
    if (length(precision.scale) != n) {
      stop("'precision.scale' must be the same length as 'data'")
    }
  }
  m = sum(precision.scale)
  ybar = 1/m * sum(precision.scale * data)
  v = 1/m * sum(precision.scale * data^2) - ybar^2
  aggregated.data <- data.frame(s1 = v, s2 = 0.5*sum(log(precision.scale)), s3 = m, s4 = length(data), s5 = ybar)
  names(aggregated.data) <- c("Y1", "Y2", "Y3", "Y4", "Y5")
  return(aggregated.data)
}

#' Aggregate Gaussian data
#'
#' Aggregates numerical data frame column using sufficient statistics for Gaussian samples, into an \code{inla.mdata} object compatible with the\code{agaussian} likelihood in \code{INLA}.
#' Uses \code{\link{agaussian}}.
#'
#' @param df Data frame with a Gaussian column.
#' @param col_name Name of the Gaussian column.
#' @param precision.scale Scales for the precision of each Gaussian observation. \cr
#'    **Defaults to vector of 1s (no scaling).**
#'
#' @return Aggregated Gaussian column as an \code{inla.mdata} object.
#'
#' @keywords internal

gaussian_aggregator <- function(df, col_name, precision.scale = NULL) {
  if (!is.data.frame(df)) {
    stop("'df' must be a data frame.")
  }
  if (!is.character(col_name) || length(col_name) != 1) {
    stop("'col_name' must be a character string, naming a column in the data frame.")
  }
  if (!(col_name %in% names(df))) {
    stop(paste0("Column '", col_name, "' not found in the data frame."))
  }
  stats <- agaussian(df[[col_name]], precision.scale = precision.scale)
  return(inla.mdata(stats))
}
