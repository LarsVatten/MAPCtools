##############################################################################
## Aggregated binomial

#' Aggregate binomial data
#'
#' Aggregates binomial data using sufficient statistics for binomial samples.
#' For a sample \eqn{\boldsymbol{y} = \{y_1, \dots, y_n\}} with \eqn{y_i \sim \text{Bin}(n, p)}, the sample is aggregated into the sufficient statistic \cr \cr
#' \eqn{s = \sum_{i=1}^n y_i}.
#'
#' @param data Binomial data vector.
#' @return Aggregated binomial data.
#' @keywords internal

abinomial <- function(data) {
  if (!is.numeric(data) || !is.atomic(data)) {
    stop("'data' must be a numeric vector.")
  }
  if (anyNA(data)) {
    stop("'data' contains missing values (NA). Please remove or impute them.")
  }
  if (any(data < 0, na.rm = TRUE)) {
    stop("'data' contains negative values. Binomial counts must be non-negative.")
  }
  return(data.frame(s=sum(data)))
}

#' Aggregate binomial data
#'
#' Aggregates binomial data into sufficient statistics for binomial samples. Uses \code{\link{abinomial}}.
#'
#' @param df Data frame with binomial data column.
#' @param col_name Binomial data column.
#' @return Aggregated binomial data column.
#' @keywords internal

# Function to extract and rename the sufficient statistics from the abinomial function
binomial_aggregator <- function(df, col_name) {
  if (!is.data.frame(df)) {
    stop("'df' must be a data frame.")
  }
  if (!is.character(col_name) || length(col_name) != 1) {
    stop("'col_name' must be a character string, naming a column in the data frame.")
  }
  if (!(col_name %in% names(df))) {
    stop(paste0("Column '", col_name, "' not found in the data frame."))
  }
  stats <- abinomial(df[[col_name]])
  return(stats)
}
