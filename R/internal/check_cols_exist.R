#' Check if a set of columns is missing from a data frame. For use in \code{\link{aggregate_data}}.
#'
#' @param cols String or vector of strings that is the name of the columns.
#' @param df Data frame
#'
#' @return Nothing. Casts an error message if any of the columns are missing.
#' @keywords internal


check_cols_exist <- function(cols, data) {
  if (!is.null(cols)) {
    missing_cols <- setdiff(cols, names(data))
    if (length(missing_cols) > 0) {
      stop(paste0("These columns are missing from the data: ",
                  paste(missing_cols, collapse = ", ")))
    }
  }
}
