#' Find expected groups based on distinct values across a set of variables
#'
#' Given a data frame and a set of discrete (or factor) variables,
#' returns all combinations of their observed levels **and** the list of levels.
#'
#' @param data A data frame whose columns you want to examine.
#' @param vars Character vector of column names in \code{data} to use.
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{grid}{A data.frame where each row is one combination of the variable levels
#'               (equivalent to what \code{\link{expand.grid}} would produce).}
#'   \item{levels}{A named list; for each variable in \code{vars} it gives the sorted
#'                 unique values (or factor levels) observed in \code{data}.}
#' }
#'
#' @keywords internal

expected_groups <- function(data, vars) {
  levels_list <- lapply(vars, function(col) {
    col_data <- data[[col]]
    if (is.factor(col_data)) {
      # Use existing levels even if not present in the data
      levels(col_data)
    } else {
      sort(unique(col_data))
    }
  })
  names(levels_list) <- vars

  combo_df <- expand.grid(levels_list, stringsAsFactors = FALSE)

  list(
    grid   = combo_df,
    levels = levels_list
  )
}
