#' Find expected groups based on distinct values across a set of variables
#'
#' Given a data frame and a set of discrete (or factor) variables,
#' returns all combinations of their observed levels **and** the list of levels.
#'
#' @param df A data frame whose columns you want to examine.
#' @param vars Character vector of column names in `df` to use.
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{grid}{A data.frame where each row is one combination of the variable levels
#'               (equivalent to what \code{expand.grid()} would produce).}
#'   \item{levels}{A named list; for each variable in `vars` it gives the sorted
#'                 unique values (or factor levels) observed in `df`.}
#' }
#'
#' @keywords internal

expected_groups <- function(df, vars) {
  levels_list <- lapply(vars, function(col) {
    col_data <- df[[col]]
    if (is.factor(col_data)) levels(col_data) else sort(unique(col_data))
  })
  names(levels_list) <- vars

  combo_df <- do.call(expand.grid, c(levels_list, list(stringsAsFactors = FALSE)))

  list(grid   = combo_df,
       levels = levels_list)
}
