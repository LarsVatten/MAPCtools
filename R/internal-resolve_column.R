#' @importFrom rlang sym enquo is_string is_symbol is_vector is_quosure as_name
NULL

#' Helper for evaluating column names, strings, or self-contained vectors
#'
#' Accepts:
#' - Unquoted column names (tidy-eval style)
#' - Quoted column names (as strings)
#' - Self-contained vectors
#'
#' @param arg A column name (unquoted or string) or a vector.
#'
#' @return A quosure, symbol, or literal vector depending on input.
#'
#' @keywords internal
resolve_column <- function(arg) {
  resolved <- NULL

  if (is_string(arg)) {
    resolved <- sym(arg)
  } else if (is_symbol(arg) || is_quosure(arg)) {
    resolved <- enquo(arg)
  } else if (is_vector(arg)) {
    return(arg)  # literal vector — skip validation
  } else {
    resolved <- enquo(arg)
  }

  return(resolved)
}



