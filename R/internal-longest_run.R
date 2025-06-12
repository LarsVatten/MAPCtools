#' @title Function for finding longest consecutive run of non-missing indices
#'
#' @param x A vector of indices for which to find the longest consecutive run of indices
#'
#' @return A named list, containing arguments:
#' \describe{
#'  \item{\code{from}}{The first index of the longest run.}
#'  \item{\code{to}}{The last index of the longest run.}
#'  \item{\code{step}}{The stepsize of the longest run.}
#'}
#' @keywords internal

longest_run <- function(x) {
  x0 <- sort(unique(x))
  if (length(x0) < 2) {
    return(list(from = x0[1], to = x0[1], step = NA_real_))
  }
  d    <- diff(x0)
  step <- min(d)
  run_start <- c(TRUE, d != step)
  grp       <- cumsum(run_start)
  runs  <- split(x0, grp)
  best  <- runs[[which.max(lengths(runs))]]
  list(from = min(best), to = max(best), step = step)
}
