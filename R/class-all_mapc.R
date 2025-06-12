## TO COME: MAKE A S3 CLASS OBJECT WITH DEFINED PRINT ETC.

#' @export
print.all_mapc <- function(x, ...) {
  cat("All MAPC model fits:")
  for (name in names(x)) {
    cat("\n\n---", name, "---\n")
    print(x[[name]])
  }
  invisible(x)
}

#' @export
summary.all_mapc <- function(object, ...) {
  cat("======== Summary of all MAPC model fits ========\n\n")
  for (name in names(object)) {
    cat("\n______________________________________________________________\n")
    cat("Model:", name, "\n\n")
    summary(object[[name]])
  }
  invisible(object)
}

#' @export
plot.all_mapc <- function(x, which = NULL, effects = c("age","period","cohort"), ...) {
  models_to_plot <- if (is.null(which)) names(x) else which

  for (name in models_to_plot) {
    plot(x[[name]], which = effects, ...)
  }

  invisible(x)
}
