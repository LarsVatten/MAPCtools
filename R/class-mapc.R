# S3 methods for class mapc

#' @export
print.mapc <- function(x, ...) {
  stopifnot(inherits(x, "mapc"))

  cat("MAPC model fit\n")
  cat("Model:", x$apc_format, "\n")
  cat(sprintf("Total CPU time used: %.2f s | Elapsed time: %.2f s\n",
              x$time.used["user.self"] + x$time.used["sys.self"],
              x$time.used["elapsed"]), "\n")
  cat(" - Number of fixed effects estimated:", nrow(x$model_fit$summary.fixed), "\n")
  cat(" - Number of random effects estimated:", length(x$model_fit$summary.random), "\n")
  cat(" - Number of hyperparameters estimated:", nrow(x$model_fit$summary.hyperpar), "\n")
  cat(" - Number of linear combinations estimated:", nrow(x$model_fit$summary.lincomb.derived), "\n")
  cat(" - DIC score:", x$model_fit$dic$dic, "\n")
  cat(" - WAIC score:", x$model_fit$waic$waic, "\n")
  cat(" - Log-score:", -mean(log(x$model_fit$cpo$cpo)), "\n\n")
  cat("Try summary() for posterior summaries, or plot() for visual results.")
  invisible(x)
}

#' @export
summary.mapc <- function(object, ...) {
  stopifnot(inherits(object, "mapc"))

  cat("==== MAPC summary ====\n\n")

  cat("--- Fixed effects ---\n")
  print(object$model_fit$summary.fixed)
  cat("---------------------\n\n")

  cat("--- Random effects---\n")
  cat(paste(names(object$model_fit$summary.random), collapse=", "), "\n\n")
  cat("View each random effect's posterior summary with ...$model_fit$summary.random$<name_of_effect>.\n\n")
  cat("---------------------\n\n")

  cat("--- Hyperparameters ---\n")
  print(object$model_fit$summary.hyperpar)
  cat("----------------------\n\n")

  cat("--- Linear combinations ---\n")
  print(object$model_fit$summary.lincomb.derived)

  invisible(object)
}

#' @export
plot.mapc <- function(x, which = c("age","period","cohort"), ...) {
  stopifnot(inherits(x, "mapc"))

  for (eff in which) {
    if(!is.null(x$plots[[eff]])) {
      print(x$plots[[eff]])
    }
  }
  invisible(x)
}
