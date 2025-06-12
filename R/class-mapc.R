## TO COME: MAKE A S3 CLASS OBJECT WITH DEFINED PRINT ETC.

#' @export
print.mapc <- function(x, ...) {
  cat("MAPC model fit\n")
  cat("Model:", x$apc_format, "\n")
  cat(" - Total CPU time used:", x$model_fit$cpu.used[[4]], "seconds.\n")
  cat(" - Number of fixed effects estimated:", nrow(x$model_fit$summary.fixed), "\n")
  cat(" - Number of random effects estimated:", length(x$model_fit$summary.random), "\n")
  cat(" - Number of hyperparameters estimated:", nrow(x$model_fit$summary.hyperpar), "\n")
  cat(" - Number of linear combinations estimated:", nrow(x$model_fit$summary.lincomb.derived), "\n")
  cat(" - DIC score:", x$model_fit$dic$dic, "\n")
  cat(" - WAIC score:", x$model_fit$waic$waic, "\n")
  cat("Try summary() for posterior summaries, or plot() for visual results.")
  invisible(x)
}

#' @export
summary.mapc <- function(object, ...) {
  cat("==== MAPC summary ====\n\n")

  cat("--- Fixed effects ---\n")
  print(object$model_fit$summary.fixed)
  cat("---------------------\n\n")

  cat("--- Random effects---\n")
  cat(paste(names(object$model_fit$summary.random), collapse=", "), "\n\n")
  cat("View each random effect's posterior summary with ...$model_fit$summary.random$<name_of_effect>.\n\n")
  cat("---------------------\n\n")

  cat("--- Hyperparamters ---\n")
  print(object$model_fit$summary.hyperpar)
  cat("----------------------\n\n")

  cat("--- Linear combinations ---\n")
  print(object$model_fit$summary.lincomb.derived)

  invisible(object)
}

#' @export
plot.mapc <- function(x, which = c("age","period","cohort"), ...) {
  for (eff in which) {
    if(!is.null(x$plots[[eff]])) {
      print(x$plots[[eff]])
    }
  }
  invisible(x)
}
