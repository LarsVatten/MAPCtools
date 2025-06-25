# S3 methods for class all_mapc

#' @export
print.all_mapc <- function(x, ...) {
  stopifnot(inherits(x, "all_mapc"))

  mapc_names <- names(x)[vapply(x, function(e) inherits(e, "mapc"), logical(1))]
  n_mapc <- length(mapc_names)

  cat("Total number of MAPC models fit:", n_mapc, "\n")
  cat(sprintf("Total CPU time used: %.2f s | Elapsed time: %.2f s\n",
              x$time.used["user.self"] + x$time.used["sys.self"],
              x$time.used["elapsed"]), "\n")

  cat("=============== All model fits: ===============\n\n")
  for (name in mapc_names) {
    cat("---", name, "---\n")
    cat(sprintf("Total CPU time used: %.2f s | Elapsed time: %.2f s\n",
                x[[name]]$time.used["user.self"] + x[[name]]$time.used["sys.self"],
                x[[name]]$time.used["elapsed"]), "\n")
    cat(" - Number of fixed effects estimated:", nrow(x[[name]]$model_fit$summary.fixed), "\n")
    cat(" - Number of random effects estimated:", length(x[[name]]$model_fit$summary.random), "\n")
    cat(" - Number of hyperparameters estimated:", nrow(x[[name]]$model_fit$summary.hyperpar), "\n")
    cat(" - Number of linear combinations estimated:", nrow(x[[name]]$model_fit$summary.lincomb.derived), "\n")
    cat(" - DIC score:", x[[name]]$model_fit$dic$dic, "\n")
    cat(" - WAIC score:", x[[name]]$model_fit$waic$waic, "\n")
    cat(" - Log-score:", -mean(log(x[[name]]$model_fit$cpo$cpo)), "\n\n")
  }
  invisible(x)
}

#' @export
summary.all_mapc <- function(object, ...) {
  stopifnot(inherits(object, "all_mapc"))

  mapc_names <- names(object)[vapply(object, function(e) inherits(e, "mapc"), logical(1))]
  n_mapc <- length(mapc_names)

  cat("Total number of MAPC models fit:", n_mapc, "\n\n")

  cat("======== Summary of all MAPC model fits ========\n\n")
  for (name in mapc_names) {
    cat("______________________________________________________________\n")
    cat("Model:", name, "\n\n")
    summary(object[[name]])
    cat("\n\n")
  }
  invisible(object)
}

#' @export
plot.all_mapc <- function(x, ...) {
  stopifnot(inherits(x, "all_mapc"))

  if(!is.null(x$model_selection)) {
    for (p in x$model_selection) {
      print(p)
    }
  } else{
    cat("There is no summary of model selection criteria available for these models.")
  }

  invisible(x)
}
