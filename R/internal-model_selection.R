#############################################################################
# Imports
#' @import ggplot2
#' @importFrom ggpubr as_ggplot
#' @importFrom grid unit
#' @importFrom gridExtra tableGrob ttheme_default
NULL
#############################################################################
# Suppress warnings due to tidy NSE:
utils::globalVariables(c("model", "value", "stringsAsFactors"))
############################################################################

#' Make a table of model fit scores
#'
#' Generates a table summarizing the provided model fit scores.
#'
#' @param dic_scores Named list of DIC scores. Names correspond to the model.
#' @param waic_scores Named list of WAIC scores. Names correspond to the model.
#' @param log_scores Named list of log-scores scores (derived from CPO scores). Names correspond to the model.
#'
#' @return A table summarizing the model fit scores, as a \code{\link[ggplot2]{ggplot}} object.
#'
#' @keywords internal
model_selection_criteria_table <- function(dic_scores = NULL,
                                           waic_scores = NULL,
                                           log_scores  = NULL) {

  score_list <- list(
    DIC        = dic_scores,
    WAIC       = waic_scores,
    `Log-score`= log_scores
  )
  nonnull <- score_list[!vapply(score_list, is.null, logical(1))]
  if (length(nonnull) == 0) {
    stop("At least one of 'dic_scores', 'waic_scores' or 'log_scores' must be provided.")
  }

  model_sets <- lapply(nonnull, names)
  if (any(vapply(model_sets, is.null, logical(1)))) {
    stop("All non-NULL score-lists must be named, e.g. list(aPc=..., APC=..., ...).")
  }
  model_names <- unique(unlist(model_sets, use.names = FALSE))

  df_tab <- data.frame(
    Model = model_names,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  for (crit in names(nonnull)) {
    vals <- unlist(nonnull[[crit]])
    df_tab[[crit]] <- round(vals[model_names], 3)
  }

  n_rows <- nrow(df_tab)
  n_cols <- ncol(df_tab)
  blue_odd   <- "lemonchiffon1"
  blue_even  <- "lemonchiffon2"
  green_odd  <- "honeydew1"
  green_even <- "honeydew2"
  header_fill <- c("lemonchiffon3", rep("honeydew3", n_cols - 1))

  body_fill <- matrix(NA_character_, nrow = n_rows, ncol = n_cols)
  for (i in seq_len(n_rows)) {
    is_odd <- (i %% 2 == 1)
    for (j in seq_len(n_cols)) {
      body_fill[i,j] <- if (j==1) {
        if (is_odd) blue_odd else blue_even
      } else {
        if (is_odd) green_odd else green_even
      }
    }
  }

  th <- ttheme_default()
  th$core$bg_params$fill      <- body_fill
  th$core$fg_params$col       <- "black"
  th$colhead$bg_params$fill   <- header_fill
  th$colhead$fg_params$col    <- "black"

  tg <- tableGrob(df_tab, rows = NULL, theme = th)

  for (i in seq_along(tg$widths))  tg$widths[[i]] <- unit(1, "null")
  for (i in seq_along(tg$heights)) tg$heights[[i]] <- unit(1, "null")

  tg <- as_ggplot(tg)

  tg <- tg + theme(plot.margin = margin(50, 50, 50, 50))

  tg
}

################################################################################
#' Make a plot of model fit scores
#'
#' Makes a plot summarizing the model fit scores of a set of models,
#' and ranks the models based on their score.
#'
#' @param scores Named list of model fit scores. Names correspond to the model.
#' @param title (Optional) Plot title; defaults to the name of \code{scores}.
#' @param alphabetic (Optional) TRUE/FALSE, indicating if models are to be sorted alphabetically along the horizontal axis. Defaults to \code{FALSE}.
#'
#' @details
#' The function assumes lower scores are preferable, so the models with the lowest scores are ranked higher.
#'
#'
#' @return Plot summarizing the model fit scores, and ranking the models based on their scores.
#'
#' @keywords internal
plot_model_selection_criteria <- function(scores, title = NULL, alphabetic = FALSE) {

  if (is.null(names(scores))) {
    stop("'scores' must be a named numeric vector or list, e.g. c(apC=..., Apc=...)")
  }

  vals <- unlist(scores)
  df <- data.frame(
    model = names(vals),
    value = vals,
    stringsAsFactors = FALSE
  )

  if (is.null(title)) {
    title <- deparse(substitute(scores))
  }

  if (alphabetic) {
    df$model <- factor(df$model, levels = sort(unique(df$model)))
  } else {
    df$model <- factor(df$model, levels = unique(df$model))
  }

  df$rank <- as.factor(rank(df$value, ties.method = "first"))

  ggplot(df, aes(x = model, y = value, group = 1)) +
    geom_line(color = "black") +
    geom_point() +
    geom_text(aes(label = rank), vjust = -1, show.legend = FALSE) +
    labs(title = title, x = "Model", y = title, color = "Rank") +
    theme(
      plot.title      = element_text(hjust = 0.5),
    ) +
    scale_y_continuous(expand = expansion(mult = c(0.1, 0.25)))
}
