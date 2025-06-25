###############################################################################
### Imports:

#' @importFrom ggplot2 ggplot facet_grid
#' @importFrom dplyr %>% distinct across all_of coalesce
NULL
##############################################################################
### Suppress notes due to tidy NSE:

utils::globalVariables(c("present", "fill_color"))

###############################################################################
## Function for plotting missing groups

#' Plot Missing Group Combinations
#'
#' Creates a tile plot highlighting combinations of grouping variables
#' that are expected but missing from the data. Allows for faceting.
#'
#' @param data Data frame.
#' @param x Variable in \code{data} whose values define the x-axis.
#' @param y Variable in \code{data} whose values define the y-axis.
#' @param stratify_by (Optional) Stratification variable. If
#'   supplied, missing data is examined separately for each leves of \code{stratify_by}, and each level gets its own panel.
#' @param for_each (Optional) Additional stratification variable. If supplied,
#'   separate plot windows are created per level of \code{for_each}.
#' @param facet_labeller A \code{labeller} function (e.g. \code{\link[ggplot2]{labeller}}),
#'   or a named list where names match facet variables and values are named
#'   vectors/lists mapping levels to labels (optional).
#' @param title Character string for the plot title. Defaults to "Missing data".
#' @param subtitle Character string for the plot subtitle. Defaults to NULL.
#' @param x_lab Character string for the x-axis label. Defaults to the name of \code{x_var}.
#' @param y_lab Character string for the y-axis label. Defaults to the name of \code{y_var}.
#'
#' @return A ggplot object, or NULL if no missing combinations found.
#' @export
#'
#' @seealso \code{\link[ggplot2]{ggplot}}
#'
#' @examples
#' data("toy_data")
#'
#' # Plot missing data across age and period, stratified by education, for each sex
#' plot_missing_data (data        = toy_data,
#'                    x           = period,
#'                    y           = age,
#'                    stratify_by = education,
#'                    for_each    = sex)
#'
#'

plot_missing_data <- function(data,
                              x,
                              y,
                              stratify_by    = NULL,
                              for_each       = NULL,
                              facet_labeller = NULL,
                              title          = "Missing data",
                              subtitle       = NULL,
                              x_lab          = NULL,
                              y_lab          = NULL) {
  # capture args
  x_q      <- ensym(x)
  y_q      <- ensym(y)
  strat_q  <- enquo(stratify_by)
  each_q   <- enquo(for_each)

  has_strat <- !quo_is_null(strat_q)
  has_each  <- !quo_is_null(each_q)

  # recursion on for_each
  if (has_each) {
    vals <- unique(pull(data, !!each_q))
    plots <- set_names(
      lapply(vals, function(val) {
        sub <- filter(data, !!each_q == val)
        plot_missing_data(
          data           = sub,
          x              = !!x_q,
          y              = !!y_q,
          stratify_by    = !!strat_q,
          for_each       = NULL,
          facet_labeller = facet_labeller,
          title          = title,
          subtitle       = subtitle %||% paste0(as_label(each_q), ": ", val),
          x_lab          = x_lab,
          y_lab          = y_lab
        )
      }),
      vals
    )
    return(plots)
  }

  x_name <- as_label(x_q)
  y_name <- as_label(y_q)
  strat_name <- if (has_strat) as_label(strat_q) else NULL

  if (x_name == y_name) stop("x and y cannot be the same")

  # determine expected grid
  across_vars <- c(x_name, y_name)
  if (has_strat) across_vars <- c(across_vars, strat_name)

  eg <- expected_groups(data, across_vars)
  expected_df   <- eg$grid
  expected_list <- eg$levels

  actual_groups <- data %>% distinct(across(all_of(across_vars)))
  plot_df <- expected_df %>%
    left_join(actual_groups %>% mutate(present = TRUE), by = across_vars) %>%
    mutate(present = coalesce(present, FALSE),
           fill_color = ifelse(present, "white", "red"))

  if (all(plot_df$present)) {
    message("No missing data found for the specified variables.")
    return(NULL)
  }

  for (col in c(x_name, y_name)) {
    original_class <- class(data[[col]])
    if (original_class %in% c("integer", "numeric")) {
      plot_df[[col]] <- as.numeric(plot_df[[col]])
    } else {
      plot_df[[col]] <- factor(plot_df[[col]], levels = expected_list[[col]])
    }
  }

  # build base plot
  p <- ggplot(plot_df,
              aes(x = .data[[x_name]],
                  y = .data[[y_name]],
                  fill = fill_color)) +
    geom_tile(color = "black") +
    scale_fill_identity() +
    labs(
      title    = title,
      subtitle = subtitle,
      x        = x_lab %||% x_name,
      y        = y_lab %||% y_name
    ) +
    theme(
      axis.text.x     = element_text(angle = 45, margin = margin(t=5)),
      plot.title      = element_text(hjust = 0.5),
      plot.subtitle   = element_text(hjust=0.5),
      panel.border    = element_rect(color = "black", fill = NA, linewidth = 1),
      strip.background= element_rect(fill = "grey90", color = "black")
    )

  is_x_numeric <- is.numeric(data[[x_name]])
  is_y_numeric <- is.numeric(data[[y_name]])

  if (is_x_numeric) {
    p <- p + scale_x_continuous(breaks = pretty(data[[x_name]]), expand = c(0,0))
  } else {
    p <- p + scale_x_discrete(expand = c(0,0))
  }

  if (is_y_numeric) {
    p <- p + scale_y_continuous(breaks = pretty(data[[y_name]]), expand = c(0,0))
  } else {
    p <- p + scale_y_discrete(expand = c(0,0))
  }


  # facet by stratify_by
  if (has_strat) {
    if (!is.null(facet_labeller)) {
      p <- p + facet_wrap(vars(!!strat_q), labeller = facet_labeller)
    } else {
      p <- p + facet_wrap(vars(!!strat_q))
    }
  }

  return(p)
}
