##############################################################################
# Suppress warnings due to tidy NSE:

utils::globalVariables(c(
  "bin","time_bin", "x_value", "group", "mean_y", "overlay_group",
  "mean_val", "mean_response", "data2", "strat"
))
###############################################################################
### Imports:

#' @import ggplot2
#' @importFrom dplyr %>% distinct across all_of group_by summarise pull filter count rename mutate
#' @importFrom dplyr filter count rename mutate transmute left_join
#' @importFrom utils tail head
#' @importFrom stats reformulate
#' @importFrom rlang ensym enquo quo_is_null as_label as_name sym
#' @importFrom scales rescale squish
#' @importFrom viridis viridis
#' @importFrom purrr set_names
#' @importFrom stringr str_to_title
NULL
##############################################################################

#' Plot counts of observations across a single variable, optionally stratified
#'
#' Computes the number of observations at each value of a specified variable and
#' creates a line plot of these counts using \pkg{ggplot2}. If a stratification variable
#' is provided, counts are calculated per strata and plotted as separate colored lines.
#' If an additional stratification variable is provided, separate plot windows are created for each level.
#'
#' @param data Data frame containing all input variables.
#' @param x Variable in \code{data} whose values define the x-axis for counts.
#' @param stratify_by (Optional) Stratification variable. If
#'   supplied, counts are computed for each combination of \code{x} and
#'   \code{stratify_by}, and separate lines are drawn per level of \code{stratify_by}.
#' @param for_each (Optional) Additional stratification variable. If supplied,
#'   separate plot windows are created per level of \code{for_each}.
#' @param title (Optional) Plot title; defaults to \code{"Observation counts"}.
#' @param subtitle (Optional) Plot subtitle; defaults to \code{NULL} if for \code{for_each} is \code{NULL},
#' defaults to \code{<name of for_each>: <level of for_each>} for each plot window if \code{for_each} is supplied.
#' @param legend_title (Optional) Legend title; defaults to name of \code{stratify_var} if it is supplied.
#' @param x_lab (Optional) Label for the x-axis; defaults to the name of \code{x}.
#' @param y_lab (Optional) Label for the y-axis; defaults to the name of \code{y}.
#' @param viridis_color_option (Optional) Option for color gradient; defaults to "D".
#'   Options are "A", "B", "C", "D", E", "F", "G", "H".
#'   See \pkg{viridis} for information, or experiment yourself.
#'
#' @return A \link[ggplot2]{ggplot} object displaying counts across the variable supplied in \code{x},
#'   optionally stratified by \code{stratify_by}. If \code{for_each} is supplied, separate plots are created  in separate windows for each level.
#'   Visuals can be modified with \pkg{ggplot2}.
#'
#' @export
#'
#' @seealso \code{\link{plot_counts_2D}}, \code{\link{plot_binned_counts}},
#'          \code{\link{plot_counts_with_mean}}, \code{\link[ggplot2]{ggplot}}
#'
#' @examples
#' data("toy_data")
#' # Counts by age
#' plot_counts_1D(toy_data, x = age)
#' # Counts by age, stratified by education level
#' plot_counts_1D(toy_data, x = age,
#'                stratify_by = education)
#' # Count by age, stratified by education level, for each sex
#' plot_counts_1D(toy_data, x = age,
#'                stratify_by = education, for_each = sex)
plot_counts_1D <- function(data, x,
                           stratify_by = NULL,
                           for_each    = NULL,
                           title       = "Observation counts",
                           subtitle    = NULL,
                           legend_title = NULL,
                           x_lab        = NULL,
                           y_lab       = NULL,
                           viridis_color_option = "D") {

  x_sym        <- ensym(x)
  strat_q    <- enquo(stratify_by)
  each_q     <- enquo(for_each)

  if (!quo_is_null(strat_q)) {
    strat_name <- as_name(strat_q)
    if (!(strat_name %in% names(data))) {
      stop(paste0("stratify_by column '", strat_name, "' not found in data."))
    }
    if (!is.character(data[[strat_name]]) && !is.factor(data[[strat_name]])) {
      stop(paste0("stratify_by column '", strat_name, "' must be character or factor, not ", typeof(data[[strat_name]]), "."))
    }
  }

  if (!quo_is_null(each_q)) {
    each_name <- as_name(each_q)
    if (!(each_name %in% names(data))) {
      stop(paste0("for_each column '", each_name, "' not found in data."))
    }
    if (!is.character(data[[each_name]]) && !is.factor(data[[each_name]])) {
      stop(paste0("for_each column '", each_name, "' must be character or factor, not ", typeof(data[[each_name]]), "."))
    }
  }

  x_name <- as_name(x_sym)

  has_strat    <- !quo_is_null(strat_q)
  has_each     <- !quo_is_null(each_q)

  if (has_each) {
    vals <- unique(pull(data, !!each_q))
    plots <- set_names(
      lapply(vals, function(val) {
        sub <- filter(data, !!each_q == val)
        plot_counts_1D(
          data        = sub,
          x           = !!x_sym,
          stratify_by = !!strat_q,
          for_each    = NULL,
          title       = title,
          subtitle    = if (!is.null(subtitle)) subtitle else paste0(as_label(each_q), ": ", val),
          legend_title = legend_title,
          x_lab       = x_lab,
          y_lab       = y_lab,
          viridis_color_option = viridis_color_option
        )
      }),
      vals
    )
    return(plots)
  }

  if (has_strat) {
    df <- data %>%
      count(!!x_sym, !!strat_q, name = "count") %>%
      rename(x_value = !!x_sym)
  } else {
    df <- data %>%
      count(!!x_sym, name = "count") %>%
      rename(x_value = !!x_sym)
  }

  if (has_strat) {
    p <- ggplot(df, aes(x = x_value, y = count, color = !!strat_q, group = !!strat_q)) +
      geom_line(linewidth = 1) +
      scale_color_viridis_d(option = viridis_color_option)
  } else {
    p <- ggplot(df, aes(x = x_value, y = count, group=1)) +
      geom_line(linewidth = 1, color = "black")
  }

  p <- p +
    labs(
      title = title,
      subtitle = subtitle,
      x     = x_lab %||% as_label(x_sym),
      y     = y_lab %||% "count",
      color = if (has_strat) (if(!is.null(legend_title)) legend_title else as_label(strat_q)) else NULL
    ) +
    scale_y_continuous(breaks = pretty(unique(df$count)))
    theme(
      plot.title   = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      axis.text.x  = element_text(angle = 45),
      legend.position = if (has_strat) "right" else "none"
    )

    is_x_numeric <- is.numeric(data[[x_name]])

    if (is_x_numeric) {
      p <- p + scale_x_continuous(breaks = pretty(data[[x_name]]), expand = c(0,0))
    } else {
      p <- p + scale_x_discrete(expand = c(0,0))
    }


    # facet by stratify_by
    if (has_strat) {
      p <- p + facet_wrap(vars(!!strat_q))
    }

  p
}

############################################################################
#' Plot counts of observations across two dimensions, optionally stratified
#'
#' Computes the number of observations for each combination of two specified
#' variables, and displays the result as a heatmap using \pkg{ggplot2}. If a stratification variable
#' is provided, counts are calculated per strata and strata-specific heatmaps are displayed in individual panels.
#' If an additional stratification variable is provided, separate plot windows are created for each level.
#'
#' @param data Data frame containing all input variables.
#' @param x Variable in \code{data} whose values define the x-axis for counts.
#' @param y Variable in \code{data} whose values define the y-axis for counts.
#' @param stratify_by (Optional) Stratification variable. If
#'   supplied, counts are computed for each combination of \code{x}, \code{y} and
#'   \code{stratify_by}, and separate heatmaps are generated per level of \code{stratify_by}.
#' @param for_each (Optional) Additional stratification variable. If supplied,
#'   separate plot windows are created per level of \code{for_each}.
#' @param color_gradient (Optional) Color gradient for the heatmap. Specified as a character vector of three colors, representing: c(<low_counts>, <middle_counts>, <high_counts>).
#'   Defaults to \code{c("blue", "beige", "red")}. Colors must be recognized by \code{\link[ggplot2]{ggplot}}.
#' @param title (Optional) Plot title; defaults to \code{"Observation counts"}.
#' @param subtitle (Optional) Plot subtitle; defaults to \code{NULL} if for \code{for_each} is \code{NULL},
#' defaults to \code{<name of for_each>: <level of for_each>} for each plot window if \code{for_each} is supplied.
#' @param legend_title (Optional) Legend title for color gradient; defaults to "Count".
#' @param x_lab (Optional) Label for the x-axis; defaults to the name of \code{x}.
#' @param y_lab (Optional) Label for the y-axis; defaults to the name of \code{y}.
#'
#' @return If \code{for_each} is not supplied, a \link[ggplot2]{ggplot} object
#'   showing a heatmap of counts for each \code{x}-\code{y} combination, optionally
#'   faceted by \code{stratify_by}. If \code{for_each} is supplied, a named list
#'   of such \code{ggplot} objects, one per unique value of \code{for_each}.
#'
#' @export
#'
#' @seealso \code{\link{plot_counts_1D}}, \code{\link{plot_binned_counts}},
#'          \code{\link{plot_counts_with_mean}}, \code{\link[ggplot2]{ggplot}}
#'
#' @examples
#' data("toy_data")
#' # Heatmap of counts by age and period
#' plot_counts_2D(toy_data, x = age, y = period)
#' # Heatmap of counts by age and period, stratified by education
#' plot_counts_2D(toy_data, x = period, y = age,
#'                stratify_by = education)
#' # Heatmap of counts by age and period, stratified by education, for each sex
#' plot_counts_2D(toy_data, x = period, y = age,
#'                stratify_by = education, for_each = sex)
plot_counts_2D <- function(data, x, y,
                           stratify_by = NULL,
                           for_each = NULL,
                           color_gradient = c("blue", "beige", "red"),
                           title = "Observation counts",
                           legend_title = NULL,
                           subtitle = NULL,
                           x_lab = NULL,
                           y_lab = NULL) {
  x_sym <- ensym(x)
  y_sym <- ensym(y)

  strat_q <- enquo(stratify_by)
  each_q <- enquo(for_each)

  if (!quo_is_null(strat_q)) {
    strat_name <- as_name(strat_q)
    if (!(strat_name %in% names(data))) {
      stop(paste0("stratify_by column '", strat_name, "' not found in data."))
    }
    if (!is.character(data[[strat_name]]) && !is.factor(data[[strat_name]])) {
      stop(paste0("stratify_by column '", strat_name, "' must be character or factor, not ", typeof(data[[strat_name]]), "."))
    }
  }

  if (!quo_is_null(each_q)) {
    each_name <- as_name(each_q)
    if (!(each_name %in% names(data))) {
      stop(paste0("for_each column '", each_name, "' not found in data."))
    }
    if (!is.character(data[[each_name]]) && !is.factor(data[[each_name]])) {
      stop(paste0("for_each column '", each_name, "' must be character or factor, not ", typeof(data[[each_name]]), "."))
    }
  }

  has_stratify <- !quo_is_null(strat_q)
  has_for_each <- !quo_is_null(each_q)

  x_name <- as_label(x_sym)
  y_name <- as_label(y_sym)

  if (has_for_each) {
    group_vals <- unique(pull(data, !!each_q))

    plots <- lapply(group_vals, function(val) {
      df_sub <- filter(data, !!each_q == val)

      plot_counts_2D(
        data = df_sub,
        x = !!x_sym,
        y = !!y_sym,
        stratify_by = !!strat_q,
        color_gradient = color_gradient,
        title = title,
        subtitle = if (!is.null(subtitle)) subtitle else paste0(as_label(each_q), ": ", val),
        legend_title = legend_title,
        x_lab = x_lab,
        y_lab = y_lab
      )
    })

    names(plots) <- as.character(group_vals)
    return(plots)
  }

  if (has_stratify) {
    agg_data <- data %>%
      group_by(!!y_sym, !!x_sym, !!strat_q) %>%
      summarise(count = n(), .groups = "drop")
  } else {
    agg_data <- data %>%
      group_by(!!y_sym, !!x_sym) %>%
      summarise(count = n(), .groups = "drop")
  }

  global_min <- min(agg_data$count, na.rm = TRUE)
  global_max <- max(agg_data$count, na.rm = TRUE)

  p <- ggplot(agg_data, aes(x = !!x_sym, y = !!y_sym, fill = count)) +
    geom_tile() +
    scale_fill_gradientn(
      colors = color_gradient,
      values = rescale(c(global_min, (global_min + global_max) / 2, global_max)),
      na.value = "black",
      limits = c(global_min, global_max)
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      x = if (!is.null(x_lab)) x_lab else x_name,
      y = if (!is.null(y_lab)) y_lab else y_name,
      fill = if(!is.null(legend_title)) legend_title else "count"
    ) +
    theme(
      plot.title = element_text(hjust=0.5),
      plot.subtitle = element_text(hjust=0.5),
      axis.text.x = element_text(angle = 45)
    )

  is_x_numeric <- is.numeric(data[[x_name]])
  is_y_numeric <- is.numeric(data[[y_name]])

  if (is.numeric(data[[x_name]])) {
    p <- p + scale_x_continuous(expand = c(0, 0))
  } else {
    p <- p + scale_x_discrete(expand = c(0, 0))
  }

  if (is.numeric(data[[y_name]])) {
    p <- p + scale_y_continuous(expand = c(0, 0))
  } else {
    p <- p + scale_y_discrete(expand = c(0, 0))
  }


  # facet by stratify_by
  if (has_stratify) {
    p <- p + facet_wrap(vars(!!strat_q))
  }

  return(p)
}

###########################################################################
#' Plot counts of observations across bins of a numeric variable, optionally stratified
#'
#' Bins a specified numeric variable into intervals, counts observations per value of a specified variable and bin groups,
#' and plots lines for each bin group using \pkg{ggplot2}. If a stratification variable
#' is provided, counts are calculated per strata and plotted as separate colored lines.
#' If an additional stratification variable is provided, separate plot windows are created for each level.
#'
#' @param data Data frame containing all input variables.
#' @param x Variable in \code{data} whose values define the x-axis for counts.
#' @param bin_by Numeric variable in \code{data} for which to bin observations.
#' @param n_bins (Optional) Number of bins to create across \code{bin_by}; defaults to 8.
#' @param bin_width (Optional) Width of the bins created across \code{bin_by}; defaults to NULL.
#'    Overrides \code{n_bins} if both are supplied.
#' @param stratify_by (Optional) Stratification variable. If
#'   supplied, counts are computed for each combination of \code{x}, bin of \code{bin_by} and
#'   \code{stratify_by}, and separate panels are made for each level of \code{stratify_by}.
#' @param for_each (Optional) Additional stratification variable. If supplied,
#'   separate plot windows are created per level of \code{for_each}.
#' @param title (Optional) Plot title; defaults to \code{"Observation counts"}.
#' @param subtitle (Optional) Plot subtitle; defaults to \code{NULL} if for \code{for_each} is \code{NULL},
#' defaults to \code{<name of for_each>: <level of for_each>} for each plot window if \code{for_each} is supplied.
#' @param legend_title (Optional) Legend title; defaults to name of \code{bin_by} + "bin".
#' @param x_lab (Optional) Label for the x-axis; defaults to the name of \code{x}.
#' @param y_lab (Optional) Label for the y-axis; defaults to the name of \code{y}.
#' @param viridis_color_option (Optional) Option for color gradient; defaults to "D".
#'   Options are "A", "B", "C", "D", E", "F", "G", "H".
#'   See \pkg{viridis} for information, or experiment yourself.
#'
#' @return If \code{for_each} is not supplied, a \link[ggplot2]{ggplot} object showing counts
#'   per \code{x} and bin groups, optionally faceted by \code{stratify_by}. If \code{for_each}
#'   is supplied, a named list of such plots.
#'
#'
#' @export
#'
#' @seealso \code{\link{plot_counts_1D}}, \code{\link{plot_counts_2D}},
#'          \code{\link{plot_counts_with_mean}}, \code{\link[ggplot2]{ggplot}}
#'
#' @examples
#' data("toy_data")
#' # Counts by period, binned by age
#' plot_binned_counts(toy_data, x = period,
#'                    bin_by = age, n_bins = 4)
#' # Counts by period, binned by age, stratified by education levels
#' plot_binned_counts(toy_data, period,
#'                    bin_by = age, n_bins = 4,
#'                    stratify_by = education)
#' # Counts by period, binned by age, stratified by education levels, for each sex
#' plot_binned_counts(toy_data, period,
#'                    bin_by = age, n_bins = 4,
#'                    stratify_by = education, for_each = sex)
plot_binned_counts <- function(data,
                               x,
                               bin_by,
                               stratify_by = NULL,
                               for_each    = NULL,
                               n_bins      = 8,
                               bin_width   = NULL,
                               title       = "Observation counts",
                               subtitle    = NULL,
                               legend_title = NULL,
                               x_lab       = NULL,
                               y_lab       = NULL,
                               viridis_color_option = "D") {

  x_q      <- enquo(x)
  bin_by_q <- enquo(bin_by)
  strat_q  <- enquo(stratify_by)
  each_q   <- enquo(for_each)

  if (!quo_is_null(strat_q)) {
    strat_name <- as_name(strat_q)
    if (!(strat_name %in% names(data))) {
      stop(paste0("stratify_by column '", strat_name, "' not found in data."))
    }
    if (!is.character(data[[strat_name]]) && !is.factor(data[[strat_name]])) {
      stop(paste0("stratify_by column '", strat_name, "' must be character or factor, not ", typeof(data[[strat_name]]), "."))
    }
  }

  if (!quo_is_null(each_q)) {
    each_name <- as_name(each_q)
    if (!(each_name %in% names(data))) {
      stop(paste0("for_each column '", each_name, "' not found in data."))
    }
    if (!is.character(data[[each_name]]) && !is.factor(data[[each_name]])) {
      stop(paste0("for_each column '", each_name, "' must be character or factor, not ", typeof(data[[each_name]]), "."))
    }
  }

  has_strat <- !quo_is_null(strat_q)
  has_each  <- !quo_is_null(each_q)

  x_name      <- as_label(x_q)
  bin_by_name <- as_label(bin_by_q)
  strat_name  <- if (has_strat) as_label(strat_q) else NULL
  each_name   <- if (has_each) as_label(each_q) else NULL

  # recursion
  if (has_each) {
    vals <- unique(pull(data, !!each_q))
    plots <- set_names(
      lapply(vals, function(val) {
        sub <- filter(data, !!each_q == val)
        plot_binned_counts(sub,
                           x           = !!x_q,
                           bin_by      = !!bin_by_q,
                           stratify_by = !!strat_q,
                           for_each    = NULL,
                           n_bins      = n_bins,
                           bin_width   = bin_width,
                           title       = title,
                           subtitle    = paste0(each_name, ": ", val),
                           legend_title= legend_title,
                           x_lab       = x_lab,
                           y_lab       = y_lab,
                           viridis_color_option = viridis_color_option
        )
      }),
      vals
    )
    return(plots)
  }

  vals <- pull(data, !!bin_by_q) %>% na.omit()
  min_v   <- floor(min(vals, na.rm = TRUE))
  max_v   <- ceiling(max(vals, na.rm = TRUE))

  if (!is.null(bin_width)) {
    if (!missing(n_bins) && !is.null(n_bins)) {
      message("Both 'bin_width' and 'n_bins' provided; using 'bin_width' and ignoring 'n_bins'.")
    }
    brk <- seq(min_v, max_v + bin_width, by = bin_width)
  } else {
    seq_raw <- seq(min_v, max_v, length.out = n_bins + 1)
    brk <- pretty(seq(min_v, max_v, length.out = n_bins + 1))
  }
  if (brk[1] > min_v) brk <- c(min_v, brk)
  if (tail(brk,1) < max_v) brk <- c(brk, max_v)

  brk <- unique(as.integer(brk))
  if (length(brk) < 2) stop("Unable to compute valid bin breaks.")

  bin_labels <- paste(head(brk, -1), "-", tail(brk, -1), sep = "")
  bin_col    <- paste0(bin_by_name, "_bin")

  data2 <- data %>%
    mutate(!!bin_col := cut(
      !!bin_by_q,
      breaks         = brk,
      right          = FALSE,
      include.lowest = TRUE,
      labels         = bin_labels
    ))

  count_df <- if (has_strat) {
    data2 %>% count(!!x_q, !!sym(bin_col), !!strat_q, name = "n")
  } else {
    data2 %>% count(!!x_q, !!sym(bin_col), name = "n")
  }

  p <- ggplot(count_df, aes(x = !!x_q, y = n, color = !!sym(bin_col))) +
    geom_line(linewidth = 1) +
    labs(
      title    = title,
      subtitle = subtitle,
      x        = x_lab %||% x_name,
      y        = y_lab %||% "count",
      color    = legend_title %||% paste(bin_by_name, "bin")
    ) +
    scale_color_viridis_d(option = viridis_color_option, direction = -1) +
    theme(
      axis.text.x    = element_text(angle = 45, hjust = 1),
      plot.title     = element_text(hjust = 0.5),
      legend.position= "right"
    )

  if (has_strat) {
    p <- p + facet_wrap(reformulate(strat_name), labeller = label_value)
  }

  p
}

#############################################################################
#' Plot heatmap of observation counts with mean overlay, optionally stratified
#'
#' Computes counts of observations for each combination of two variables and displays
#' them as a heatmap, with an overlaid line showing the mean of the second variable
#' across the first. If a stratification variable is provided, observations are counted per strata and strata-specific heatmaps are displayed in individual panels.
#' If an additional stratification variable is provided, separate plot windows are created for each level.
#'
#' @param data Data frame containing all input variables.
#' @param x Variable in \code{data} whose values define the x-axis for counts.
#' @param y Variable in \code{data} whose values define the y-axis for counts, and for which the mean is computed for each value for \code{x}.
#' @param stratify_by (Optional) Stratification variable. If
#'   supplied, counts and means are computed for each level of \code{stratify_by},
#'   and separate heatmaps are generated per level of \code{stratify_by}.
#' @param for_each (Optional) Additional stratification variable.
#'   If supplied, separate plot windows are created per level of \code{for_each}.
#' @param title (Optional) Plot title; defaults to \code{NULL}, in which case a title
#'   of the form "\<Y\> distribution across \<X\>" is used.
#' @param subtitle (Optional) Plot subtitle; defaults to \code{NULL}, or to
#'   "\<for_each\>: \<level\>" when \code{for_each} is supplied.
#' @param heatmap_legend (Optional) Label for the heatmap legend; defaults to "Count".
#' @param mean_legend (Optional) Label for the overlay mean line legend; defaults to "Mean".
#' @param viridis_color_option (Optional) Option for color gradient; defaults to "D".
#'   Options are "A", "B", "C", "D", E", "F", "G", "H".
#'   See \pkg{viridis} for information, or experiment yourself.
#' @param mean_color (Optional) Color for the overlay mean line; defaults to "coral".
#'
#' @return If \code{for_each} is not supplied, a \code{\link[ggplot2]{ggplot}} object showing a
#'   heatmap of counts with a mean overlay line, optionally faceted by \code{stratify_by}.
#'   If \code{for_each} is supplied, a named list of such plots.
#'
#'
#' @export
#'
#' @seealso \code{\link{plot_counts_1D}}, \code{\link{plot_counts_2D}},
#'          \code{\link{plot_binned_counts}}, \code{\link[ggplot2]{ggplot}}
#'
#' @examples
#' data("toy_data")
#' # Heatmap of counts by age vs. period with mean age overlay
#' plot_counts_with_mean(toy_data, x = period, y = age)
#' # Heatmap of counts by age vs. period with mean age overlay, stratified by education
#' plot_counts_with_mean(toy_data, x = period, y = age,
#'                       stratify_by = education)
#' # Heatmap of counts by age vs. period with mean age overlay, stratified by education, for each sex
#' plot_counts_with_mean(toy_data, x = period, y = age,
#'                       stratify_by = education, for_each = sex)
plot_counts_with_mean <- function(data,
                                  x,
                                  y,
                                  stratify_by   = NULL,
                                  for_each      = NULL,
                                  title         = NULL,
                                  subtitle      = NULL,
                                  heatmap_legend= "Count",
                                  mean_legend   = "Mean",
                                  viridis_color_option= "D",
                                  mean_color = "coral") {

  x_q     <- enquo(x)
  y_q     <- enquo(y)
  x_name <- as_label(x_q)
  y_name <- as_label(y_q)
  strat_q <- enquo(stratify_by)
  each_q  <- enquo(for_each)

  if (!quo_is_null(strat_q)) {
    strat_name <- as_name(strat_q)
    if (!(strat_name %in% names(data))) {
      stop(paste0("stratify_by column '", strat_name, "' not found in data."))
    }
    if (!is.character(data[[strat_name]]) && !is.factor(data[[strat_name]])) {
      stop(paste0("stratify_by column '", strat_name, "' must be character or factor, not ", typeof(data[[strat_name]]), "."))
    }
  }

  if (!quo_is_null(each_q)) {
    each_name <- as_name(each_q)
    if (!(each_name %in% names(data))) {
      stop(paste0("for_each column '", each_name, "' not found in data."))
    }
    if (!is.character(data[[each_name]]) && !is.factor(data[[each_name]])) {
      stop(paste0("for_each column '", each_name, "' must be character or factor, not ", typeof(data[[each_name]]), "."))
    }
  }

  has_strat <- !quo_is_null(strat_q)
  has_each  <- !quo_is_null(each_q)

  if (has_each) {
    levels <- unique(pull(data, !!each_q))
    return(set_names(
      lapply(levels, function(level) {
        subset <- filter(data, !!each_q == level)
        plot_counts_with_mean(
          data          = subset,
          x         = !!x_q,
          y         = !!y_q,
          stratify_by   = !!strat_q,
          for_each      = NULL,
          title         = title,
          subtitle      = paste0(as_label(each_q), ": ", level),
          heatmap_legend    = heatmap_legend,
          mean_legend= mean_legend,
          viridis_color_option= viridis_color_option,
          mean_color    = mean_color
        )
      }),
      levels
    ))
  }

  df <- data %>% filter(!is.na(!!x_q), !is.na(!!y_q))

  if (has_strat) {
    df <- df %>% filter(!is.na(!!strat_q))
    df_counts <- df %>%
      count(!!strat_q, !!x_q, !!y_q, name = "count") %>%
      transmute(
        strat = !!strat_q,
        x     = !!x_q,
        y     = !!y_q,
        count = count
      )

    df_mean <- df %>%
      group_by(!!strat_q, !!x_q) %>%
      summarise(mean_y = mean(!!y_q, na.rm = TRUE), .groups = "drop") %>%
      transmute(
        strat         = !!strat_q,
        x             = !!x_q,
        mean_y        = mean_y,
        overlay_group = mean_legend
      )
  } else {
    df_counts <- df %>%
      count(!!x_q, !!y_q, name = "count") %>%
      transmute(x = !!x_q, y = !!y_q, count = count)

    df_mean <- df %>%
      group_by(!!x_q) %>%
      summarise(mean_y = mean(!!y_q, na.rm = TRUE), .groups = "drop") %>%
      transmute(x = !!x_q, mean_y = mean_y, overlay_group = mean_legend)
  }

  fill_max <- max(df_counts$count, na.rm = TRUE)

  p <- ggplot(df_counts, aes(x = x, y = y)) +
    geom_tile(aes(fill = count)) +
    {
      if (has_strat) {
        geom_line(
          data = df_mean,
          aes(x = x, y = mean_y,
              color = overlay_group,
              group = interaction(strat, overlay_group)),
          size = 1.2
        )
      } else {
        geom_line(
          data = df_mean,
          aes(x = x, y = mean_y,
              color = overlay_group,
              group = overlay_group),
          size = 1.2
        )
      }
    } +
    scale_fill_viridis_c(
      option    = viridis_color_option,
      direction = 1,
      limits    = c(0, fill_max),
      oob       = squish,
      name      = heatmap_legend,
      guide     = guide_colorbar(
        barwidth      = unit(5, "cm"),
        barheight     = unit(0.5, "cm"),
        title.position= "top",
        label.position= "bottom"
      )
    ) +
    scale_color_manual(
      name   = NULL,
      values = setNames(mean_color, mean_legend),
      guide  = guide_legend(
        override.aes = list(linetype = 1),
        title.position = "bottom",
        label.position = "top"
      )
    ) +
    labs(
      title    = title %||% paste(str_to_title(as_label(y_q)),
                                  "distribution across",
                                  as_label(x_q)),
      subtitle = subtitle,
      x        = as_label(x_q),
      y        = as_label(y_q)
    ) +
    theme(
      plot.title     = element_text(hjust = 0.5),
      plot.subtitle  = element_text(hjust = 0.5),
      axis.text.x    = element_text(angle = 45),
      legend.position= "bottom"
    )

  if (is.numeric(data[[x_name]])) {
    p <- p + scale_x_continuous(expand = c(0, 0))
  } else {
    p <- p + scale_x_discrete(expand = c(0, 0))
  }

  if (is.numeric(data[[y_name]])) {
    p <- p + scale_y_continuous(expand = c(0, 0))
  } else {
    p <- p + scale_y_discrete(expand = c(0, 0))
  }

  if (has_strat) {
    p <- p + facet_wrap(vars(strat), labeller = label_value)
  }

  p
}

################################################################################
#' Plot mean of a response variable across a single variable, optionally stratified
#'
#' Computes the mean of a specified response variable at each value of a specified x variable
#' and displays a line plot using \pkg{ggplot2}. If a stratification variable
#' is provided, means are calculated per strata and plotted as separate colored lines.
#' If an additional stratification variable is provided, separate plot windows are created for each level.
#'
#' @param data A \code{data.frame} or tibble containing the dataset.
#' @param response A numeric variable in \code{data} whose mean will be plotted.
#' @param x A variable in \code{data} defining the x-axis for computing means.
#' @param stratify_by (Optional) Stratification variable. If
#'   supplied, counts are computed for each combination of \code{x} and
#'   \code{stratify_by}, and separate lines are drawn per level of \code{stratify_by}.
#' @param for_each (Optional) Additional stratification variable. If supplied,
#'   separate plot windows are created per level of \code{for_each}.
#' @param title (Optional) Plot title; defaults to \code{"Observation counts"}.
#' @param subtitle (Optional) Plot subtitle; defaults to \code{NULL} if for \code{for_each} is \code{NULL},
#' defaults to \code{<name of for_each>: <level of for_each>} for each plot window if \code{for_each} is supplied.
#' @param legend_title (Optional) Legend title; defaults to name of \code{stratify_var} if it is supplied.
#' @param x_lab (Optional) Label for the x-axis; defaults to the name of \code{x}.
#' @param y_lab (Optional) Label for the y-axis; defaults to the name of \code{paste("Mean", <response_name>)}.
#' @param viridis_color_option (Optional) Option for color gradient; defaults to "D".
#'   Options are "A", "B", "C", "D", E", "F", "G", "H".
#'   See \pkg{viridis} for information, or experiment yourself.
#'
#' @return A \code{\link[ggplot2]{ggplot}} object displaying the mean of the response across the variable supplied in \code{x},
#'   optionally stratified by \code{stratify_by}. If \code{for_each} is supplied, separate plots are created  in separate windows for each level.
#'   Visuals can be modified with \pkg{ggplot2}.
#'
#' @export
#'
#' @seealso \code{\link{plot_mean_response_2D}}, \code{\link[ggplot2]{ggplot}}
#'
#' @examples
#' data("toy_data")
#' # Mean by age
#' plot_mean_response_1D(toy_data, response = count, x = age)
#' # Mean count by age, stratified by education
#' plot_mean_response_1D(toy_data, response = count, x = age,
#'                       stratify_by = education)
#' # Mean count by age, stratified by education, for each sex
#' plot_mean_response_1D(toy_data, response = count, x = age,
#'                       stratify_by = education, for_each = sex)
plot_mean_response_1D <- function(data,
                                  response,
                                  x,
                                  stratify_by = NULL,
                                  for_each    = NULL,
                                  title       = NULL,
                                  subtitle    = NULL,
                                  legend_title = NULL,
                                  x_lab       = NULL,
                                  y_lab       = NULL,
                                  viridis_color_option = "D") {

  resp_q   <- enquo(response)
  x_q      <- enquo(x)
  strat_q  <- enquo(stratify_by)
  each_q   <- enquo(for_each)

  if (!quo_is_null(strat_q)) {
    strat_name <- as_name(strat_q)
    if (!(strat_name %in% names(data))) {
      stop(paste0("stratify_by column '", strat_name, "' not found in data."))
    }
    if (!is.character(data[[strat_name]]) && !is.factor(data[[strat_name]])) {
      stop(paste0("stratify_by column '", strat_name, "' must be character or factor, not ", typeof(data[[strat_name]]), "."))
    }
  }

  if (!quo_is_null(each_q)) {
    each_name <- as_name(each_q)
    if (!(each_name %in% names(data))) {
      stop(paste0("for_each column '", each_name, "' not found in data."))
    }
    if (!is.character(data[[each_name]]) && !is.factor(data[[each_name]])) {
      stop(paste0("for_each column '", each_name, "' must be character or factor, not ", typeof(data[[each_name]]), "."))
    }
  }

  has_strat <- !quo_is_null(strat_q)
  has_each  <- !quo_is_null(each_q)

  if (has_each) {
    vals <- unique(pull(data, !!each_q))
    plots <- set_names(
      lapply(vals, function(val) {
        df_sub <- filter(data, !!each_q == val)
        plot_mean_response_1D(
          data          = df_sub,
          response      = !!resp_q,
          x             = !!x_q,
          stratify_by   = !!strat_q,
          for_each      = NULL,
          title         = title,
          subtitle      = subtitle %||% paste0(as_name(each_q), ": ", val),
          legend_title  = legend_title,
          x_lab         = x_lab,
          y_lab         = y_lab,
          viridis_color_option = viridis_color_option
        )
      }),
      vals
    )
    return(plots)
  }

  resp_nm  <- as_label(resp_q)
  x_name     <- as_label(x_q)

  grouping <- list(x_q)
  if (has_strat) grouping <- append(grouping, list(strat_q))

  summary_df <- data %>%
    group_by(!!!grouping) %>%
    summarise(mean_val = mean(!!resp_q, na.rm = TRUE), .groups = "drop")

  p <- ggplot(summary_df, aes(x = !!x_q, y = mean_val, group=1))
  if (has_strat) {
    p <- p +
      aes(color = !!strat_q, group = !!strat_q) +
      scale_color_viridis_d(
        name = legend_title %||% as_label(strat_q),
        option = viridis_color_option,
        direction = -1
      )
  }
  p <- p + geom_line(linewidth = 1)

  p <- p +
    labs(
      title = title %||% paste("Mean", resp_nm),
      subtitle = subtitle,
      x     = x_lab %||% x_name,
      y     = y_lab %||% paste("mean", resp_nm),
    ) +
    theme(
      plot.title     = element_text(hjust = 0.5),
      plot.subtitle     = element_text(hjust = 0.5),
      axis.text.x    = element_text(angle = 45)
    )

  if (is.numeric(data[[x_name]])) {
    p <- p + scale_x_continuous(expand = c(0, 0))
  } else {
    p <- p + scale_x_discrete(expand = c(0, 0))
  }

  return(p)
}

##################################################################################

#' Plot mean of a response variable across two dimensions, optionally stratified
#'
#' Computes the mean of a specified response variable for each combination of two
#' variables and displays it as a heatmap using \pkg{ggplot2}. If a stratification variable
#' is provided, means are calculated per strata and strata-specific heatmaps are displayed in individual panels.
#' If an additional stratification variable is provided, separate plot windows are created for each level.
#'
#' @param data Data frame containing all input variables.
#' @param response Numeric variable in \code{data} whose mean to compute.
#' @param x Variable in \code{data} for the horizontal axis.
#' @param y Variable in \code{data} for the vertical axis.
#' @param stratify_by (Optional) Stratification variable. If
#'   supplied, means are computed for each combination of \code{x}, \code{y} and
#'   \code{stratify_by}, and separate heatmaps are generated per level of \code{stratify_by}.
#' @param for_each (Optional) Additional stratification variable.
#'   If supplied, separate plot windows are created per level of \code{for_each}.
#' @param color_gradient (Optional) Color gradient for the heatmap. Specified as a character vector of three colors, representing: c(<low_counts>, <middle_counts>, <high_counts>).
#'   Defaults to \code{c("blue", "beige", "red")}. Colors must be recognized by \code{\link[ggplot2]{ggplot}}.
#' @param title Plot title; defaults to \code{NULL}, in which case a title
#'   of the form "Mean \code{<response>}" is used.
#' @param subtitle (Optional) Plot subtitle; defaults to \code{NULL} if for \code{for_each} is \code{NULL},
#' defaults to \code{<name of for_each>: <level of for_each>} for each plot window if \code{for_each} is supplied.
#' @param x_lab (Optional) Label for the x-axis; defaults to the name of \code{x}.
#' @param y_lab (Optional) Label for the y-axis; defaults to the name of \code{y}.
#'
#' @return A \link[ggplot2]{ggplot} object showing the mean of \code{response}
#'   across \code{x} and \code{y}, optionally faceted by \code{facet_row} and/or \code{facet_col}.
#'
#' @export
#'
#' @seealso \code{\link{plot_mean_response_1D}}, \code{\link[ggplot2]{ggplot}}
#'
#' @examples
#' data("toy_data")
#' # Mean count by age and period
#' plot_mean_response_2D(toy_data, response = count, x = period, y = age)
#' # Mean count by age and period, stratified by education level
#' plot_mean_response_2D(toy_data, response = count, x = period, y = age,
#'                       stratify_by = education)
#' # Mean count by age and period, stratified by education level, for each sex
#' plot_mean_response_2D(toy_data, response = count, x = period, y = age,
#'                       stratify_by = education, for_each = sex)
plot_mean_response_2D <- function(data,
                                  response,
                                  x,
                                  y,
                                  stratify_by = NULL,
                                  for_each    = NULL,
                                  color_gradient = c("blue","beige","red"),
                                  title = NULL,
                                  subtitle = NULL,
                                  x_lab = NULL,
                                  y_lab = NULL) {


  resp_q       <- enquo(response)
  x_q          <- enquo(x)
  y_q          <- enquo(y)
  strat_q      <- enquo(stratify_by)
  each_q       <- enquo(for_each)

  if (!quo_is_null(strat_q)) {
    strat_name <- as_name(strat_q)
    if (!(strat_name %in% names(data))) {
      stop(paste0("stratify_by column '", strat_name, "' not found in data."))
    }
    if (!is.character(data[[strat_name]]) && !is.factor(data[[strat_name]])) {
      stop(paste0("stratify_by column '", strat_name, "' must be character or factor, not ", typeof(data[[strat_name]]), "."))
    }
  }

  if (!quo_is_null(each_q)) {
    each_name <- as_name(each_q)
    if (!(each_name %in% names(data))) {
      stop(paste0("for_each column '", each_name, "' not found in data."))
    }
    if (!is.character(data[[each_name]]) && !is.factor(data[[each_name]])) {
      stop(paste0("for_each column '", each_name, "' must be character or factor, not ", typeof(data[[each_name]]), "."))
    }
  }

  has_strat    <- !quo_is_null(strat_q)
  has_each     <- !quo_is_null(each_q)

  # recursion over for_each
  if (has_each) {
    vals <- unique(pull(data, !!each_q))
    plots <- set_names(
      lapply(vals, function(val) {
        sub <- filter(data, !!each_q == val)
        plot_mean_response_2D(
          data             = sub,
          response         = !!resp_q,
          x                = !!x_q,
          y                = !!y_q,
          stratify_by      = !!strat_q,
          for_each         = NULL,
          color_gradient   = color_gradient,
          title            = title,
          subtitle         = subtitle %||% paste0(as_name(each_q), ": ", val),
          x_lab            = x_lab,
          y_lab            = y_lab
        )
      }),
      vals
    )
    return(plots)
  }

  # names
  resp_nm <- as_label(resp_q)
  x_name    <- as_label(x_q)
  y_name    <- as_label(y_q)
  strat_nm<- if (has_strat) as_label(strat_q) else NULL

  grouping <- list(x_q, y_q)
  if (has_strat) grouping <- append(grouping, list(strat_q))
  summary_df <- data %>%
    group_by(!!!grouping) %>%
    summarise(mean_response = mean(!!resp_q, na.rm = TRUE), .groups = "drop")

  mn <- min(summary_df$mean_response, na.rm = TRUE)
  mx <- max(summary_df$mean_response, na.rm = TRUE)

  p <- ggplot(summary_df,
              aes(x = !!x_q,
                  y = !!y_q,
                  fill = mean_response)) +
    geom_tile() +
    scale_fill_gradientn(
      colors = color_gradient,
      values = rescale(c(mn, (mn+mx)/2, mx)),
      na.value = "black",
      limits = c(mn, mx),
      guide = guide_colorbar(barwidth = 0.5, barheight = 5)
    ) +
    labs(
      title = title %||% paste("Mean", resp_nm),
      subtitle = subtitle,
      x = x_lab %||% x_name,
      y = y_lab %||% y_name,
      fill = paste("mean", resp_nm)
    ) +
    theme(axis.text.x = element_text(angle = 45),
          plot.title = element_text(hjust=0.5),
          plot.subtitle = element_text(hjust=0.5))

  if (is.numeric(data[[x_name]])) {
    p <- p + scale_x_continuous(expand = c(0, 0))
  } else {
    p <- p + scale_x_discrete(expand = c(0, 0))
  }

  if (is.numeric(data[[y_name]])) {
    p <- p + scale_y_continuous(expand = c(0, 0))
  } else {
    p <- p + scale_y_discrete(expand = c(0, 0))
  }

  if (has_strat) {
    p <- p + facet_wrap(vars(!!strat_q))
  }

  p
}
