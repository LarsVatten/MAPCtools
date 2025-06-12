# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# This file serves as the source file for function used to perform exploratory
# analysis on age-period-cohort data.

utils::globalVariables(c(
  "bin","time_bin"
))

###############################################################################
### Imports:

#' @importFrom ggplot2 ggplot facet_grid
#' @importFrom dplyr %>% distinct across all_of
#' @importFrom utils tail
#' @importFrom stats reformulate
#' @importFrom rlang ensym enquo quo_is_null as_label
#' @importFrom dplyr group_by summarise pull filter
#' @importFrom scales rescale
#' @importFrom rlang ensym as_name
#' @importFrom dplyr count rename
#' @importFrom rlang enquo quo_is_null as_label
#' @importFrom dplyr group_by summarise pull filter
#' @importFrom rlang enquo quo_is_null as_label sym
#' @importFrom dplyr pull filter mutate count
#' @importFrom viridis viridis
#' @importFrom purrr set_names
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
#' @param x_breaks (Optional) Numeric vector of break points for the x-axis;
#'   defaults to \code{pretty()} over unique values of \code{x}.
#' @param y_breaks (Optional) Numeric vector of break points for the y-axis;
#'   defaults to \code{pretty()} over unique values of \code{y}.
#'
#' @return A \link[ggplot2]{ggplot} object displaying counts across the variable supplied in \code{x},
#'   optionally stratified by \code{stratify_by}. If \code{for_each} is supplied, separate plots are created  in separate windows for each level.
#'   Visuals can be modified with \pkg{ggplot2}.
#'
#' @export
#'
#' @examples
#' data("toy_data")
#' # Counts by age
#' plot_counts_1D(toy_data, age)
#' # Counts by age, stratified by education level
#' plot_counts_1D(toy_data, age, stratify_by = education)
#' # Count by age, stratified by education level, for each sex
#' plot_counts_1D(toy_data, age, stratify_by = education, for_each = sex)
plot_counts_1D <- function(data, x,
                           stratify_by = NULL,
                           for_each    = NULL,
                           title       = "Observation counts",
                           subtitle    = NULL,
                           legend_title = NULL,
                           x_lab        = NULL,
                           y_lab       = NULL,
                           x_breaks    = NULL,
                           y_limits    = NULL,
                           y_breaks    = NULL) {

  x_sym        <- ensym(x)
  strat_quo    <- enquo(stratify_by)
  each_quo     <- enquo(for_each)

  has_strat    <- !quo_is_null(strat_quo)
  has_each     <- !quo_is_null(each_quo)

  if (has_each) {
    vals <- unique(pull(data, !!each_quo))
    plots <- set_names(
      lapply(vals, function(val) {
        sub <- filter(data, !!each_quo == val)
        plot_counts_1D(
          data        = sub,
          x           = !!x_sym,
          stratify_by = !!strat_quo,
          for_each    = NULL,
          title       = title,
          subtitle    = if (!is.null(subtitle)) subtitle else paste0(as_label(each_quo), ": ", val),
          legend_title = legend_title,
          x_breaks    = x_breaks,
          y_limits    = y_limits,
          y_breaks    = y_breaks,
          x_label     = x_label
        )
      }),
      vals
    )
    return(plots)
  }

  if (has_strat) {
    df <- data %>%
      count(!!x_sym, !!strat_quo, name = "count") %>%
      rename(x_value = !!x_sym, group = !!strat_quo)
  } else {
    df <- data %>%
      count(!!x_sym, name = "count") %>%
      rename(x_value = !!x_sym)
  }

  if (has_strat) {
    p <- ggplot(df, aes(x = x_value, y = count, color = group, group = group)) +
      geom_line(linewidth = 1) +
      scale_color_viridis_d(option = "D")
  } else {
    p <- ggplot(df, aes(x = x_value, y = count)) +
      geom_line(linewidth = 1, color = "black")
  }

  p <- p +
    labs(
      title = title,
      subtitle = subtitle,
      x     = x_label %||% as_label(x_sym),
      y     = "Count",
      color = if (has_strat) (if(!is.null(legend_title)) legend_title else as_label(strat_quo)) else NULL
    ) +
    theme(
      plot.title   = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust=0.5),
      axis.text.x  = element_text(angle = 45),
      legend.position = if (has_strat) "right" else "none"
    )

  x_name <- as_name(x_sym)
  x_vals <- data[[x_name]]
  x_lims <- range(x_vals, na.rm = TRUE)
  x_brks <- x_breaks %||% pretty(x_vals)

  y_brks <- y_breaks %||% pretty(unique(df$count))
  y_lims <- y_limits %||% range(df$count, na.rm = TRUE)

  p <- p +
    scale_x_continuous(limits = x_lims, breaks = x_brks) +
    scale_y_continuous(limits = y_lims, breaks = y_brks)

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
#'   Defaults to \code{c("blue", "beige", "red")}.
#' @param title (Optional) Plot title; defaults to \code{"Observation counts"}.
#' @param subtitle (Optional) Plot subtitle; defaults to \code{NULL} if for \code{for_each} is \code{NULL},
#' defaults to \code{<name of for_each>: <level of for_each>} for each plot window if \code{for_each} is supplied.
#' @param legend_title (Optional) Legend title for color gradient; defaults to "Count".
#' @param x_lab (Optional) Label for the x-axis; defaults to the name of \code{x}.
#' @param y_lab (Optional) Label for the y-axis; defaults to the name of \code{y}.
#' @param x_breaks (Optional) Numeric vector of break points for the x-axis;
#'   defaults to \code{pretty()} over unique values of \code{x}.
#' @param y_breaks (Optional) Numeric vector of break points for the y-axis;
#'   defaults to \code{pretty()} over unique values of \code{y}.
#'
#' @return If \code{for_each} is not supplied, a \link[ggplot2]{ggplot} object
#'   showing a heatmap of counts for each \code{x}-\code{y} combination, optionally
#'   faceted by \code{stratify_by}. If \code{for_each} is supplied, a named list
#'   of such \code{ggplot} objects, one per unique value of \code{for_each}.
#'
#' @export
#'
#'
#'
#'
#' @examples
#' data("toy_data")
#' # Heatmap of counts by age and period
#' plot_counts_2D(toy_data, age, period)
#' # Heatmap of counts by age and period, stratified by education
#' plot_counts_2D(toy_data, period, age, stratify_by = education)
#' # Heatmap of counts by age and period, stratified by education, for each sex
#' plot_counts_2D(toy_data, period, age, stratify_by = education, for_each = sex)
plot_counts_2D <- function(data, x, y,
                           stratify_by = NULL,
                           for_each = NULL,
                           color_gradient = c("blue", "beige", "red"),
                           title = "Observation counts",
                           legend_title = NULL,
                           subtitle = NULL,
                           x_lab = NULL,
                           y_lab = NULL,
                           x_breaks = NULL,
                           y_breaks = NULL) {
  x_sym <- ensym(x)
  y_sym <- ensym(y)

  stratify_quo <- enquo(stratify_by)
  for_each_quo <- enquo(for_each)

  has_stratify <- !quo_is_null(stratify_quo)
  has_for_each <- !quo_is_null(for_each_quo)

  x_name <- as_label(x_sym)
  y_name <- as_label(y_sym)

  if (has_for_each) {
    group_vals <- unique(pull(data, !!for_each_quo))

    plots <- lapply(group_vals, function(val) {
      df_sub <- filter(data, !!for_each_quo == val)

      plot_counts_2D(
        data = df_sub,
        x = !!x_sym,
        y = !!y_sym,
        stratify_by = !!stratify_quo,
        color_gradient = color_gradient,
        title = title,
        subtitle = if (!is.null(subtitle)) subtitle else paste0(as_label(for_each_quo), ": ", val),
        legend_title = legend_title,
        x_lab = x_lab,
        y_lab = y_lab,
        x_breaks = x_breaks,
        y_breaks = y_breaks,
      )
    })

    names(plots) <- as.character(group_vals)
    return(plots)
  }

  if (has_stratify) {
    agg_data <- data %>%
      group_by(!!y_sym, !!x_sym, !!stratify_quo) %>%
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
      fill = if(!is.null(legend_title)) legend_title else "Count"
    ) +
    scale_y_continuous(
      breaks = if (!is.null(y_breaks)) y_breaks else pretty(unique(data[[y_name]])),
      expand = c(0, 0)
    ) +
    scale_x_continuous(
      breaks = if (!is.null(x_breaks)) x_breaks else pretty(unique(data[[x_name]])),
      , expand = c(0, 0)
    ) +
    theme(
      plot.title = element_text(hjust=0.5),
      plot.subtitle = element_text(hjust=0.5),
      axis.text.x = element_text(angle = 45)
    )

  if (has_stratify) {
    p <- p + facet_wrap(
      vars(!!stratify_quo)
    )
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
#' @param y_breaks (Optional) Numeric vector of break points for the y-axis;
#'   defaults to \code{pretty()} over unique values of \code{y}.
#'
#' @return If \code{for_each} is not supplied, a \link[ggplot2]{ggplot} object showing counts
#'   per \code{x} and bin groups, optionally faceted by \code{stratify_by}. If \code{for_each}
#'   is supplied, a named list of such plots.
#'
#' @details
#' - Uses \link[rlang]{enquo}, \link[rlang]{quo_is_null}, and \link[rlang]{as_label}
#'   for tidy-evaluation of columns.
#' - Bins are computed via \link[stats]{pretty} on an extended range of \code{bin_by}.
#' - Counts are computed with \link[dplyr]{count}.
#' - Line colors use \link[viridis]{scale_color_viridis_d}.
#' - Faceting is applied with \link[ggplot2]{facet_wrap} when stratifying.
#'
#' @export
#'
#' @examples
#' data("toy_data")
#' # Counts by period, binned by age
#' plot_binned_counts(toy_data, period, bin_by = age, n_bins = 4)
#' # Counts by period, binned by age, stratified by education levels
#' plot_binned_counts(toy_data, period, bin_by = age, n_bins = 4, stratify_by = education)
#' # Counts by period, binned by age, stratified by education levels, for each sex
#' plot_binned_counts(toy_data, period, bin_by = age, n_bins = 4, stratify_by = education, for_each = sex)
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
                               y_breaks    = NULL) {

  x_q      <- enquo(x)
  bin_by_q <- enquo(bin_by)
  strat_q  <- enquo(stratify_by)
  each_q   <- enquo(for_each)

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
                           y_breaks    = y_breaks
        )
      }),
      vals
    )
    return(plots)
  }

  vals    <- pull(data, !!bin_by_q)
  min_v   <- floor(min(vals, na.rm = TRUE))
  max_v   <- ceiling(max(vals, na.rm = TRUE))

  if (!is.null(bin_width)) {
    if (!missing(n_bins) && !is.null(n_bins)) {
      message("Both 'bin_width' and 'n_bins' provided; using 'bin_width' and ignoring 'n_bins'.")
    }
    brk <- seq(min_v, max_v + bin_width, by = bin_width)
  } else {
    seq_raw <- seq(min_v, max_v, length.out = n_bins + 1)
    brk     <- unique(floor(seq_raw))
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
      y        = y_lab %||% "Count",
      color    = legend_title %||% paste(bin_by_name, "bin")
    ) +
    scale_color_viridis_d(option = "D", direction = -1) +
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
#'
#' @return A \link[ggplot2]{ggplot} object displaying the mean of the response across the variable supplied in \code{x},
#'   optionally stratified by \code{stratify_by}. If \code{for_each} is supplied, separate plots are created  in separate windows for each level.
#'   Visuals can be modified with \pkg{ggplot2}.
#'
#' @export
#'
#'
#' @examples
#' data("toy_data")
#' # Mean by age
#' plot_mean_response_1D(toy_data, response = count, x = age)
#' # Mean count by age, stratified by education
#' plot_mean_response_1D(toy_data, response = count, x = age, stratify_by = education)
#' # Mean count by age, stratified by education, for each sex
#' plot_mean_response_1D(toy_data, response = count, x = age, stratify_by = education, for_each = sex)
plot_mean_response_1D <- function(data,
                                  response,
                                  x,
                                  stratify_by = NULL,
                                  for_each    = NULL,
                                  title       = NULL,
                                  subtitle    = NULL,
                                  legend_title = NULL,
                                  x_lab       = NULL,
                                  y_lab       = NULL) {

  resp_q   <- enquo(response)
  x_q      <- enquo(x)
  strat_q  <- enquo(stratify_by)
  each_q   <- enquo(for_each)

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
          subtitle      = paste0(as_name(each_q), ": ", val),
          legend_title  = legend_title,
          x_lab         = x_lab,
          y_lab         = y_lab
        )
      }),
      vals
    )
    return(plots)
  }

  resp_nm  <- as_label(resp_q)
  x_nm     <- as_label(x_q)

  grouping <- list(x_q)
  if (has_strat) grouping <- append(grouping, list(strat_q))

  summary_df <- data %>%
    group_by(!!!grouping) %>%
    summarise(mean_val = mean(!!resp_q, na.rm = TRUE), .groups = "drop")

  p <- ggplot(summary_df, aes(x = !!x_q, y = mean_val))
  if (has_strat) {
    p <- p +
      aes(color = !!strat_q, group = !!strat_q) +
      scale_color_viridis_d(name = if (has_strat) (if (!is.null(legend_title)) legend_title else as_label(strat_q)) else NULL,
                            option = "D", direction = -1)
  }
  p <- p + geom_line(linewidth = 1)

  p <- p +
    labs(
      title = if (!is.null(title)) title else paste0("Mean ", resp_nm),
      subtitle = subtitle,
      x     = if (!is.null(x_lab)) x_lab else x_nm,
      y     = if (!is.null(y_lab)) y_lab else paste("Mean", resp_nm),
    ) +
    theme(
      plot.title     = element_text(hjust = 0.5),
      plot.subtitle     = element_text(hjust = 0.5),
      axis.text.x    = element_text(angle = 45)
    )

  return(p)
}
