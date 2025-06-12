##############################################################################
### Imports:

#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon scale_color_manual
#' @importFrom ggplot2 scale_fill_manual labs theme element_text guides
#' @importFrom ggplot2 guide_legend scale_x_continuous scale_x_discrete
NULL

##############################################################################

#' @title Compute dynamic pretty breaks for continuous x-axis
#' @description Internal helper to compute breaks on a numeric scale,
#'   ensuring the number of breaks is in a desired range and aligned with data bounds.
#' @param x A numeric vector.
#' @param target_n Target number of breaks.
#' @param max_breaks Maximum allowed number of breaks.
#' @param min_breaks Minimum allowed number of breaks.
#' @return A numeric vector of break points.
#' @keywords internal
dynamic_pretty_breaks <- function(x,
                                  target_n  = 8,
                                  max_breaks = 12,
                                  min_breaks = 3) {
  # 1. compute true data range
  x_rng <- range(x, na.rm = TRUE)
  r     <- diff(x_rng)

  # 2. pick "nice" number of intervals
  n_int <- clamp(
    round(r / (r / target_n)),
    lower = min_breaks,
    upper = max_breaks
  )

  # 3. get the pretty breaks (may overshoot at both ends)
  brks <- pretty(x_rng, n = n_int)

  # 4. clamp only the very first break up to the data-min
  if (brks[1] < x_rng[1]) {
    brks[1] <- x_rng[1]
  }

  # 5. drop anything above the data-max (instead of clamping it)
  brks <- brks[ brks <= x_rng[2] ]

  brks
}

#' @title Compute dynamic pretty breaks for discrete x-axis
#' @description Internal helper to select a well-spaced subset of factor levels or unique strings
#'   for axis labeling on a discrete scale.
#' @param x A factor or character vector.
#' @param target_n Target number of breaks.
#' @param max_breaks Maximum allowed number of breaks.
#' @param min_breaks Minimum allowed number of breaks.
#' @return A character vector of selected breaks.
#' @keywords internal
dynamic_pretty_discrete_breaks <- function(x,
                                           target_n   = 8,
                                           max_breaks = 12,
                                           min_breaks = 3) {
  # 1. capture your categories in order
  vals <- unique(as.character(x))
  n    <- length(vals)

  # 2. clamp permissible break-counts
  mb <- max(min_breaks, 2)
  MB <- min(max_breaks, n)
  candidates <- seq(mb, MB)

  # 3. keep only those m where (n-1) divides (m-1) exactly
  exact_ms <- candidates[(n-1) %% (candidates-1) == 0]

  if (length(exact_ms)) {
    # 4a. pick the exact-spacing count closest to your target
    m_final <- exact_ms[which.min(abs(exact_ms - target_n))]
    step    <- (n - 1) / (m_final - 1)
    idx     <- seq(1, n, by = step)

  } else {
    # 4b. fallback: "as even as possible"
    m_final <- clamp(target_n, lower = mb, upper = MB)
    idx     <- unique(round(seq(1, n, length.out = m_final)))
  }

  # 5. return those categories
  vals[as.integer(idx)]
}


#' @title Clamp a numeric value within bounds
#' @description Internal helper to restrict values between a lower and upper bound.
#' @param x A numeric vector.
#' @param lower Lower bound.
#' @param upper Upper bound.
#' @return A numeric vector with values clamped.
#' @keywords internal
clamp <- function(x, lower, upper) pmax(lower, pmin(x, upper))


#' @title Plot time effects with uncertainty ribbons
#' @description Internal plotting helper to visualize median differences over time (or other x-axis),
#'   including HPD interval ribbons and stratified lines.
#' @param data A data frame with columns: x, median_differences, hpd_lower, hpd_upper, Strata.
#' @param x_lab Label for the x-axis.
#' @param y_lab Label for the y-axis.
#' @param family A string indicating model family ("binomial", "poisson", or other).
#' @param color_palette A named vector of colors for strata.
#' @param plot_theme A ggplot2 theme object.
#' @param legend_title Title for the legend.
#' @return A ggplot object.
#' @keywords internal
plot_time_effect <- function(data, x_lab, y_lab, family,
                             color_palette, plot_theme,
                             legend_title = "Strata") {


  if(family=="binomial" | family=="poisson") {
    data$hpd_lower <- exp(data$hpd_lower)
    data$median_differences <- exp(data$median_differences)
    data$hpd_upper <- exp(data$hpd_upper)
  }

  # 2. Build the plot, *including* group = Strata in the global aes
  p <- ggplot(data, aes(x = x, y = median_differences, color = Strata, fill  = Strata, group = Strata)) +
    geom_line(linewidth = 1) +
    geom_ribbon(aes(ymin = hpd_lower, ymax = hpd_upper), alpha = 0.2) +
    plot_theme +
    scale_color_manual(values = color_palette) +
    scale_fill_manual(values = color_palette) +
    labs(x = x_lab, y = y_lab, color = legend_title, fill  = legend_title,
         title = paste("Differences in", tolower(x_lab), "effects")) +
    theme(legend.position = "bottom", legend.title    = element_text(hjust = 0.5)) +
    guides(color = guide_legend(title.position = "top", title.hjust = 0.5),
           fill  = guide_legend(title.position = "top", title.hjust = 0.5))

  # 3. Conditional x-axis
  if (is.numeric(data$x)) {
    p <- p + scale_x_continuous(breaks = dynamic_pretty_breaks(data$x))
  } else {
    p <- p + scale_x_discrete(breaks = dynamic_pretty_discrete_breaks(data$x)) # Uses factor levels
  }

  p
}

