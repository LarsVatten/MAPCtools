###############################################################################
### Imports:

#' @importFrom ggplot2 ggplot facet_grid
#' @importFrom dplyr %>% distinct across all_of
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
#' @param x_var Variable for the x-axis.
#' @param y_var Variable for the y-axis.
#' @param row_var Grouping variable in \code{data} for facet rows (optional).
#' @param col_var Grouping variable for facet columns (optional).
#' @param facet_labeller A \code{labeller} function (e.g. \code{\link[ggplot2]{labeller}}),
#'   or a named list where names match facet variables and values are named
#'   vectors/lists mapping levels to labels (optional).
#' @param x_breaks Vector of values for x-axis breaks (optional).
#' @param y_breaks Vector of values for y-axis breaks (optional).
#' @param title Character string for the plot title. Defaults to "Missing data".
#' @param subtitle Character string for the plot subtitle. Defaults to NULL.
#' @param x_lab Character string for the x-axis label. Defaults to the name of \code{x_var}.
#' @param y_lab Character string for the y-axis label. Defaults to the name of \code{y_var}.
#'
#' @return A ggplot object, or NULL if no missing combinations found.
#' @export

plot_missing_data <- function(data, x_var, y_var,
                              row_var = NULL, col_var = NULL,
                              facet_labeller = NULL,
                              x_breaks = NULL, y_breaks = NULL,
                              title = "Missing data",
                              subtitle = NULL,
                              x_lab = NULL, y_lab = NULL) {
  # Capture and standardize input
  x_var <- ensym(x_var)
  y_var <- ensym(y_var)

  if(missing(row_var)) {
    row_var <- NULL
  } else {
    row_var <- ensym(row_var)
  }

  if(missing(col_var)) {
    col_var <- NULL
  } else {
    col_var <- ensym(col_var)
  }

  x_name <- as_label(x_var)
  y_name <- as_label(y_var)
  row_name <- if (!is.null(row_var)) as_label(row_var) else NULL
  col_name <- if (!is.null(col_var)) as_label(col_var) else NULL

  across_vars <- c(x_name, y_name, row_name, col_name)

  if (x_name == y_name) stop("x_var and y_var cannot be the same")

  # Expected and actual groups
  eg <- expected_groups(data, across_vars)
  expected_df <- eg$grid
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

  # Set factor levels
  for (col in across_vars) {
    plot_df[[col]] <- factor(plot_df[[col]], levels = expected_list[[col]])
  }

  # Facet formula
  facet_formula <- NULL
  if (!is.null(row_name) || !is.null(col_name)) {
    facet_formula <- as.formula(paste(ifelse(is.null(row_name), ".", row_name),
                                      "~",
                                      ifelse(is.null(col_name), ".", col_name)))
  } else {
    remaining <- setdiff(across_vars, c(x_name, y_name))
    if (length(remaining) == 1) {
      facet_formula <- as.formula(paste("~", remaining[1]))
    } else if (length(remaining) >= 2) {
      facet_formula <- as.formula(paste(remaining[1], "~", remaining[2]))
    }
  }

  # Facet labeller
  final_labeller <- NULL
  if (!is.null(facet_labeller)) {
    if (is.function(facet_labeller)) {
      final_labeller <- facet_labeller
    } else if (is.list(facet_labeller)) {
      final_labeller <- as_labeller(lapply(facet_labeller, function(m) {
        if (is.list(m)) m <- unlist(m, use.names = TRUE)
        if (!is.character(m) || is.null(names(m))) {
          stop("Each element must be a named character vector or coercible to one.")
        }
      }))
    }
  }

  facet_layer <- if (!is.null(facet_formula)) {
    do.call(facet_grid, c(list(facet_formula, drop = FALSE),
                          if (!is.null(final_labeller)) list(labeller = final_labeller) else list()))
  } else {
    NULL
  }

  # ggplot
  p <- ggplot(plot_df, aes(x = .data[[x_name]], y = .data[[y_name]], fill = fill_color)) +
    geom_tile(color = "black") +
    scale_fill_identity() +
    labs(
      title = title,
      subtitle = subtitle,
      x = x_lab %||% x_name,
      y = y_lab %||% y_name
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10)),
      plot.title = element_text(size = rel(1.5), hjust = 0.5),
      panel.border = element_rect(color = "grey80", fill = NA, linewidth = 0.5),
      strip.background = element_rect(fill = "grey90", color = "grey80"),
      strip.text = element_text(face = "bold"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )

  current_y <- levels(plot_df[[y_name]])
  current_x <- levels(plot_df[[x_name]])

  # Custom axis breaks (optional)
  if (!is.null(y_breaks)) {
    if (!is.null(names(y_breaks))) {
      y_brks <- intersect(names(y_breaks), current_y)
      y_labs <- y_breaks[y_brks]
    } else {
      y_brks <- y_breaks
      y_labs <- y_brks
    }
  } else {
    y_brks <- pretty(data[[y_name]])
    y_labs <- waiver()
  }

  if (!is.null(x_breaks)) {
    if (!is.null(names(x_breaks))) {
      x_brks <- intersect(names(x_breaks), current_x)
      x_labs <- x_breaks[x_brks]
    } else {
      x_brks <- x_breaks
      x_labs <- x_brks
    }
  } else {
    x_brks <- pretty(data[[x_name]])
    x_labs <- waiver()
  }

  p <- p +
    scale_x_discrete(limits = current_x, breaks = x_brks, labels = x_labs, drop = FALSE) +
    scale_y_discrete(limits = current_y, breaks = y_brks, labels = y_labs, drop = FALSE)

  if (!is.null(facet_layer)) {
    p <- p + facet_layer
  }

  return(p)
}

##############################################################################

