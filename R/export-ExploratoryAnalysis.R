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
#' @importFrom dplyr %>% distinct anti_join across all_of
#' @importFrom utils tail
#' @importFrom stats reformulate
NULL

###############################################################################
## Function for plotting missing groups

#' Plot Missing Group Combinations
#'
#' Creates a tile plot highlighting combinations of grouping variables
#' that are expected but missing from the data. Allows for faceting.
#'
#' @param df Data frame.
#' @param group_vars Character vector of column names to define groups.
#' @param x_var Character string: grouping variable for the x-axis.
#' @param y_var Character string: grouping variable for the y-axis.
#' @param facet_rows Character string: grouping variable for facet rows (optional).
#' @param facet_cols Character string: grouping variable for facet columns (optional).
#' @param facet_labeller A `labeller` function (e.g. \code{\link[ggplot2]{labeller}}),
#'   or a named list where names match facet variables and values are named
#'   vectors/lists mapping levels to labels (optional).
#' @param x_breaks Vector of values for x-axis breaks (optional).
#' @param y_breaks Vector of values for y-axis breaks (optional).
#' @param plot_title Character string for the plot title.
#' @param x_lab Character string for the x-axis label.
#' @param y_lab Character string for the y-axis label.
#'
#' @return A ggplot object, or NULL if no missing combinations found.
#' @import ggplot2
#' @import dplyr
#' @export

plot_missing_APC_strata <- function(df, group_vars, x_var, y_var,
                                facet_rows = NULL, facet_cols = NULL,
                                facet_labeller = NULL,
                                x_breaks = NULL, y_breaks = NULL,
                                plot_title = "Missing Data Combinations",
                                x_lab = x_var, y_lab = y_var) {

  # --- Input Checks (Basic) ---
  if(!is.data.frame(df)) {stop("The input 'df' must be a data frame.")}
  if(!is.character(group_vars)) {stop("Argument 'group_vars' must be a string or vector of strings, name of grouping variables.")}
  check_cols_exist(c(group_vars), df)
  if(!is.character(x_var) || !length(x_var)==1) {stop("Argument 'x_var' must be a single string, name of x-axis variable.")}
  if(!x_var %in% group_vars) {stop("Argument 'x_var' must be one of the grouping variables in 'group_vars'." )}
  if(!is.character(y_var) || !length(y_var)==1) {stop("Argument 'y_var' must be a single string, name of y-axis variable.")}
  if(!y_var %in% group_vars) {stop("Argument 'y_var' must be one of the grouping variables in 'group_vars'." )}
  if(x_var == y_var){stop("'x_var' and 'y_var' cannot be the same variable.")}
  if (!is.null(facet_rows)) stopifnot(is.character(facet_rows), length(facet_rows) == 1, facet_rows %in% group_vars)
  if (!is.null(facet_cols)) stopifnot(is.character(facet_cols), length(facet_cols) == 1, facet_cols %in% group_vars)


  # --- Calculate Missing Groups ---
  eg <- expected_groups(df, group_vars)
  expected_df   <- eg$grid
  expected_list <- eg$levels

  actual_groups <- df %>% distinct(across(all_of(group_vars)))

  missing_groups <- anti_join(expected_df, actual_groups, by = group_vars)

  if (nrow(missing_groups) == 0) {
    message("No missing group combinations found for the specified variables.")
    return(NULL)
  }

  plot_facet_vars <- unique(c(x_var, y_var, facet_rows, facet_cols))
  for (col in intersect(group_vars, plot_facet_vars)) {
    # Ensure the levels exist in expected_list before assigning
    if(col %in% names(expected_list)) {
      missing_groups[[col]] <- factor(missing_groups[[col]], levels = expected_list[[col]])
    } else {
      # This case shouldn't happen if logic is correct, but as safety:
      missing_groups[[col]] <- factor(missing_groups[[col]])
    }
  }
  # Also ensure plotting vars are factors, even if not in facets (redundant if always in group_vars)
  if (x_var %in% names(expected_list)) {
    missing_groups[[x_var]] <- factor(missing_groups[[x_var]], levels = expected_list[[x_var]])
  }
  if (y_var %in% names(expected_list)) {
    missing_groups[[y_var]] <- factor(missing_groups[[y_var]], levels = expected_list[[y_var]])
  }


  # --- Determine Facet Formula ---
  facet_formula <- NULL
  if (!is.null(facet_rows) || !is.null(facet_cols)) {
    rows_str <- if (is.null(facet_rows)) "." else facet_rows
    cols_str <- if (is.null(facet_cols)) "." else facet_cols
    if (rows_str != "." || cols_str != ".") {
      facet_formula <- stats::as.formula(paste(rows_str, "~", cols_str))
    }
  } else {
    remaining <- setdiff(group_vars, c(x_var, y_var))
    if (length(remaining) == 1) {
      facet_formula <- stats::as.formula(paste("~", remaining[1]))
    } else if (length(remaining) >= 2) {
      facet_formula <- stats::as.formula(paste(remaining[1], "~", remaining[2]))
      # message(paste("Defaulting to facets:", remaining[1], "~", remaining[2])) # Keep message optional
    }
  }

  # --- Process Facet Labeller ---
  final_labeller <- NULL
  if (!is.null(facet_labeller)) {
    # 1. If they passed a pure function, use it directly
    if (is.function(facet_labeller)) {
      final_labeller <- facet_labeller

      # 2. If they passed a named list, either named vectors or lists
    } else if (is.list(facet_labeller)) {
      # Convert any inner lists to named character vectors
      facet_labeller_clean <- lapply(facet_labeller, function(m) {
        # if it's a list, unlist it
        if (is.list(m)) m <- unlist(m, use.names = TRUE)
        # must be character + named
        if (!is.character(m) || is.null(names(m))) {
          stop("Each element of `facet_labeller` must be a named character vector or a list coercible to one.")
        }
        m
      })
      # Now build a ggplot labeller
      final_labeller <- ggplot2::as_labeller(facet_labeller_clean)

    } else {
      warning("`facet_labeller` needs to be either a function or a named list. Ignoring and using default.")
    }
  }

  # --- Build Facet Layer ---
  if (!is.null(facet_formula)) {
    facet_args <- list(facet_formula, drop = FALSE)
    if (!is.null(final_labeller)) {
      facet_args$labeller <- final_labeller
    }
    facet_layer <- do.call(ggplot2::facet_grid, facet_args)
  }

  # --- Build the ggplot ---
  p <- ggplot(missing_groups, aes(x = .data[[x_var]], y = .data[[y_var]])) +
    geom_tile(fill = "red", color = "white", alpha = 0.8) +
    labs(title = plot_title, x = x_lab, y = y_lab) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x  = element_text(angle = 45, hjust = 1, vjust = 1),
      axis.text.y  = element_text(),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(margin = margin(r = 10)),
      plot.title   = element_text(size = rel(1.5), hjust = 0.5),
      panel.border = element_rect(color = "grey80", fill = NA, linewidth = 0.5),
      strip.background = element_rect(fill = "grey90", color = "grey80"),
      strip.text = element_text(face = "bold"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )

  # --- Calculate Valid Breaks ---
  current_x_levels <- levels(missing_groups[[x_var]])
  current_y_levels <- levels(missing_groups[[y_var]])

  valid_x_breaks <- NULL
  if (!is.null(x_breaks)) {
    valid_x_breaks <- intersect(as.character(x_breaks), current_x_levels)
    if (length(valid_x_breaks) == 0) valid_x_breaks <- NULL
  }

  valid_y_breaks <- NULL
  if (!is.null(y_breaks)) {
    valid_y_breaks <- intersect(as.character(y_breaks), current_y_levels)
    if (length(valid_y_breaks) == 0) valid_y_breaks <- NULL
  }

  # --- Apply Scales --- ## THIS IS THE CORRECTED SECTION ##
  # Build scale arguments dynamically
  scale_x_args <- list(limits = current_x_levels, drop = FALSE)
  if (!is.null(valid_x_breaks)) {
    scale_x_args$breaks <- valid_x_breaks
  }

  scale_y_args <- list(limits = current_y_levels, drop = FALSE)
  if (!is.null(valid_y_breaks)) {
    scale_y_args$breaks <- valid_y_breaks
  }

  # Add the scales using do.call
  p <- p +
    do.call(scale_x_discrete, scale_x_args) +
    do.call(scale_y_discrete, scale_y_args)


  # --- Add Facet Layer ---
  if (!is.null(facet_layer)) {
    p <- p + facet_layer
  }

  return(p)
}


##############################################################################

#' Plot counts of a factor across binned numeric intervals
#'
#' Given a data frame, a numeric variable to bin (e.g. age, year, birth_year)
#' and a discrete (factor or character) grouping variable (e.g. education, race),
#' this function will:
#'   1. Create either equal-width bins of size `bin_size`, or use the provided
#'      numeric vector `bins` as breakpoints;
#'   2. Count the number of rows in each bin x group;
#'   3. Return a ggplot2 bar chart (dodged) of those counts.
#'
#' @param df A data frame containing at least \code{time_var} and \code{group_var}.
#' @param time_var String name of the numeric variable to bin.
#' @param group_var String name of the discrete grouping variable.
#' @param bin_size Single number giving the desired bin width.  Bins will be
#'   created from \code{floor(min(df[[time_var]]))} up to ceiling(max(...)), with
#'   a final shorter bin if needed.  Ignored if \code{bins} is supplied.
#' @param bins Optional numeric vector of breakpoints to pass to \code{cut(..., breaks = bins)}.
#' @param labels Optional character vector of labels for the bins; if NULL, labels
#'   are auto-generated by \code{cut()}.
#' @param plot_title Title for the plot.  Defaults to
#'   "Count of <\code{group_var}> by binned <\code{time_var}>".
#' @param x_lab Label for the x-axis (bins).  Defaults to \code{time_var}.
#' @param y_lab Label for the y-axis.  Defaults to "Count".
#'
#' @return A ggplot2 object (bar chart).  Also (invisibly) returns the
#'   underlying data.frame of counts as the \code{"data"} attribute.
#'
#' @import dplyr
#' @import ggplot2
#' @export
#'
#' @examples
#' df <- data.frame(
#'   age    = sample(25:85, 500, replace = TRUE),
#'   educ   = sample(c("HS","BA","MA","PhD"), 500, replace = TRUE)
#' )
#' # auto 10-year bins:
#' plot_binned_counts(df, "age", "educ", bin_size = 10)
#'
#' # custom bins:
#' my_bins   <- c(25, 35, 50, 65, 85)
#' my_labels <- c("25-34", "35-49", "50-64", "65-84")
#' plot_binned_counts(df, "age", "educ", bins = my_bins, labels = my_labels,
#'                    plot_title = "Custom age bins by education")
plot_binned_counts <- function(df,
                               time_var,
                               group_var,
                               bin_size = NULL,
                               bins     = NULL,
                               labels   = NULL,
                               plot_title = NULL,
                               x_lab      = NULL,
                               y_lab      = "Count") {
  # inputs
  if (!is.data.frame(df)) stop("'df' must be a data frame.")
  if (!is.character(time_var) || length(time_var) != 1) stop("'time_var' must be a single string.")
  if (!is.character(group_var) || length(group_var) != 1) stop("'group_var' must be a single string.")
  if (is.null(bins) && is.null(bin_size)) stop("must supply either 'bins' or 'bin_size'")
  if (!is.null(bins) && (!is.numeric(bins) || length(bins) < 2))
    stop("'bins' must be a numeric vector of length less than or equal to 2")
  if (!is.null(bin_size) && (!is.numeric(bin_size) || length(bin_size) != 1))
    stop("'bin_size' must be a single number")

  # compute breakpoints if needed
  if (is.null(bins)) {
    mn <- floor(min(df[[time_var]], na.rm = TRUE))
    mx <- ceiling(max(df[[time_var]], na.rm = TRUE))
    # ensure we cover the full range
    bks <- seq(mn, mx, by = bin_size)
    if (tail(bks,1) < mx) bks <- c(bks, mx)
  } else {
    bks <- bins
  }

  # default labels & plot title
  if (is.null(labels)) {
    # let cut() auto-generate
    lbls <- NULL
  } else {
    if (length(labels) != length(bks) - 1)
      stop("length(labels) must be length(bins) - 1")
    lbls <- labels
  }
  if (is.null(plot_title)) {
    plot_title <- sprintf("Count of %s by binned %s", group_var, time_var)
  }
  if (is.null(x_lab)) x_lab <- time_var

  # bin & count
  df2 <- df %>%
    mutate(
      bin = cut(.data[[time_var]],
                breaks = bks,
                right = FALSE,
                include.lowest = TRUE,
                labels = lbls)
    ) %>%
    count(bin, .data[[group_var]], name = "n")

  # build plot
  p <- ggplot(df2, aes(x = bin, y = n, fill = factor(.data[[group_var]]))) +
    geom_col(position = "dodge") +
    labs(
      title    = plot_title,
      x        = x_lab,
      y        = y_lab,
      fill     = group_var
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x  = element_text(angle = 45, hjust = 1),
      axis.title.x = element_text(margin = margin(t = 8)),
      axis.title.y = element_text(margin = margin(r = 8))
    )

  # attach the data invisibly
  attr(p, "data") <- df2
  return(p)
}

###############################################################################

#' Plot observation counts by education and race across binned time intervals
#'
#' Takes a data frame, one numeric "time" variable (e.g. age, year, birth_year),
#' and two discrete grouping variables (education and race). Internally it:
#'   1. Constructs bins (either equal-width via `bin_size`, or from your
#'      explicit `bins` vector);
#'   2. Labels them (auto or via your `labels`);
#'   3. Counts rows in each time_bin x education x race;
#'   4. Produces a dodged bar chart faceted by race.
#'
#' @param df A data frame containing at least the columns named by `time_var`,
#'   `education_var`, and `race_var`.
#' @param time_var String: name of the numeric variable to bin (e.g. `"age"`,
#'   `"year"`, `"birth_year"`).
#' @param education_var String: name of the education factor variable.
#' @param race_var String: name of the race factor variable.
#' @param bin_size Single numeric: width of equal-sized bins. Ignored if
#'   `bins` is provided.
#' @param bins Optional numeric vector of breakpoints to use instead of
#'   `bin_size`. Must be length less than or equal to 2.
#' @param labels Optional character vector of length `length(bins) - 1` for
#'   custom bin labels.
#' @param plot_title Title for the plot. Defaults to
#'   `"Counts by <time_var> bins, education and race"`.
#' @param x_lab Label for the x-axis. Defaults to `"<time_var> bin"`.
#' @param y_lab Label for the y-axis. Defaults to `"Count"`.
#'
#' @return A ggplot2 bar-chart object. Invisibly, the underlying counts
#'   data frame is attached as `attr(<plot>,"data")`.
#'
#' @import dplyr
#' @import ggplot2
#' @importFrom rlang sym
#' @export
plot_counts_edu_race <- function(df,
                                 time_var,
                                 education_var,
                                 race_var,
                                 bin_size = NULL,
                                 bins     = NULL,
                                 labels   = NULL,
                                 plot_title = NULL,
                                 x_lab      = NULL,
                                 y_lab      = "Count") {
  # -- Input checks --
  if (!is.data.frame(df)) stop("'df' must be a data frame.")
  for (nm in c(time_var, education_var, race_var)) {
    if (!nm %in% names(df)) stop("'", nm, "' not found in df")
  }
  if (is.null(bins) && is.null(bin_size))
    stop("must supply either 'bins' or 'bin_size'")
  if (!is.null(bins) && (!is.numeric(bins) || length(bins) < 2))
    stop("'bins' must be a numeric vector of length less than or equal to 2")
  if (!is.null(bin_size) && (!is.numeric(bin_size) || length(bin_size) != 1))
    stop("'bin_size' must be a single number")

  # -- Compute breakpoints --
  if (is.null(bins)) {
    mn <- floor(min(df[[time_var]], na.rm = TRUE))
    mx <- ceiling(max(df[[time_var]], na.rm = TRUE))
    bks <- seq(mn, mx, by = bin_size)
    if (tail(bks,1) < mx) bks <- c(bks, mx)
  } else {
    bks <- bins
  }

  # -- Labels & titles defaults --
  if (is.null(labels)) lbls <- NULL else {
    if (length(labels) != length(bks)-1)
      stop("length(labels) must equal length(bins)-1")
    lbls <- labels
  }
  if (is.null(plot_title)) {
    plot_title <- sprintf(
      "Counts by %s bins, education and race",
      time_var
    )
  }
  if (is.null(x_lab)) x_lab <- paste0(time_var, " bin")

  race_labels <- c(
    "1" = "White",
    "2" = "Black",
    "3" = "Hispanic",
    "4" = "Other"
  )

  labeller_list <- setNames(list(race_labels), race_var)

  # -- Bin and count --
  df2 <- df %>%
    mutate(
      time_bin = cut(
        .data[[time_var]],
        breaks          = bks,
        right           = FALSE,
        include.lowest  = TRUE,
        labels          = lbls
      )
    ) %>%
    count(
      time_bin,
      .data[[education_var]],
      .data[[race_var]],
      name = "n"
    )

  # -- Plot --
  p <- ggplot(df2, aes(
    x    = time_bin,
    y    = n,
    fill = factor(.data[[education_var]])
  )) +
    geom_col(position = "dodge") +
    labs(
      title = plot_title,
      x     = x_lab,
      y     = y_lab,
      fill  = education_var
    ) +
    scale_fill_discrete(name = "Education") +
    facet_wrap(
      reformulate(race_var),
      labeller = labeller(.cols = labeller_list)
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x   = element_text(angle = 45, hjust = 1, margin = margin(t = 8)),
      axis.title.x  = element_text(margin = margin(t = 8)),
      axis.title.y  = element_text(margin = margin(r = 8)),
      plot.title    = element_text(size = 16, hjust = 0.5),
      legend.title  = element_text(size = 12),
      strip.text    = element_text(size = 12)
    )

  # attach data
  attr(p, "data") <- df2
  return(p)
}
