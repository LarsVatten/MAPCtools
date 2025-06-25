##############################################################################
### Imports:

#' @importFrom dplyr %>% setdiff mutate across select distinct arrange all_of
#' @importFrom dplyr group_by rename n_groups bind_cols cur_data cur_column
#' @importFrom dplyr n tibble relocate summarise inner_join bind_rows group_vars
#' @importFrom dplyr group_modify group_vars pull is_grouped_df ungroup row_number
#' @importFrom tidyselect all_of everything matches eval_select
#' @importFrom rlang syms sym expr !!! eval_tidy := enquo as_name
#' @importFrom stats as.formula na.omit
#' @importFrom fastDummies dummy_cols
#' @importFrom tidyr expand_grid
#' @importFrom stats setNames
#' @importFrom tibble as_tibble
NULL
##############################################################################

### Function for adding cohort to data frame

#' Add cohort column to data frame
#'
#' Adds a column for birth cohorts to a data frame, derived from specified age and period columns through the relation \code{cohort = period - age}.
#'
#' @param data Data frame with age and period column.
#' @param age Age column in \code{data}.
#' @param period Period column in \code{data}.
#' @param cohort_name Name of the cohort column to be created. Defaults to \code{"cohort"}.
#' @return Data frame with additional column for birth cohorts added.
#' @keywords internal

add_cohort_column <- function(data, age, period, cohort_name="cohort") {


  age_q <- resolve_column(enquo(age))
  period_q <- resolve_column(enquo(period))

  age_name <- as_name(age_q)
  period_name <- as_name(period_q)

  if (!all(c(age_name, period_name) %in% names(data))) {
    stop("The specified age and/or period columns do not exist in the data frame.")
  }

  data <- data %>%
    mutate(!!cohort_name := !!period_q - !!age_q) %>%
    relocate(!!age_q, !!period_q, !!sym(cohort_name), .before = everything())

  return(data)
}

##############################################################################
### Function for adding cohort index to data frame

#' Add cohort column to data frame
#'
#' Adds a column for cohort indices to a data frame, derived from specified age and period index columns through the relationship \code{cohort index = period index - age index + max(age index)}.
#'
#' @param data Data frame with age and period columns.
#' @param age_index Age index column in \code{data}.
#' @param period_index Period index column in \code{data}.
#' @param cohort_name Name of the cohort index column to be created. Defaults to \code{"cohort_index"}.
#' @return Data frame with additional column for cohort indices.
#' @keywords internal

add_cohort_index <- function(data, age_index, period_index, cohort_name="cohort_index") {


  age_q <- resolve_column(enquo(age_index))
  period_q <- resolve_column(enquo(period_index))

  age_name <- as_name(age_q)
  period_name <- as_name(period_q)

  if (!all(c(age_name, period_name) %in% names(data))) {
    stop("The specified age and/or period columns do not exist in the data frame.")
  }

  data <- data %>%
    mutate(
      !!cohort_name := as.integer(!!period_q - !!age_q + max(!!age_q))
    ) %>%
    relocate(!!age_q, !!period_q, !!sym(cohort_name), .before = everything())
  return(data)
}

##############################################################################

#' Add 1-indexed age, period and cohort indices via match()
#'
#' @param df            Data frame
#' @param age_name      Name of the age (or age-group) column (string).
#' @param period_name   Name of the period (e.g. year) column (string).
#' @param age_order     Character vector giving the desired ordering of age levels
#' @param period_order  Vector (numeric or character) giving the desired ordering of periods
#' @param M             Grid factor, defined as the ratio of age interval width to period interval width.
#' @return              Data frame with new columns: age_index, period_index, cohort_index
#' @keywords internal
add_APC_by_match <- function(df,
                             age_name,
                             period_name,
                             age_order,
                             period_order,
                             M = 1) {
  df %>%
    mutate(
      age_index    = match(.data[[age_name]],    age_order),
      period_index = match(.data[[period_name]], period_order)
    ) %>%
    mutate(
      cohort_index = M * (max(age_index, na.rm=TRUE) - age_index) + period_index
    ) %>%
    arrange(age_index, period_index)
}

#' Add 1-indexed APC columns to data frame, handling numeric or categorical age/period
#'
#'
#' @param data            Data frame with age and period columns.
#' @param age             Age column in \code{data}.
#' @param period          Period column in \code{data}.
#' @param age_order       (Optional) Character vector giving the desired order of age levels.
#'                        If NULL and the \code{age} column is factor/character, uses \code{unique(sort(data[[age]]))}.
#' @param period_order    (Optional) Vector (numeric or character) giving the desired order of periods.
#'                        If NULL and \code{period} column is a factor/character, uses \code{unique(sort(data[[period]]))}.
#' @param M               Grid factor, defined as the ratio of age interval width to period interval width.
#'                        Defaults to 1 (i.e. assuming equal sized age and period increments).
#' @return                The data frame with new columns \code{age_index}, \code{period_index}, \code{cohort_index},
#'                        and sorted by \code{(age_index, period_index)}.
#' @export
as.APC.df <- function(data,
                      age,
                      period,
                      age_order     = NULL,
                      period_order  = NULL,
                      M             = 1) {


  age_q    <- enquo(age)
  period_q <- enquo(period)

  age_name    <- as_name(age_q)
  period_name <- as_name(period_q)

  if (!all(c(age_name, period_name) %in% names(data)))
    stop("The specified age and/or period columns do not exist in the data frame.")

  age_vec    <- data[[as_name(enquo(age))]]
  period_vec <- data[[as_name(enquo(period))]]

  use_match <- (!is.numeric(age_vec) || !is.numeric(period_vec)) ||
    !is.null(age_order)  || !is.null(period_order)

  if (use_match) {
    if (is.null(age_order))    age_order    <- sort(unique(age_vec))
    if (is.null(period_order)) period_order <- sort(unique(period_vec))

    return(
      add_APC_by_match(data,
                       age_name      = age_name,
                       period_name   = period_name,
                       age_order    = age_order,
                       period_order = period_order,
                       M            = M)
    )
  }

  age_idx    <- as.integer(age_vec    - min(age_vec,    na.rm=TRUE) + 1)
  period_idx <- as.integer(period_vec - min(period_vec, na.rm=TRUE) + 1)
  cohort_idx <- period_idx - age_idx + max(age_idx, na.rm=TRUE)

  data$age_index    <- age_idx
  data$period_index <- period_idx
  data$cohort_index <- cohort_idx

  data %>%
    relocate(
      age_index,
      period_index,
      cohort_index,
      !!age_q,
      !!period_q,
      .before = everything()
    ) %>%
    arrange(!!age_q, !!period_q)
}

###################################################################################
# Function for aggregating entire data frame

#' Aggregate data across an entire data frame using sufficient statistics
#'
#' Aggregates specified columns of a data frame into summarizing statistics,
#' preserving the potentially complex structure returned by aggregator functions
#' (like data frames or inla.mdata objects) within list-columns.
#' Aggregation is performed according to sufficient statistics for the specified
#' distribution of the columns. Possible distributions: Gaussian, binomial.
#' This function aggregates the entire data frame into a single row result.
#'
#' @param data A data frame.
#' @param gaussian Gaussian columns in \code{data} to be aggregated. The Gaussian observations are collapsed into an \code{inla.mdata} object compatible with the \code{agaussian} family, see the documentation for the \code{agaussian} family in \code{INLA} for details.
#'   **Defaults to \code{NULL} (optional).**
#' @param gaussian.precision.scales Scales for the precision of Gaussian observations. \cr
#'   Must be one of: \cr
#'   - \code{NULL}: Use default scales of 1 for all observations in all \code{gaussian} columns.
#'   - A single numeric vector: Applied only if *exactly one* column is specified in \code{gaussian}. Length must match \code{nrow(data)}.
#'   - A *named list*: Where \code{names(gaussian.precision.scales)} are the names of the Gaussian columns (must match columns specified in \code{gaussian}). Each list element must be a numeric vector of scales for that column, with length matching \code{nrow(data)}. \cr
#'   **Defaults to NULL (optional).**
#' @param binomial Binomial columns in \code{data} to be aggregated.
#'   **Defaults to \code{NULL} (optional).**
#'
#' @return A single-row data frame (tibble) containing:
#'   - A column \code{n} with the total number of rows in the input data.
#'   - For each specified column in \code{gaussian}, \code{binomial}, a corresponding
#'     *list-column* (named e.g., \code{colname_gaussian}, \code{colname_binomial}.
#'     Each element of these list-columns can be accessed by using the \code{$} operator twice, e.g. through \code{data$colname_gaussian$Y1} for the first element of the Gaussian summary.
#'
#' @export

aggregate_df <- function(data,
                         gaussian = NULL,
                         gaussian.precision.scales = NULL,
                         binomial = NULL) {

  if (!requireNamespace("INLA", quietly = TRUE)) {
    stop("This function requires the INLA package. Please install it using:\n",
         'install.packages("INLA", repos = c(INLA = "https://inla.r-inla-download.org/R/stable"))')
  }

  if (!is.data.frame(data)) {
    stop("'data' must be a data frame.")
  }
  n_rows <- nrow(data)
  if (n_rows == 0) {
    stop("'data' has 0 rows.")
  }

  gaussian_q <- enquos(gaussian, .ignore_empty = "all")
  binomial_q <- enquos(binomial, .ignore_empty = "all")

  gaussian <- names(eval_select(expr(c(!!!gaussian_q)), data))
  binomial <- names(eval_select(expr(c(!!!binomial_q)), data))

  processed_scales <- list()
  scales_q <- enquo(gaussian.precision.scales)

  if (!quo_is_null(scales_q)) {
    scales_val <- eval_tidy(scales_q, data = data)

    scales_val <- as.numeric(scales_val)

    if (is.numeric(scales_val) && is.vector(scales_val)) {
      processed_scales <- setNames(
        rep(list(scales_val), length(gaussian)),
        gaussian
      )
    } else if (is.list(scales_val)) {
      if (!all(names(scales_val) %in% gaussian)) {
        stop("Names of gaussian.precision.scales must match gaussian columns.")
      }
      processed_scales <- scales_val
    } else {
      stop("gaussian.precision.scales must resolve to a numeric vector or a named list of numeric vectors.")
    }

    bad <- names(processed_scales)[
      vapply(processed_scales, function(x) length(x) != n_rows, logical(1))
    ]
    if (length(bad)) {
      stop("precision scales for columns ", paste(bad, collapse=", "), " are not length ", n_rows)
    }
  }

  count_summary <- tibble(n = n_rows)

  if (length(gaussian) > 0) {
    gaussian_summary <- data %>%
      summarise(
        across(
          all_of(gaussian),
          ~ gaussian_aggregator(
            y = .x,
            precision.scale = processed_scales[[cur_column()]]
          ),
          .names = "{col}_gaussian"
        )
      )
  } else {
    gaussian_summary <- tibble(.rows = 1)
  }

  if (!is.null(binomial) && length(binomial) > 0) {
    binomial_summary <- data %>%
      summarise(across(all_of(binomial),
                       ~ binomial_aggregator(data = cur_data(), col_name = cur_column()),
                       .names = "{col}_binomial"))
  } else {
    binomial_summary <- tibble(.rows = 1)
  }

  aggregated_data <- bind_cols(
    count_summary,
    gaussian_summary,
    binomial_summary
  ) %>% as_tibble()

  return(aggregated_data)
}

##############################################################################
# Function for aggregating data within groups

#' Aggregate grouped data using aggregate_df
#'
#' Aggregates a grouped data frame into summarizing statistics within groups by
#' applying the \code{\link{aggregate_df}} function to each group.
#' Aggregation is performed according to sufficient statistics for the specified
#' distribution of the columns to be aggregated.
#'
#' @param data Data frame to be grouped and aggregated.
#' @param by Columns in \code{data} for which to group data by.
#' @param gaussian Gaussian columns in \code{grouped_data} to be aggregated. \cr
#'   **Defaults to \code{NULL} (optional).**
#' @param gaussian.precision.scales Scales for the precision of Gaussian observations. \cr
#'   See \code{\link{aggregate_df}} documentation for format details, and \code{agaussian} in \code{INLA} for more details.
#'   **Defaults to NULL.**
#' @param binomial Binomial columns in \code{grouped_data} to be aggregated. \cr
#'   **Defaults to \code{NULL} (optional).**
#'
#' @return Aggregated data frame (tibble), with one row per group, containing
#'   grouping variables, count \code{n} per group, and aggregated list-columns for
#'   specified variables as returned by \code{\link{aggregate_df}}.
#'
#' @keywords internal

aggregate_grouped_df <- function(data,
                                 by,
                                 gaussian = NULL,
                                 gaussian.precision.scales = NULL,
                                 binomial = NULL) {

  data <- data %>% mutate(.row_id___ = row_number())

  gb_expr  <- enquo(by)
  gb_names <- names(eval_select(expr(c(!!gb_expr)), data))

  grouped_data <- data %>% group_by(!!!syms(gb_names))

  gaussian_cols <- names(eval_select(expr(c(!!!enquos(gaussian,
                                                      .ignore_empty = "all"))),
                                     data))
  binomial_cols <- names(eval_select(expr(c(!!!enquos(binomial,
                                                      .ignore_empty = "all"))),
                                     data))

  full_scales <- eval_tidy(enquo(gaussian.precision.scales), data = data)

  aggregated <- grouped_data %>%
    group_modify(~{
      df <- .x
      scales_this_grp <- NULL
      if (!is.null(full_scales)) {
        idx <- df$.row_id___

        if (is.numeric(full_scales)) {
          scales_this_grp <- full_scales[idx]

        } else if (is.list(full_scales)) {
          scales_this_grp <- lapply(full_scales, `[`, idx)

        } else {
          stop("`gaussian.precision.scales` must be a numeric vector ",
               "or a named list of numeric vectors.")
        }
      }

      aggregate_df(
        df %>% select(-.row_id___),           # drop helper column
        gaussian  = gaussian_cols,
        gaussian.precision.scales = scales_this_grp,
        binomial  = binomial_cols
      )
    }) %>%
    ungroup()

  aggregated
}


##############################################################################
# Function for creating NA structure in APC data frame.

#' Create NA structure across age, period and cohort groups based on strata
#'
#' Creates a data frame where age, period, and cohort values are placed into
#' columns specific to their stratum (defined by `stratify_var`), with other
#' strata combinations marked as NA. This structure is often useful for
#' specific modeling approaches, like certain Age-Period-Cohort (APC) models.
#' Optionally includes unique indices for random effects.
#'
#' @param data Data frame with age, period, cohort, and stratification columns.
#' @param stratify_by Stratification variable column. This column
#'   will be used to create the stratum-specific NA structure. It should ideally
#'   be a factor or character vector.
#' @param age Age column in \code{data} (must be a numeric/integer column).
#' @param period Name of the period column (must be a numeric/integer column).
#' @param cohort Name of the cohort column (must be a numeric/integer column).
#' @param include.random Logical. Whether to include a unique index ('random')
#'   for each combination of age, period, and stratum, potentially for use as
#'   random effect identifiers in models. **Defaults to FALSE.**
#'
#' @return A data frame containing the original \code{age}, \code{period},
#'   \code{cohort}, and \code{stratify_by} columns, plus:
#'   - Dummy indicator columns for each level of \code{stratify_by} (e.g., \code{Region_North}, \code{Region_South} if \code{Region} was a stratifying variable).
#'   - Stratum-specific age, period, and cohort columns (e.g., \code{age_Region_North},
#'     \code{period_Region_North}, \code{cohort_Region_North}), containing the respective
#'     value if the row belongs to that stratum, and \code{NA} otherwise.
#'   - If \code{include.random = TRUE}, a column named \code{random} with unique integer indices.
#'   The rows are ordered primarily by the stratification variable levels. This is useful for defining random components in MAPC models.
#'
#' @export

as.APC.NA.df <- function(data,
                         stratify_by,
                         age,
                         period,
                         cohort,
                         include.random = FALSE) {

  if (!is.data.frame(data)) {
    stop("'data' must be a data frame.")
  }
  if (nrow(data) == 0) {
    stop("'data' must not be empty.")
  }

  stratify_q <- resolve_column(enquo(stratify_by))
  stratify_name <- as_name(stratify_q)

  age_q <- resolve_column(enquo(age))
  period_q <- resolve_column(enquo(period))
  cohort_q <- resolve_column(enquo(cohort))

  age_name <- as_name(age_q)
  period_name <- as_name(period_q)
  cohort_name <- as_name(cohort_q)

  required_cols <- c(age_name, period_name, cohort_name)
  check_cols_exist(required_cols, data)

  if (!is.numeric(data[[age_name]])) {
    warning(paste0("Column '", age_name, "' is not numeric. Coercing to numeric. NAs may be introduced."))
    data[[age_name]] <- suppressWarnings(as.numeric(data[[age_name]]))
    if (all(is.na(data[[age_name]]))) stop(paste("Coercion failed: Column '", age_name, "' could not be converted to numeric."))
  }
  if (!is.numeric(data[[period_name]])) {
    warning(paste0("Column '", period_name, "' is not numeric. Coercing to numeric. NAs may be introduced."))
    data[[period_name]] <- suppressWarnings(as.numeric(data[[period_name]]))
    if (all(is.na(data[[period_name]]))) stop(paste("Coercion failed: Column '", period_name, "' could not be converted to numeric."))
  }
  if (!is.numeric(data[[cohort_name]])) {
    warning(paste0("Column '", cohort_name, "' is not numeric. Coercing to numeric. NAs may be introduced."))
    data[[cohort_name]] <- suppressWarnings(as.numeric(data[[cohort_name]]))
    if (all(is.na(data[[cohort_name]]))) stop(paste("Coercion failed: Column '", cohort_name, "' could not be converted to numeric."))
  }

  if (is.numeric(data[[stratify_name]])) {
    warning(paste0("Column '", stratify_name, "' is numeric. Treating each unique number as a separate category. Consider converting to factor or character first if this is not intended."))
  }
  if (all(is.na(data[[stratify_name]]))) {
    stop(paste0("The stratification column '", stratify_name, "' contains only NA values."))
  }

  data[[stratify_name]] <- as.factor(data[[stratify_name]])
  if (nlevels(data[[stratify_name]]) == 0) {
    stop(paste0("The stratification column '", stratify_name, "' has no valid levels after converting to factor (might be all NA or empty)."))
  }

  if (!is.logical(include.random) || length(include.random) != 1) {
    stop("'include.random' must be a single logical value (TRUE or FALSE).")
  }

  dummy_df <- dummy_cols(data, select_columns = stratify_name, remove_first_dummy = FALSE, ignore_na = TRUE)

  dummy_df <- dummy_df %>%
    select(-all_of(c(age_name, period_name, cohort_name)))

  stratify_levels <- grep(paste0("^", stratify_name, "_"), colnames(dummy_df), value = TRUE)

  for (level in stratify_levels) {
    dummy_df[[paste0("age_", level)]] <- dummy_df[[level]] * data[[age_name]]
    dummy_df[[paste0("period_", level)]] <- dummy_df[[level]] * data[[period_name]]
    dummy_df[[paste0("cohort_", level)]] <- dummy_df[[level]] * data[[cohort_name]]
  }

  dummy_df[dummy_df == 0] <- NA

  interaction_cols <- grep(
    paste0("^(age|period|cohort)_", stratify_name, "_"),
    names(dummy_df),
    value = TRUE
  )
  dummy_df <- dummy_df %>%
    select(all_of(stratify_levels), all_of(interaction_cols))

  df_new <- bind_cols(dummy_df, data)

  df_new <- df_new %>% select(!matches("\\.\\.\\.[0-9]+"))  # Removes unwanted ...1, ...2 suffixes

  ordered_rows <- do.call(bind_rows, lapply(stratify_levels, function(level) {
    df_new[!is.na(df_new[[level]]), ]
  }))

  if(include.random) {
    ordered_rows$random <- rep(NA, nrow(ordered_rows))
    i = 1
    stratify_levels <- unique(ordered_rows[[stratify_name]])
    for (age in 1:max(ordered_rows[[age_name]])) {
      for (period in 1:max(ordered_rows[[period_name]])) {
        for (stratum in stratify_levels) {
          ordered_rows$random[ordered_rows[[age_name]] == age &
                                ordered_rows[[period_name]] == period &
                                ordered_rows[[stratify_name]] == stratum] <- i
          i = i + 1
        }
      }
    }
  }

  ordered_rows
  return(as.data.frame(ordered_rows))
}
