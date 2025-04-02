# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# This file serves as a library for analyzing age-period-cohort data,
# and for fitting Bayesian multivariate APC models with INLA.

load("C:/Users/lavat/OneDrive/Dokumenter/Masteroppgave 2025/Git repo/Masteroppgave2025/Code/Data preprocessing/female_data.RData")

##############################################################################
### Imports:

#' @importFrom dplyr %>% setdiff mutate across select distinct arrange all_of
#' @importFrom dplyr group_by rename n_groups bind_cols cur_data cur_column
#' @importFrom dplyr n tibble relocate summarise inner_join bind_rows group_vars
#' @importFrom dplyr group_modify group_vars pull is_grouped_df
#' @importFrom tidyselect all_of
#' @importFrom rlang syms sym expr !!!
#' @importFrom stats as.formula na.omit
#' @importFrom utils capture.output
#' @importFrom fastDummies dummy_cols
#' @importFrom INLA inla.mdata
#'
##############################################################################

#' @title Group data frame
#'
#' @description Groups data into groups specified by variables \code{group_by} and/or \code{stratify_by}.
#' Grouping is performed using using \code{dplyr}'s \link[dplyr:group_by]{group_by()} method, so that group manipulation method's from \code{dplyr} can be used on the resulting data frame.  \cr
#' At least one of \code{group_by} and \code{stratify_by} must be supplied by the user.
#'
#' @param df A data frame with grouping and/or stratification variables.
#' @param group_by Variables in data frame used to group the data. \cr
#'      **Defaults to empty list (optional).**
#' @param stratify_by Variables in data frame used to stratify the data. \cr
#'      **Defaults to empty list (optional).**
#' @return Grouped data frame.
#' @export

group_df <- function(df, group_by=c(), stratify_by=c()) {

  # Check if both are empty vectors
  if (length(group_by) == 0 && length(stratify_by) == 0) {
    stop("You must supply at least one of `group_by` or `stratify_by`.")
  }

  # Ensure stratify_by is a character vector (list of columns)
  for (stratum in stratify_by) {
    if (!is.character(stratum)) stop("stratify_by must be a character vector of column names.")
  }
  for (group in group_by) {
    if (!is.character(group)) stop("group_by must be a character vector of column names.")
  }
  # Ensure the specified columns exist in the data frame
  missing_cols <- setdiff(c(group_by, stratify_by), names(df))
  if (length(missing_cols) > 0) {
    stop("The following columns are missing in the data frame: ", paste(missing_cols, collapse = ", "))
  }

  if (length(group_by)!=0 && length(stratify_by)!=0) {
    # Convert all stratification variables to factors
    df <- df %>%
      mutate(across(all_of(stratify_by), as.factor))

    # Define indicator column name
    indicator_col <- "Stratum"

    # Create a single indicator variable for all stratification variables
    df <- df %>%
      mutate(`Stratum` = as.integer(as.factor(interaction(!!!syms(stratify_by), sep = "_"))))

    # Create the correspondence table (mapping between stratification variables and factor levels)
    correspondence_table <- df %>%
      select(all_of(stratify_by), `Stratum`) %>%
      distinct() %>%
      arrange(`Stratum`)  # Ensure ordered factor levels

    correspondence_table <- as.data.frame(correspondence_table)

    # Group the data by all specified grouping variables and the stratify indicator
    df_grouped <- df %>%
      group_by(!!!syms(group_by), .data[[indicator_col]]) %>%
      select(!!!syms(c(group_by, stratify_by, "Stratum")), everything())

    n_groups <- number_of_groups(df, group_by)
    n_strata <- number_of_strata(df, stratify_by)

    # Display correspondence table
    message("Strata are given the following factor levels:\n",
            paste(capture.output(print(correspondence_table, row.names = FALSE)), collapse = "\n"))
    message("Grouped data frame into ", n_strata, " strata, with ", n_group, " groups per strata. (Total: ", n_group*n_strata, " distinct groups.)")

    message("NB! The stratum indicator column is named 'Stratum'. Rename as you like.")
    return(df_grouped)
  } else if(length(group_by)!=0) {
    print("group")
    # Group the data by all specified grouping variables and the stratify indicator
    df_grouped <- df %>% group_by(!!!syms(group_by)) %>%
      select(!!!syms(c(group_by)), everything())

    message("Grouped data frame into ", number_of_groups(df, group_by), " groups.")

    return(df_grouped)
  } else if(length(stratify_by)!=0) {
    # Convert all stratification variables to factors
    df <- df %>%
      mutate(across(all_of(stratify_by), as.factor))

    # Define indicator column name
    indicator_col <- "Stratum"

    # Create a single indicator variable for all stratification variables
    df <- df %>%
      mutate(`Stratum` = as.integer(as.factor(interaction(!!!syms(stratify_by), sep = "_"))))

    # Create the correspondence table (mapping between stratification variables and factor levels)
    correspondence_table <- df %>%
      select(all_of(stratify_by), `Stratum`) %>%
      distinct() %>%
      arrange(`Stratum`)  # Ensure ordered factor levels

    correspondence_table <- as.data.frame(correspondence_table)

    # Group the data by all specified grouping variables and the stratify indicator
    df_grouped <- df %>%
      group_by(.data[[indicator_col]]) %>%
      select(!!!syms(c(stratify_by, "Stratum")), everything())

    # Display correspondence table
    message("Strata are given the following factor levels:\n",
            paste(capture.output(print(correspondence_table, row.names = FALSE)), collapse = "\n"))
    message("Data frame stratified into ", number_of_strata(df, stratify_by), " strata.")

    message("NB! The stratum indicator column is named 'Stratum'. Rename as you like.")
    return(df_grouped)
  } else {
    warning("No grouping or stratification variables was provided. Data frame unchanged.")
    return(df)}
}


##############################################################################
### Group age-period data

#' Group age-period-cohort data
#'
#' Groups a data frame with an age column and a period column into groups of age and period. Optionally, additional stratification can be performed.
#'
#' @param df A data frame with age and period columns.
#' @param age Name of age column in data frame. \cr
#'      **Defaults to \code{"age"} if not specified.**
#' @param period Name of period column in data frame. \cr
#'      **Defaults to \code{"period"} if not specified.**
#' @param stratify_by Additional stratification variables. \cr
#'      **Defaults to NULL (optional).**
#'
#'
#' @return Data frame grouped by age and period, and additional factor levels for each strata.
#' @export

group_APC_df <- function(df, age=NULL, period=NULL, stratify_by=NULL) {

  # Check that age and period are character strings (if provided)
  if (!is.null(age) && !is.character(age)) {
    stop("`age` must be a string, the name of the age column.")
  }
  if (!is.null(period) && !is.character(period)) {
    stop("`period` must be a string, the name of the period column.")
  }

  if (is.null(age) && is.null(period)) {
    # First check if columns named "age" and "period" actually exist
    if (!all(c("age", "period") %in% names(df))) {
      stop("No columns named 'age' and/or 'period' were found in the data frame. Please provide the column names explicitly.")
    } else {
      # Then fall back and notify the user
      age <- "age"
      period <- "period"
      message("No names for age and period columns were given. Using columns named 'age' and 'period'.")
    }
    # If only age provided:
  } else if (!is.null(age) && is.null(period)) {
    # If df does not contain column named "period":
    if (!c("Period") %in% names(df)) {
      stop("No column named 'period' was found in the data frame. Please provide the name of the period column. (period = <name of period column>)")
    } else{
      period <- "period"
      message("No name for the period column was given. Using column named 'period'.")
    }
  } else if (is.null(age) && !is.null(period)) {
    # If df does not contain column named "period":
    if (!c("age") %in% names(df)) {
      stop("No column named 'age' was found in the data frame. Please provide the name of the age column. (age = <name of age column>)")
    } else{
      age <- "age"
      message("No name for the age column was given. Using column named 'age'.")
    }
  }

  # Check if stratify_by is a string or vector of strings
  if (!is.null(stratify_by)) {
    if (!is.character(stratify_by)) {
      stop("'stratify_by' must be a string or vector of strings.")
    }
  }

  # Ensure the specified columns exist in the data frame
  missing_cols <- setdiff(c(age, period, stratify_by), names(df))
  if (length(missing_cols) > 0) {
    stop("The following provided columns are missing in the data frame: ", paste(missing_cols, collapse = ", "))
  }

  df <- add_cohort_column(df, age=age, period=period)

  print(head(df$cohort))

  if (!is.null(stratify_by)) {
    # Convert all stratification variables to factors
    df <- df %>%
      mutate(across(all_of(stratify_by), as.factor))

    # Define indicator column name
    indicator_col <- "Stratum"

    # Create a single indicator variable for all stratification variables
    df <- df %>%
      mutate(`Stratum` = as.integer(as.factor(interaction(!!!syms(stratify_by), sep = "_"))))

    # Create the correspondence table (mapping between stratification variables and factor levels)
    correspondence_table <- df %>%
      select(all_of(stratify_by), `Stratum`) %>%
      distinct() %>%
      arrange(`Stratum`)  # Ensure ordered factor levels

    correspondence_table <- as.data.frame(correspondence_table)

    # Group the data by all specified grouping variables and the stratify indicator
    df_grouped <- df %>%
      group_by(across(all_of(c(age, period))), .data[[indicator_col]]) %>%
      select(!!!syms(c(age, period, "cohort", stratify_by)), everything())

    # Display correspondence table
    message("NB! Added cohort column, calculated from cohort=period-age+max(age).")
    message("Strata are given the following factor levels in the data frame:\n",
            paste(capture.output(print(correspondence_table, row.names=FALSE)), collapse = "\n"))
    message("Grouped data frame into ", number_of_strata(df, stratify_by), " strata, with ", number_of_groups(df, c(age, period)), "age-period groups per strata.")
    return(df_grouped)
  }

  # Group the data by age and period
  df_grouped <- df %>%
    group_by(across(all_of(c(age, period)))) %>%
    select(!!!syms(c(age, period, "cohort")), everything())

  message("NB! Added cohort column, calculated from cohort=period-age+max(age).")
  message("Grouped data frame into ", n_groups(df_grouped), " age-period groups.")
  return(df_grouped)
}

##############################################################################
### Function for adding cohort to data frame

#' Add cohort column to data frame
#'
#' Adds a column for birth cohorts to a data frame, derived from specified age and period columns through the relation \code{cohort = period - age}.
#'
#' @param df Data frame with age and period column.
#' @param age Name of age column in data frame. \cr
#'      **Defaults to \code{"age"} if not specified.**
#' @param period Name of period column in data frame. \cr
#'      **Defaults to \code{"period"} if not specified.**
#' @return Data frame with additional column for birth cohorts added.
#' @export

add_cohort_column <- function(df, age=NULL, period=NULL) {

  # Ensure the specified age and period columns exist in the data frame
  if (!all(c(age, period) %in% names(df))) {
    stop("The specified age or period columns do not exist in the data frame.")
  }
  # Check that age and period are character strings (if provided)
  if (!is.null(age) && !is.character(age)) {
    stop("`age` must be a string, the name of the age column.")
  }
  if (!is.null(period) && !is.character(period)) {
    stop("`period` must be a string, the name of the period column.")
  }

  if (is.null(age) && is.null(period)) {
    # First check if columns named "age" and "period" actually exist
    if (!all(c("age", "period") %in% names(df))) {
      stop("No columns named 'age' and/or 'period' were found in the data frame. Please provide the column names explicitly.")
    } else {
      # Then fall back and notify the user
      age <- "age"
      period <- "period"
      message("Names for age and period columns were not provided. Using columns named 'age' and 'period'.")
    }
    # If only age provided:
  } else if (!is.null(age) && is.null(period)) {
    # If df does not contain column named "period":
    if (!c("Period") %in% names(df)) {
      stop("No column named 'period' was found in the data frame. Please provide the name of the period column. (period = <name of period column>)")
    } else{
      period <- "period"
      message("Name for the period column was not provided. Using column named 'period'.")
    }
  } else if (is.null(age) && !is.null(period)) {
    # If df does not contain column named "period":
    if (!c("age") %in% names(df)) {
      stop("No column named 'age' was found in the data frame. Please provide the name of the age column. (age = <name of age column>)")
    } else{
      age <- "age"
      message("Name for the age column was not provided. Using column named 'age'.")
    }
  }

  # Add the cohort column by subtracting the age from the period
  df <- df %>%
    mutate(cohort = !!sym(period) - !!sym(age)) %>%
    relocate(!!sym(age), !!sym(period), cohort, .before = everything()) # Move age, period, and cohort to the front

  return(df)
}

##############################################################################
### Function for adding cohort index to data frame

#' Add cohort column to data frame
#'
#' Adds a column for cohort indices to a data frame, derived from specified age and period index columns through the relationship \code{cohort index = period index - age index + max(age index)}.
#'
#' @param df Data frame with age and period column.
#' @param age_index Name of age column in data frame. \cr
#'      **Defaults to \code{"age_index"} if not specified.**
#' @param period_index Name of period column in data frame. \cr
#'      **Defaults to \code{"period_index"} if not specified.**
#' @return Data frame with additional column for cohort indices.
#' @export

add_cohort_index <- function(df, age_index=NULL, period_index=NULL) {

  # Ensure the specified age and period columns exist in the data frame
  if (!all(c(age_index, period_index) %in% names(df))) {
    stop("The specified age index or period index columns do not exist in the data frame.")
  }
  # Check that age and period are character strings (if provided)
  if (!is.null(age_index) && !is.character(age_index)) {
    stop("`age_index` must be a string, the name of the age index column.")
  }
  if (!is.null(period_index) && !is.character(period_index)) {
    stop("`period_index` must be a string, the name of the period index column.")
  }

  if (is.null(age_index) || is.null(period_index)) {
    # First check if columns named "age" and "period" actually exist
    if (!all(c("age_index", "period_index") %in% names(df))) {
      stop("No columns named 'age_index' and/or 'period_index' were found in the data frame. Please provide the column names explicitly.")
    } else {
      # Then fall back and notify the user
      age_index <- "age_index"
      period_index <- "period_index"
      message("No names for age and period index columns were given. Using columns named 'age_index' and 'period_index'.")
    }
    # If only age provided:
  } else if (!is.null(age_index) && is.null(period_index)) {
    # If df does not contain column named "period":
    if (!c("period_index") %in% names(df)) {
      stop("No column named 'period_index' was found in the data frame. Please provide the name of the period index column.")
    } else{
      period_index <- "period_index"
      message("No name for the period index column was given. Using column named 'period_index'.")
    }
  } else if (is.null(age_index) && !is.null(period_index)) {
    # If df does not contain column named "period":
    if (!c("age_index") %in% names(df)) {
      stop("No column named 'age_index' was found in the data frame. Please provide the name of the age index column.")
    } else{
      age <- "age"
      message("No name for the age index column was given. Using column named 'age_index'.")
    }
  }

  # Ensure the specified age and period columns exist in the data frame
  if (!all(c(age_index, period_index) %in% names(df))) {
    stop("The specified age or period columns do not exist in the data frame.")
  }

  # Add the cohort column by subtracting the age from the period
  df <- df %>%
    mutate(
      cohort_index = as.integer(!!sym(period_index) - !!sym(age_index) + max(!!sym(age_index)))
    ) %>%
    relocate(!!sym(age_index), !!sym(period_index), cohort_index, .before = everything())
  return(df)
}

##############################################################################
### Function for making 1-index age, period and cohort columns.

#' Add 1-indexed APC columns to data frame, and reorders rows: first by age, then by period.
#'
#' Adds 1-indexed columns for age, period and cohort to a data frame with age and period columns.
#' \cr \code{age_index = age - min(age) + 1}
#' \cr \code{period_index = period - min(period) + 1}
#' \cr \code{cohort_index = period_index - age_index + max(age_index)}
#'
#' @param df Data frame with age and period column.
#' @param age Name of age column in data frame. \cr
#'      **Defaults to \code{"age"} if not specified.**
#' @param period Name of period column in data frame. \cr
#'      **Defaults to \code{"period"} if not specified.**
#' @return Data frame with index columns for age, period and cohort, reordered by age and period.
#' @export

as.APC.df <- function(df, age=NULL, period=NULL) {

  # Ensure the specified age and period columns exist in the data frame
  if (!all(c(age, period) %in% names(df))) {
    stop("The specified age or period columns do not exist in the data frame.")
  }
  # Check that age and period are character strings (if provided)
  if (!is.null(age) && !is.character(age)) {
    stop("`age` must be a string, the name of the age column.")
  }
  if (!is.null(period) && !is.character(period)) {
    stop("`period` must be a string, the name of the period column.")
  }

  if (is.null(age) && is.null(period)) {
    # First check if columns named "age" and "period" actually exist
    if (!all(c("age", "period") %in% names(df))) {
      stop("No columns named 'age' and/or 'period' were found in the data frame. Please provide the column names explicitly.")
    } else {
      # Then fall back and notify the user
      age <- "age"
      period <- "period"
      message("No names for age and period columns were given. Using columns named 'age' and 'period'.")
    }
    # If only age provided:
  } else if (!is.null(age) && is.null(period)) {
    # If df does not contain column named "period":
    if (!c("Period") %in% names(df)) {
      stop("No column named 'period' was found in the data frame. Please provide the name of the period column. (period = <name of period column>)")
    } else{
      period <- "period"
      message("No name for the period column was given. Using column named 'period'.")
    }
  } else if (is.null(age) && !is.null(period)) {
    # If df does not contain column named "period":
    if (!c("age") %in% names(df)) {
      stop("No column named 'age' was found in the data frame. Please provide the name of the age column. (age = <name of age column>)")
    } else{
      age <- "age"
      message("No name for the age column was given. Using column named 'age'.")
    }
  }

  # Add the age and period index columns
  df$age_index <- as.integer(df$age - min(df$age) + 1)
  df$period_index <- as.integer(df$period - min(df$period) + 1)
  df <- add_cohort_index(df, "age_index", "period_index")

  df <- df %>% relocate(age_index, period_index, cohort_index, age, period) %>% arrange(age, period)

  return(df)
}

###################################################################################
# Function for aggregating entire data frame

#' Aggregate data across an entire data frame using sufficient statistics
#'
#' Aggregates specified columns of a data frame into summarizing statistics,
#' preserving the potentially complex structure returned by aggregator functions
#' (like data frames or inla.mdata objects) within list-columns.
#' Aggregation is performed according to sufficient statistics for the specified
#' distribution of the columns. Possible distributions: Gaussian, binomial, multinomial.
#' This function aggregates the entire data frame into a single row result.
#'
#' @param data A data frame.
#' @param gaussian Vector of names for the Gaussian columns to be aggregated using `gaussian_aggregator`. \cr
#'   **Defaults to NULL (optional).**
#' @param gaussian.precision.scales Scales for the precision of Gaussian observations. \cr
#'   Must be one of: \cr
#'   - `NULL`: Use default scales of 1 for all observations in all `gaussian` columns.
#'   - A single numeric vector: Applied only if *exactly one* column is specified in `gaussian`. Length must match `nrow(data)`.
#'   - A *named list*: Where `names(gaussian.precision.scales)` are the names of the Gaussian columns (must match columns specified in `gaussian`). Each list element must be a numeric vector of scales for that column, with length matching `nrow(data)`. \cr
#'   **Defaults to NULL.**
#' @param binomial Vector of names for the binomial columns to be aggregated using `binomial_aggregator`. \cr
#'   **Defaults to NULL (optional).**
#' @param multinomial Vector of names for the multinomial columns to be aggregated using `multinomial_aggregator`. \cr
#'   **Defaults to NULL (optional).**
#' @param multinomial.categories A named list where names correspond to columns in `multinomial`,
#'   and each element is a character vector of all possible categories for that multinomial variable.
#'   If `NULL` for a given column, categories are inferred from the data (may lead to inconsistency if some categories aren't present).
#'   **Defaults to NULL (infer all categories).**
#'
#' @return A single-row data frame (tibble) containing:
#'   - A column 'n' with the total number of rows in the input data.
#'   - For each specified column in `gaussian`, `binomial`, `multinomial`, a corresponding
#'     *list-column* (named e.g., `colname_gaussian`, `colname_binomial`, `colname_multinomial`).
#'     Each element of this list-column can be accessed by using the `$` operator twice, e.g. through `df$colname_gaussian$Y1`.
#'
#' @export

aggregate_df <- function(df,
                               gaussian = NULL,
                               gaussian.precision.scales = NULL,
                               binomial = NULL,
                               multinomial = NULL,
                               multinomial.categories = NULL) {

  # --- Input Validation ---
  if (!is.data.frame(df)) {
    stop("'df' must be a data frame.")
  }
  n_rows <- nrow(df)
  if (n_rows == 0) {
    stop("'df' has 0 rows.")
  }

  check_cols_exist(c(gaussian, binomial, multinomial), df)

  # Validate gaussian.precision.scales structure
  if (!is.null(gaussian) && length(gaussian) > 0) {
    if (!is.null(gaussian.precision.scales)) {
      if (is.list(gaussian.precision.scales)) {
        if (is.null(names(gaussian.precision.scales)) || any(names(gaussian.precision.scales) == "")) {
          stop("'gaussian.precision.scales' list must be named.")
        }
        if (!all(names(gaussian.precision.scales) %in% gaussian)) {
          stop("Names in 'gaussian.precision.scales' list must match columns specified in 'gaussian'.")
          # Could relax to subset if needed, but exact match is safer.
        }
        # Check lengths within the list
        for (col_name in names(gaussian.precision.scales)) {
          if (length(gaussian.precision.scales[[col_name]]) != n_rows) {
            stop(paste0("Length of precision scales for '", col_name, "' in 'gaussian.precision.scales' (",
                        length(gaussian.precision.scales[[col_name]]),
                        ") does not match number of data rows (", n_rows, ")."))
          }
          if(!is.numeric(gaussian.precision.scales[[col_name]])) {
            stop(paste0("Scales for '", col_name, "' must be numeric."))
          }
        }
      } else if (is.vector(gaussian.precision.scales, mode = "numeric")) {
        if (length(gaussian) != 1) {
          stop("Vector of precision sales is only allowed when exactly one Gaussian column is specified. For multiple Gaussian columns you must provide precision scales in a named list (with name of Gaussian columns).")
        }
        if (length(gaussian.precision.scales) != n_rows) {
          stop(paste0("Length of vector 'gaussian.precision.scales' (",
                      length(gaussian.precision.scales),
                      ") does not match number of data rows (", n_rows, ")."))
        }
      } else {
        stop("'gaussian.precision.scales' must be NULL, a numeric vector (for single Gaussian column), or a named list.")
      }
    }
    # If gaussian.precision.scales is NULL, it's fine, gaussian_aggregator defaults
  } else {
    # If no gaussian columns, scales argument is ignored (or should be NULL)
    if (!is.null(gaussian.precision.scales)) {
      warning("`gaussian.precision.scales` provided but no `gaussian` columns specified. Ignoring scales.")
      gaussian.precision.scales <- NULL # Ensure consistency internally
    }
  }

  # Validate multinomial.categories
  if (!is.null(multinomial) && length(multinomial) > 0) {
    if (!is.null(multinomial.categories)) {
      if (!is.list(multinomial.categories) || is.null(names(multinomial.categories))) {
        stop("'multinomial.categories' must be a named list, with the names of the multinomial columns.")
      }
      if (!all(names(multinomial.categories) %in% multinomial)) {
        warning("Names in 'multinomial.categories' do not perfectly match 'multinomial' columns. Only matching names will be used.")
        # Keep only relevant categories
        multinomial.categories <- multinomial.categories[names(multinomial.categories) %in% multinomial]
      }
      # Further checks could be added (e.g., each element is character vector)
    }
  }


  # --- Aggregation ---

  # Step 1: Calculate the total number of observations (rows)
  count_summary <- tibble(n = n_rows)

  # Step 2: Aggregate Gaussian variables
  if (!is.null(gaussian) && length(gaussian) > 0) {
    gaussian_summary <- df %>%
      summarise(across(all_of(gaussian),
                                     ~ { # Use braces for multi-step logic inside lambda
                                       current_col <- cur_column()

                                       # Determine the correct precision scale for this column
                                       scale_for_this_col <- NULL # Default
                                       if (is.list(gaussian.precision.scales)) {
                                         # Lookup by name, default to NULL if name not present (should be caught by validation)
                                         scale_for_this_col <- gaussian.precision.scales[[current_col]]
                                       } else if (is.vector(gaussian.precision.scales, mode="numeric")) {
                                         # This case implies length(gaussian)==1 was validated
                                         scale_for_this_col <- gaussian.precision.scales
                                       } # Else: It's NULL, scale_for_this_col remains NULL (handled by agaussian)

                                       # Call the original gaussian_aggregator
                                       gaussian_aggregator(
                                         df = cur_data(),
                                         col_name = current_col,
                                         precision.scale = scale_for_this_col
                                       )
                                     },
                                     # Names will be like: colname_gaussian
                                     .names = "{col}_gaussian"))
  } else {
    gaussian_summary <- tibble(.rows = 1) # Placeholder, for bind_cols() to work in step 5
  }


  # Step 3: Aggregate binomial variables
  if (!is.null(binomial) && length(binomial) > 0) {
    binomial_summary <- df %>%
      summarise(across(all_of(binomial),
                                     ~ binomial_aggregator(df = cur_data(), col_name = cur_column()),
                                     .names = "{col}_binomial"))
  } else {
    binomial_summary <- tibble(.rows = 1)
  }

  # Step 4: Aggregate multinomial variables
  if (!is.null(multinomial) && length(multinomial) > 0) {
    # Pre-calculate all categories for requested columns if not fully provided
    # Use the provided list, falling back to inference for missing ones
    final_multinomial_categories <- list()
    for(col in multinomial) {
      if (!is.null(multinomial.categories) && col %in% names(multinomial.categories)) {
        final_multinomial_categories[[col]] <- multinomial.categories[[col]]
      } else {
        warning(paste0("Inferring categories for multinomial column '", col,
                       "'. If some categories aren't present in the data they will be overlooked. Supply categories explicitly via 'multinomial.categories' for robustness."), call. = FALSE)
        # Ensure factor levels are consistent if original is factor, else use unique
        if(is.factor(df[[col]])) {
          final_multinomial_categories[[col]] <- levels(df[[col]])
        } else {
          final_multinomial_categories[[col]] <- unique(df[[col]])
        }
      }
    }

    multinomial_summary <- df %>%
      summarise(across(all_of(multinomial),
                                     ~ {
                                       current_col <- cur_column() # String, current column

                                       multinomial_aggregator(
                                         df = cur_data(),
                                         col_name = current_col,
                                         all_categories = final_multinomial_categories[[current_col]]
                                       )
                                     },
                                     .names = "{col}_multinomial"))
  } else {
    multinomial_summary <- tibble(.rows = 1)
  }


  # Step 5: Combine all aggregated results
  aggregated_df <- bind_cols(
    count_summary,
    gaussian_summary,
    binomial_summary,
    multinomial_summary
  ) %>% as_tibble()

  return(aggregated_df)
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
#' @param grouped_data Grouped data frame, created using dplyr grouping methods.
#' @param gaussian Vector of names for the Gaussian columns to be aggregated. \cr
#'   Passed directly to `aggregate_df` for each group. \cr
#'   **Defaults to NULL (optional).**
#' @param gaussian.precision.scales Scales for the precision of Gaussian observations. \cr
#'   Passed directly to `aggregate_df` for each group. The validation within
#'   `aggregate_df` will apply to the subset of data *within each group*.
#'   See \code{\link{aggregate_df}} documentation for format details.
#'   **Defaults to NULL.**
#' @param binomial Vector of names for the binomial columns to be aggregated. \cr
#'   Passed directly to `aggregate_df` for each group. \cr
#'   **Defaults to NULL (optional).**
#' @param multinomial Vector of names for the multinomial columns to be aggregated. \cr
#'   Passed directly to `aggregate_df` for each group. \cr
#'   **Defaults to NULL (optional).**
#' @param multinomial.categories A named list where names correspond to columns in `multinomial`,
#'   and each element is a character vector of all possible categories for that multinomial variable.
#'   If `NULL` for a given column, categories are inferred *across the entire input `grouped_data`*
#'   before aggregation to ensure consistency across groups. This inferred list is then
#'   passed to `aggregate_df` for each group. \cr
#'   **Defaults to NULL (infer all categories from data).**
#'
#' @return Aggregated data frame (tibble), with one row per group, containing
#'   grouping variables, count 'n' per group, and aggregated list-columns for
#'   specified variables as returned by \code{\link{aggregate_df}}.

aggregate_grouped_df <- function(grouped_df,
                                 gaussian = NULL,
                                 gaussian.precision.scales = NULL,
                                 binomial = NULL,
                                 multinomial = NULL,
                                 multinomial.categories = NULL) {

  # --- Input Validation ---
  if (!is_grouped_df(grouped_df)) {
    stop("'grouped_df' must be a data frame grouped using dplyr methods, or group_df()/group_APC_df() from MAPCtools.")
  }

  group_vars <- group_vars(grouped_df)
  if (length(group_vars) == 0) {
    # This check is slightly redundant with is_grouped_df but doesn't hurt
    stop("'grouped_df' does not appear to be grouped. Please verify group structure, or group using dplyr methods or group_df()/group_APC_df() from MAPCtools.")
  }

  # Check existence of all columns
  check_cols_exist(c(gaussian, binomial, multinomial), grouped_df)
  }


  # --- Pre-calculate Multinomial Categories (if inferring from whole dataset) ---
  # This ensures consistency across groups if categories are not explicitly provided.
  # The logic is similar to your original aggregate_grouped_df.
  final_multinomial_categories <- list()
  if (!is.null(multinomial) && length(multinomial) > 0) {
    # Use provided categories first
    if (!is.null(multinomial.categories)) {
      if (!is.list(multinomial.categories) || is.null(names(multinomial.categories))) {
        stop("'multinomial.categories' must be a named list.")
      }
      # Keep only relevant categories and ensure they are character
      relevant_names <- intersect(names(multinomial.categories), multinomial)
      if (length(relevant_names) < length(names(multinomial.categories))) {
        warning("Some names in 'multinomial.categories' do not match 'multinomial' columns and will be ignored.", call.=FALSE)
      }
      if(length(relevant_names) > 0) {
        final_multinomial_categories <- stats::setNames(lapply(multinomial.categories[relevant_names], function(cats) {
          if(!is.character(cats)) {
            warning("Converting provided multinomial categories to character.", call.=FALSE)
            as.character(cats)
          } else {
            cats
          }
        }), relevant_names)
      }
    }

    # Infer categories for any remaining multinomial columns
    cols_to_infer <- setdiff(multinomial, names(final_multinomial_categories))
    if (length(cols_to_infer) > 0) {
      warning(paste("Inferring categories across the entire input data for these multinomial columns:",
                    paste(cols_to_infer, collapse=", "),
                    ". If some categories aren't present in the data they will be overlooked. Supply via 'multinomial.categories' for robustness."),
              call. = FALSE)

      inferred_categories <- lapply(cols_to_infer, function(col) {
        current_col_data <- pull(grouped_df, !!sym(col)) # Extract column data once
        if (is.factor(current_col_data)) {
          unique_vals <- levels(current_col_data)
        } else {
          unique_vals <- unique(current_col_data)
        }
        # Ensure inferred categories are sorted characters
        cats <- as.character(sort(unique_vals))
        if (length(cats) < 1) {
          warning(paste0("Inferred categories for column '", col,
                         "' resulted in zero values after conversion to character. ",
                         "This column might produce empty results during aggregation."), call. = FALSE)
        } else if (length(cats) < 2) {
          warning(paste0("Inferred categories for column '", col,
                         "' resulted in only one unique value: '", cats,"'. ",
                         " Should there be more?"), call. = FALSE)
        }
        cats
      })
      names(inferred_categories) <- cols_to_infer
      final_multinomial_categories <- c(final_multinomial_categories, inferred_categories)
    }
    # Ensure final list only contains columns actually in `multinomial`
    final_multinomial_categories <- final_multinomial_categories[multinomial]

  } else {
    # If multinomial is NULL, make sure categories arg is also NULL for aggregate_df
    multinomial.categories <- NULL
  }


  # --- Perform Aggregation using group_modify ---
  # Apply aggregate_df to each group (.x).
  # Pass all relevant arguments.
  # .keep = TRUE ensures grouping variables are retained.
  aggregated_df <- group_modify(
    grouped_df,
    ~ aggregate_df(
      data = .x, # The data subset for the current group
      gaussian = gaussian,
      # Pass the scales argument. aggregate_df validation will apply to .x
      # Note: If scales were meant to be indexed from the original full dataset,
      # this direct pass might require aggregate_df or the aggregator helper
      # to be aware or handle potential length mismatches gracefully.
      # Often, scales might be *columns* in the data, which works fine here.
      gaussian.precision.scales = gaussian.precision.scales,
      binomial = binomial,
      multinomial = multinomial,
      # Pass the potentially pre-processed, consistent category list
      multinomial.categories = final_multinomial_categories
    ),
    .keep = TRUE # Keep the grouping variables
  )

  # The result automatically includes group_vars and the columns from aggregate_df
  # (n, col1_gaussian, col2_binomial, etc.)

  return(aggregated_df)
}

#########################################################################
##

#' Create NA structure across age, period and cohort groups
#'
#' Creates NA structure indicating which strata each age, period and cohort group belongs to.
#'
#' @param df Data frame with age, period and cohort columns.
#' @param age_col Name of age column.
#'
#'    **Defaults to "age" (optional).**
#' @param period_col Name of period column.
#'
#'    **Defaults to "period" (optional).**
#' @param cohort_col Name of cohort column.
#'
#'    **Defaults to "cohort" (optional).**
#' @param include.random Whether to include an index for each group, for random error terms in MAPC models.
#'
#' #' @param stratify_var Stratification variable to create an NA indicator structure for.
#'
#'    ** Defaults to FALSE (optional)**
#'
#' @return Data frame with NA structure across age-period-cohort groups and strata.
#' @export

as.APC.NA.df <- function(df, age_col = "age", period_col = "period", cohort_col = "cohort", stratify_var=NULL, include.random = FALSE) {

  Okay, let's enhance the as.APC.NA.df function with robust error handling and input validation, following the style of your aggregate_df example.

Kodebit

#' Create NA structure across age, period and cohort groups based on strata
#'
#' Creates a data frame where age, period, and cohort values are placed into
#' columns specific to their stratum (defined by `stratify_var`), with other
#' strata combinations marked as NA. This structure is often useful for
#' specific modeling approaches, like certain Age-Period-Cohort (APC) models.
#' Optionally includes unique indices for random effects.
#'
#' @param df Data frame with age, period, cohort, and stratification columns.
#' @param age_col Name of the age column (must be numeric/integer).
#'   **Defaults to "age".**
#' @param period_col Name of the period column (must be numeric/integer).
#'   **Defaults to "period".**
#' @param cohort_col Name of the cohort column (must be numeric/integer).
#'   **Defaults to "cohort".**
#' @param stratify_var Name of the stratification variable column. This column
#'   will be used to create the stratum-specific NA structure. It should ideally
#'   be a factor or character vector. **This argument is required.**
#' @param include.random Logical. Whether to include a unique index ('random')
#'   for each combination of age, period, and stratum, potentially for use as
#'   random effect identifiers in models. **Defaults to FALSE.**
#'
#' @return A data frame containing the original `age_col`, `period_col`,
#'   `cohort_col`, and `stratify_var` columns, plus:
#'   - Dummy indicator columns for each level of `stratify_var` (e.g., `Region_North`, `Region_South`).
#'   - Stratum-specific age, period, and cohort columns (e.g., `age_Region_North`,
#'     `period_Region_North`, `cohort_Region_North`), containing the respective
#'     value if the row belongs to that stratum, and `NA` otherwise.
#'   - If `include.random = TRUE`, a 'random' column with unique integer indices.
#'   The rows are ordered primarily by the stratification variable levels.
#'
#' @export
#' @importFrom dplyr %>% select all_of starts_with bind_cols arrange group_by mutate row_number ungroup bind_rows matches
#' @importFrom fastDummies dummy_cols
#' @importFrom rlang := !! sym

as.APC.NA.df <- function(df,
                         age_col = "age",
                         period_col = "period",
                         cohort_col = "cohort",
                         stratify_var = NULL, # Will check for non-NULL later
                         include.random = FALSE) {

  # --- Input Validation ---

  # 1. Check df
  if (!is.data.frame(df)) {
    stop("'df' must be a data frame.")
  }
  if (nrow(df) == 0) {
    stop("'df' must not be empty.")
  }

  # 2. Check core column name arguments
  if (!is.character(age_col) || length(age_col) != 1) {
    stop("'age_col' must be a single string.")
  }
  if (!is.character(period_col) || length(period_col) != 1) {
    stop("'period_col' must be a single string.")
  }
  if (!is.character(cohort_col) || length(cohort_col) != 1) {
    stop("'cohort_col' must be a single string.")
  }

  # 3. Check stratify_var argument
  if (is.null(stratify_var)) {
    stop("'stratify_var' argument must be provided (cannot be NULL).")
  }
  if (!is.character(stratify_var) || length(stratify_var) != 1) {
    stop("'stratify_var' must be a single string.")
  }

  # 4. Check column existence in df
  required_cols <- c(age_col, period_col, cohort_col, stratify_var)
  missing_cols <- required_cols[!(required_cols %in% colnames(df))]
  if (length(missing_cols) > 0) {
    stop("The following required columns are missing from 'df': ",
         paste(missing_cols, collapse = ", "))
  }

  # 5. Check data types of core columns
  if (!is.numeric(df[[age_col]])) {
    warning(paste0("Column '", age_col, "' is not numeric. Coercing to numeric. NAs may be introduced."))
    df[[age_col]] <- suppressWarnings(as.numeric(df[[age_col]]))
    if (all(is.na(df[[age_col]]))) stop(paste("Coercion failed: Column '", age_col, "' could not be converted to numeric."))
  }
  if (!is.numeric(df[[period_col]])) {
    warning(paste0("Column '", period_col, "' is not numeric. Coercing to numeric. NAs may be introduced."))
    df[[period_col]] <- suppressWarnings(as.numeric(df[[period_col]]))
    if (all(is.na(df[[period_col]]))) stop(paste("Coercion failed: Column '", period_col, "' could not be converted to numeric."))
  }
  if (!is.numeric(df[[cohort_col]])) {
    warning(paste0("Column '", cohort_col, "' is not numeric. Coercing to numeric. NAs may be introduced."))
    df[[cohort_col]] <- suppressWarnings(as.numeric(df[[cohort_col]]))
    if (all(is.na(df[[cohort_col]]))) stop(paste("Coercion failed: Column '", cohort_col, "' could not be converted to numeric."))
  }

  # Check suitability of stratify_var column (allow factors, characters, but warn numerics)
  if (is.numeric(df[[stratify_var]])) {
    warning(paste0("Column '", stratify_var, "' is numeric. Treating each unique number as a separate category. Consider converting to factor or character first if this is not intended."))
  }
  if (all(is.na(df[[stratify_var]]))) {
    stop(paste0("The stratification column '", stratify_var, "' contains only NA values."))
  }
  # Convert stratify_var to factor to ensure consistent level handling by dummy_cols
  df[[stratify_var]] <- as.factor(df[[stratify_var]])
  if (nlevels(df[[stratify_var]]) == 0) {
    stop(paste0("The stratification column '", stratify_var, "' has no valid levels after converting to factor (might be all NA or empty)."))
  }


  # 6. Check include.random argument
  if (!is.logical(include.random) || length(include.random) != 1) {
    stop("'include.random' must be a single logical value (TRUE or FALSE).")
  }

  # Step 1: Validate column existence
  required_cols <- c(age_col, period_col, cohort_col)
  check_cols_exist(required_cols, df)

  # Step 2: Create dummy variables for the stratification variable with a proper prefix
  dummy_df <- dummy_cols(df, select_columns = stratify_var, remove_first_dummy = FALSE, ignore_na = TRUE)

  # Step 3: Get only the (names of) relevant dummy variable columns (starting with "stratify_var_")
  stratify_levels <- grep(paste0("^", stratify_var, "_"), colnames(dummy_df), value = TRUE)

  # Step 4: Multiply each dummy variable with age, period, and cohort
  for (level in stratify_levels) {
    dummy_df[[paste0("age_", level)]] <- dummy_df[[level]] * df[[age_col]]
    dummy_df[[paste0("period_", level)]] <- dummy_df[[level]] * df[[period_col]]
    dummy_df[[paste0("cohort_", level)]] <- dummy_df[[level]] * df[[cohort_col]]
  }

  # Step 5: Replace all 0s with NA
  dummy_df[dummy_df == 0] <- NA

  # Step 6: Select only relevant columns from dummy_df (stratify variables + new columns)
  dummy_df <- dummy_df %>%
    select(all_of(stratify_levels), starts_with("age_"), starts_with("period_"), starts_with("cohort_"))
  # Step 7: Bind the new dummy variables back to the original dataframe
  df_new <- bind_cols(dummy_df, df)

  # Step 8: Remove any duplicated column names (e.g., from multiple operations)
  df_new <- df_new %>% select(!matches("\\.\\.\\.[0-9]+"))  # Removes unwanted ...1, ...2 suffixes

  # Step 9: Reorder rows by non-NA values in dummy variables
  ordered_rows <- do.call(bind_rows, lapply(stratify_levels, function(level) {
    df_new[!is.na(df_new[[level]]), ]
  }))

  if(include.random) {
    # Step 10: Assign unique random indices based on stratification
    ordered_rows$random <- rep(NA, nrow(ordered_rows))
    i = 1
    stratify_levels <- unique(ordered_rows[[stratify_var]]) # Get unique levels of stratifying variable
    for (age in 1:max(ordered_rows[[age_col]])) {
      for (period in 1:max(ordered_rows[[period_col]])) {
        for (stratum in stratify_levels) {
          ordered_rows$random[ordered_rows[[age_col]] == age &
                                ordered_rows[[period_col]] == period &
                                ordered_rows[[stratify_var]] == stratum] <- i
          i = i + 1
        }
      }
    }
  }

  # Step 11: Return the final structured dataframe
  return(as.data.frame(ordered_rows))
}
