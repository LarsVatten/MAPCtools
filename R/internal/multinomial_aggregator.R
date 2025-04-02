##############################################################################
## Aggregated Multinomial

#' Aggregate multinomial data. Used in \code{\link{aggregate_df}}.
#'
#' Aggregates multinomial data into sufficient statistics for multinomial samples. \cr
#' Converts input data to character before processing.
#' For a sample \eqn{\boldsymbol{y} = \{y_1, \dots, y_n\}} with \eqn{y_i \in \{1, \dots, K\}}, \eqn{P(y_i = k) = p_k, k=1, \dots, K}, the sample is aggregated into the sufficient statistic \cr \cr
#' \eqn{\boldsymbol{s} = (s_1, \dots, s_{K-1})} \cr \cr
#' where \cr \cr
#' \eqn{s_k = \sum_{i=1}^n \mathbb{I}(y_i = k)} for \eqn{k = 1, \dots, K-1}. \cr \cr
#' (The last category is omitted due to the sum-to-one constraint)
#'
#' @param data A vector containing the multinomial observations (will be coerced to character).
#' @param col_name A character string giving the name of the column (primarily for context/error messages, less critical now).
#' @param all_categories A character vector with the names or levels of all possible categories in the multinomial distribution (must include all observed values after coercion to character).
#' @return A one-row data frame containing counts for each of the first \eqn{K - 1} categories.
#' @keywords internal

amultinomial <- function(data, col_name, all_categories = NULL) {

  # --- Convert input data to character ---
  # Handles numeric, factor, integer, character inputs consistently.
  # Ensures data matches the expected character type of all_categories.
  data <- as.character(data)

  # Input checks (applied to the character version of data)
  if (!is.character(data)) {
    stop("Failed to convert input 'data' to character.")
  }

  # Check col_name
  if (!is.character(col_name) || length(col_name) != 1) {
    stop("'col_name' must be a single character string.")
  }

  # Infer all_categories if not provided (now operates on character data)
  if (is.null(all_categories)) {
    all_categories <- sort(unique(data)) # Infer from unique character values
    warning(paste0("For column '", col_name, "', 'all_categories' was not provided — inferred from data. ",
                   "If some categories aren't present in the data they will be overlooked. For consistent results, supply the categories in argument 'all_categories' explicitly."), call. = FALSE)
    if (length(all_categories) < 1) {
      # Handle case where input data was empty after conversion
      warning(paste0("For column '", col_name, "', inferred categories resulted in an empty set. ",
                     "Returning empty aggregation result."), call.=FALSE)
    }
  }

  # Check all_categories
  if (!is.character(all_categories) || length(all_categories) < 2) {
    # If categories were inferred and resulted in < 2, this error is informative.
    stop(paste0("For column '", col_name, "', 'all_categories' must be a character vector with at least two categories. ",
                "Received: ", paste(all_categories, collapse=", ")), call.=FALSE)
  }

  # Check if all data values are in categories (now comparing characters)
  unique_data_vals <- unique(data)
  if (!all(unique_data_vals %in% all_categories)) {
    # Added check for empty data to avoid cryptic error with setdiff
    if (length(unique_data_vals) == 0 && length(data) > 0) {
      # This implies data contained only NAs (if NAs aren't handled earlier)
      warning(paste0("For column '", col_name, "', data contained values (e.g., NAs) not present after filtering/conversion, ",
                     "resulting in no valid data points matching 'all_categories'."), call.=FALSE)
    } else {
      missing_vals <- setdiff(unique_data_vals, all_categories)
      stop(paste0("For column '", col_name, "', data contains categories not listed in 'all_categories': ",
                  paste(missing_vals, collapse = ", ")), call. = FALSE)
    }
  }

  # Ensure factor with full levels (input `data` is now guaranteed character)
  factor_data <- factor(data, levels = all_categories)

  # Count occurrences of each level
  counts <- table(factor_data) # Includes counts for all levels specified

  # Prepare the result structure: one column for each of the first K-1 categories
  k_minus_1_categories <- all_categories[-length(all_categories)]
  result_df <- as.data.frame(matrix(0, nrow = 1, ncol = length(k_minus_1_categories)))
  if (length(k_minus_1_categories) > 0) {
    names(result_df) <- paste0(k_minus_1_categories, "_count")
  } else {
    names(result_df) <- character(0)
  }


  # Fill in the counts for the K-1 categories that are present in the data
  counts_to_use <- counts[names(counts) %in% k_minus_1_categories]
  if(length(counts_to_use) > 0) {
    present_cat_names <- names(counts_to_use)
    result_df[1, paste0(present_cat_names, "_count")] <- counts_to_use
  }

  return(result_df)
}


#' Aggregate multinomial data (No changes needed here)
#'
#' Aggregates multinomial data into sufficient statistics for multinomial samples. Uses \code{\link{amultinomial}}.
#'
#' @param df Data frame with multinomial data column.
#' @param col_name Name of column with multinomial data.
#' @param all_categories Character vector of names of categories in multinomial distribution.
#' @return Aggregated multinomial data column (a one-row data frame).
#' @keywords internal

multinomial_aggregator <- function(df, col_name, all_categories=NULL) {
  # --- Basic Input Validation ---
  if (!is.data.frame(df)) {
    stop("'df' must be a data frame.")
  }
  if (!is.character(col_name) || length(col_name) != 1) {
    stop("'col_name' must be a character string, naming a column in the data frame.")
  }
  if (!(col_name %in% names(df))) {
    stop(paste0("Column '", col_name, "' not found in the data frame."))
  }

  if (nrow(df) == 0) {
    warning(paste0("Received empty data frame for group when aggregating '", col_name, "'. Returning empty result."), call. = FALSE)
    if (is.null(all_categories)) stop("Cannot determine structure for empty group without 'all_categories'.")
    if (!is.character(all_categories) || length(all_categories) < 2) stop("'all_categories' invalid for empty group structure.")
    # Return structure matching amultinomial output for non-empty data
    k_minus_1_categories <- all_categories[-length(all_categories)]
    result_df <- as.data.frame(matrix(0, nrow = 1, ncol = length(k_minus_1_categories)))
    if(length(k_minus_1_categories)>0) names(result_df) <- paste0(k_minus_1_categories, "_count")
    return(result_df)

  }

  # --- Category Handling & Validation ---
  # This function RELIES on receiving valid, character `all_categories`
  # The inference should happen higher up (e.g., in aggregate_grouped_df)
  if (is.null(all_categories)) {
    # This shouldn't happen if called from aggregate_df/aggregate_grouped_df
    # as they pre-calculate/pass categories. But defensively...
    stop(paste0("Internal error: 'all_categories' was NULL when calling multinomial_aggregator for column '", col_name, "'. ",
                "Categories must be provided or inferred beforehand."), call.=FALSE)
  }

  if (!is.character(all_categories) || length(all_categories) < 2) {
    stop(paste0("Invalid 'all_categories' passed to multinomial_aggregator for column '", col_name,
                "'. Must be a character vector with at least two categories."), call.=FALSE)
  }

  # --- Call amultinomial ---
  # amultinomial now handles the data type conversion internally
  stats <- amultinomial(data = df[[col_name]],
                        col_name = col_name, # Pass for context
                        all_categories = all_categories)

  return(stats)
}
