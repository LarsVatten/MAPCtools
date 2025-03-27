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

#' @importFrom dplyr setdiff mutate across select distinct arrange
#' @importFrom dplyr group_by rename n_groups
#' @importFrom tidyselect all_of
#' @importFrom rlang syms

##############################################################################

#' @title Group data frame
#'
#' @description Groups data into groups specified by variables \code{group_by} and \code{stratify_by}.
#'
#' @param df A data frame to group.
#' @param group_by Variables in data frame to group data by.
#' @param stratify_by Variables in data frame to stratify data by.
#' @return Grouped data frame with factor levels for strata.
#' @export

group_data <- function(df, group_by=c(), stratify_by=c()) {

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
    print("heiehi")
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

    n_group <- df %>%
      distinct(!!!syms(group_by)) %>%  # Replace with your column names
      nrow()

    n_strata <- df %>%
      distinct(!!!syms(stratify_by)) %>%  # Replace with your column names
      nrow()

    # Display correspondence table
    message("Strata are given the following factor levels:\n",
            paste(capture.output(print(correspondence_table, row.names = FALSE)), collapse = "\n"))
    # message("Grouped data frame into ", n_strata, " strata, with ", n_group, " groups per strata. (Total: ", n_group*n_strata, " groups)")

    message("NB! The stratum indicator column is named 'Stratum'. Rename as you like.")
    return(df_grouped)
  } else if(length(group_by)!=0) {
    print("group")
    # Group the data by all specified grouping variables and the stratify indicator
    df_grouped <- df %>% group_by(!!!syms(group_by)) %>%
      select(!!!syms(c(group_by)), everything())

    n_group <- df %>%
      distinct(!!!syms(group_by)) %>%  # Replace with your column names
      nrow()

    message("Grouped data frame into ", n_group, " groups.")

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

    n_strata <- df %>%
      distinct(!!!syms(stratify_by)) %>%  # Replace with your column names
      nrow()

    # Display correspondence table
    message("Strata are given the following factor levels:\n",
            paste(capture.output(print(correspondence_table, row.names = FALSE)), collapse = "\n"))
    message("Grouped data frame into ", n_strata, " strata.")

    message("NB! The stratum indicator column is named 'Stratum'. Rename as you like.")
    return(df_grouped)
  } else {
    warning("No grouping or stratification variables was provided. Data frame unchanged.")
    return(df)}
}


##############################################################################
### Group age-period data

#' Group age-period data
#'
#' Groups a data frame with an age column and a period column into groups of age and period. Optionally, additional stratification can be performed.
#'
#' @param df A data frame to group.
#' @param age Name of age column in data frame.
#' @param period Name of period column in data frame.
#' @param stratify_by Additional stratification variables.
#'
#'    **Defaults to NULL (optional).**
#'
#' @return Data frame grouped by age and period, and additional factor levels for each strata.
#' @export

group_APC_data <- function(df, age=NULL, period=NULL, stratify_by=NULL) {

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

  df <- add_cohort_column(df, age, period)

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
    message("Grouped data frame into ", n_groups(df_grouped), " groups.")
    return(df_grouped)
  }

    # Group the data by age and period
    df_grouped <- df %>% group_by(age, period) %>%
      select(!!!syms(c(age, period, cohort)), everything())

    message("NB! Added cohort column, calculated from cohort=period-age+max(age).")
    message("Grouped data frame into ", n_groups(df_grouped), " age-period groups.")
    return(df_grouped)
}

##############################################################################
## Function to count number of distinct groups

#' Count number of groups in data frame
#'
#' Counts number of groups across specified grouping and stratification variables in a data frame
#'
#' @param df Data frame.
#' @param group_by Columns to group data by.
#' @param stratify_by Columns to stratify data by.
#' @return Number of distinct groups in the data frame.
#' @keywords internal

number_of_groups <- function(df, group_by, stratify_by) {
  n_groups <- df %>%
    distinct(group_by, stratify_by) %>%
    nrow()
  return(n_groups)
}

##############################################################################
### Function for adding cohort to data frame

#' Add cohort column to data frame
#'
#' Adds a column for birth cohorts to a data frame, derived from specified age and period cohorts.
#'
#' @param df Data frame with age and period column.
#' @param age Name of age column.
#' @param period Name of period column.
#' @return Data frame with additional column for birth cohorts
#' @export

add_cohort_column <- function(df, age, period) {

  # Ensure the specified age and period columns exist in the data frame
  if (!all(c(age, period) %in% names(df))) {
    stop("The specified age or period columns do not exist in the data frame.")
  }

  # Add the cohort column by subtracting the age from the period
  df <- df %>%
    mutate(cohort = !!sym(period) - !!sym(age)) %>%
    relocate(!!sym(age), !!sym(period), cohort, .before = everything()) # Move age, period, and cohort to the front

  return(df)
}

##############################################################################
## Agaussian

#' Aggregate Gaussian data
#'
#' Aggregates Gaussian data into sufficient statistics for Gaussian samples.
#'
#' @param data Gaussian data vector.
#' @param precision.scale Scales for the precision of each Gaussian observation.
#'
#'    **Defaults to NULL (optional).**
#'
#' @return Aggregated Gaussian data.
#' @keywords internal

agaussian <- function(data, precision.scale=NULL) {
  if (!is.null(precision.scale)) {
    # Check if 'extra' is a vector and if its length matches 'data'
    if (!is.vector(precision.scale)) {
      stop("'precision.scale' must be a vector")
    }
    if (length(precision.scale) != length(data)) {
      stop("'precision.scale' must be the same length as 'data'")
    }
    m = sum(precision.scale)
    ybar = 1/m * sum(precision.scale * data)
    v = 1/m * sum(precision.scale * data^2) - ybar^2
    aggregated.data <- data.frame(s1 = v, s2 = 0.5*sum(log(precision.scale)), s3 = m, s4 = length(data), s5 = ybar)
    names(aggregated.data) <- c("Y1", "Y2", "Y3", "Y4", "Y5")
  } else {
    n <- length(data)
    precision.scale <- rep(1, n)
    m = sum(precision.scale)
    ybar = 1/m * sum(precision.scale * data)
    v = 1/m * sum(precision.scale * data^2) - ybar^2
    aggregated.data <- data.frame(s1 = v, s2 = 0.5*sum(log(precision.scale)), s3 = m, s4 = n, s5 = ybar)
    names(aggregated.data) <- c("Y1", "Y2", "Y3", "Y4", "Y5")
  }
  return(aggregated.data)
}

#' Aggregate Gaussian data
#'
#' Aggregates Gaussian data into sufficient statistics for Gaussian samples.
#'
#' @param df Data frame with a Gaussian column.
#' @param col_name Name of Gaussian column.
#' @param precision.scale Scales for the precision of each Gaussian observation.
#'
#'    **Defaults to NULL (optional).**
#'
#' @return Aggregated Gaussian column.
#' @keywords internal

gaussian_aggregator <- function(df, col_name, precision.scale = NULL) {
  stats <- agaussian(df[[col_name]], precision.scale = precision.scale)
  return(inla.mdata(stats))
}

##############################################################################
## Aggregated binomial

#' Aggregate binomial data
#'
#' Aggregates binomial data into sufficient statistics for binomial samples.
#'
#' @param data Binomial data vector.
#' @return Aggregated binomial data.
#' @keywords internal

abinomial <- function(data) {
  return(data.frame(s=sum(data)))
}

#' Aggregate binomial data
#'
#' Aggregates binomial data into sufficient statistics for binomial samples.
#'
#' @param df Data frame with binomial data column.
#' @param col_name Binomial data column.
#' @return Aggregated binomial data column.
#' @keywords internal

# Function to extract and rename the sufficient statistics from the abinomial function
binomial_aggregator <- function(df, col_name) {
  stats <- abinomial(df[[col_name]])
  return(stats)
}

##############################################################################
## Aggregated Multinomial

#' Aggregate multinomial data
#'
#' Aggregates mutlinomial data into sufficient statistics for multinomial samples.
#'
#' @param data Multinomial data vector.
#' @param col_name Name of column with multinomial data.
#' @param all_categories Name of categories in multinomial distribution.
#' @return Aggregated multinomial data.
#' @keywords internal

amultinomial <- function(data, col_name, all_categories) {
  # Ensure the data is a factor with all possible categories
  factor_data <- factor(data, levels = all_categories)

  # Create a table of counts for each category
  counts <- table(factor_data)

  # Remove the last category (to keep only k-1 categories)
  counts <- counts[-length(counts)]

  # Convert counts to a data frame
  counts_df <- as.data.frame(counts, stringsAsFactors = FALSE)

  # Ensure the counts_df has the correct columns: 'category' and 'count'
  colnames(counts_df) <- c("category", "count")

  # Spread the counts into wide format, filling missing categories with 0
  result_df <- counts_df %>%
    pivot_wider(names_from = category, values_from = count, values_fill = list(count = 0)) %>%
    rename_with(~ paste0(., "_count"))  # Generate names without "col_name" prefix

  return(result_df)
}

#' Aggregate multinomial data
#'
#' Aggregates binomial data into sufficient statistics for binomial samples.
#'
#' @param df Data frame with multinomial data column.
#' @param col_name Name of column with multinomial data.
#' @param all_categories Name of categories in multinomial distribution.
#' @return Aggregated multinomial data column.
#' @keywords internal

multinomial_aggregator <- function(df, col_name, all_categories) {
  stats <- amultinomial(df[[col_name]], col_name, all_categories)
  # Now, no need to remove anything, since names are already simplified in amultinomial()
  return(stats)
}

###############################################################################
## Aggregating function, aggregating data

#' Aggregate data
#'
#' Aggregates a grouped data frame into summarizing statistics within groups.
#' Aggregation is performed according to sufficient statistics for the specified distribution of the columns to be aggregated.
#' Possible distributions are: Gaussian, binomial, multinomial.
#'
#' @param grouped_data Data frame, grouped by the group_data function.
#' @param gaussian Vector of names for the Gaussian columns to be aggregated.
#'
#'    **Defaults to NULL (optional).**
#' @param precision.scales Matrix of precision scales for Gaussian columns.
#'
#'    **Defaults to NULL (optional).**
#' @param binomial Vector of names for the binomial columns to be aggregated.
#'
#'    **Defaults to NULL (optional).**
#' @param multinomial Vector of names for the multinomial columns to be aggregated.
#'
#'    **Defaults to NULL (optional).**
#' @return Aggregated data frame, aggregation performed within groups on specified columns.
#' @export

aggregate_data <- function(grouped_data, gaussian = NULL, precision.scales = NULL, binomial = NULL, multinomial = NULL) {
  # Ensure the specified columns in gaussian, binomial, and multinomial exist in the data
  missing_gaussian <- setdiff(gaussian, names(grouped_data))
  missing_binomial <- setdiff(binomial, names(grouped_data))
  missing_multinomial <- setdiff(multinomial, names(grouped_data))

  if (length(missing_gaussian) > 0) {
    stop(paste("These Gaussian columns are missing from the data:", paste(missing_gaussian, collapse = ", ")))
  }

  if (length(missing_binomial) > 0) {
    stop(paste("These binomial columns are missing from the data:", paste(missing_binomial, collapse = ", ")))
  }

  if (length(missing_multinomial) > 0) {
    stop(paste("These Multinomial columns are missing from the data:", paste(missing_multinomial, collapse = ", ")))
  }

  # Step 1: Calculate the number of observations in each group
  count_summary <- grouped_data %>% summarise(n = n())

  # Step 2: Aggregate Gaussian variables
  gaussian_summary <- grouped_data %>%
    summarise(across(all_of(gaussian),
                     ~ gaussian_aggregator(cur_data(), cur_column()),
                     .names = "{col}_gaussian"))

  # Step 3: Aggregate binomial variables
  binomial_summary <- grouped_data %>%
    summarise(across(all_of(binomial),
                     ~ binomial_aggregator(cur_data(), cur_column()),
                     .names = "{col}_binomial"))

  # Step 4: Aggregate multinomial variables
  # Get unique categories for each multinomial variable
  all_multinomial_categories <- lapply(multinomial, function(col) {
    unique(grouped_data[[col]])
  })
  names(all_multinomial_categories) <- multinomial

  multinomial_summary <- grouped_data %>%
    summarise(across(all_of(multinomial),
                     ~ multinomial_aggregator(cur_data(), cur_column(), all_multinomial_categories[[cur_column()]]),
                     .names = "{col}_multinomial"))

  # Step 5: join all aggregated columns
  group_vars <- group_vars(grouped_data)

  aggregated_data <- count_summary %>%
    inner_join(gaussian_summary, by = group_vars) %>%
    inner_join(binomial_summary, by = group_vars) %>%
    inner_join(multinomial_summary, by = group_vars)

  return(aggregated_data)
}


########################################################################
## Create APC indices

#' Create APC data frame
#'
#' Creates 1-indexed age, period and cohort columns indicating age-period-cohort groups.
#'
#' @param df Data frame with age and period columns
#' @param age Age column
#' @param period Period column
#'
#' @return Data frame with 1-indexed age, period and cohort columns.
#' @export

as.APC.df <- function(df, age, period) {
  APC.df <- df
  APC.df$actual_age <- APC.df$age
  min_age <- min(APC.df$actual_age)
  max_age <- max(APC.df$actual_age)
  APC.df$age <- APC.df$age - min_age + 1
  min_year <- min(APC.df$year)
  max_year <- max(APC.df$year)
  APC.df$period <- APC.df$year - min_year + 1
  APC.df$cohort <- max(APC.df$age) - APC.df$age + APC.df$period
  APC.df$age <- as.integer(APC.df$age)
  APC.df$period <- as.integer(APC.df$period)
  APC.df$cohort <- as.integer(APC.df$cohort)
  APC.df <- APC.df %>% arrange(age, period, cohort)
  return(APC.df)
}

#########################################################################
##

#' Create NA structure across age, period and cohort groups
#'
#' Creates NA structure indicating which strata each age, period and cohort group belongs to.
#'
#' @param df Data frame with age, period and cohort columns.
#' @param stratify_var Stratification variable to create an NA indicator structure for.
#' @param age Name of age column.
#'
#'    **Defaults to "age" (optional).**
#' @param period Name of period column.
#'
#'    **Defaults to "period" (optional).**
#' @param cohort Name of cohort column.
#'
#'    **Defaults to "cohort" (optional).**
#' @param include.random Whether to include an index for each group, for random error terms in MAPC models.
#'
#'    ** Defaults to FALSE (optional)**
#'
#' @return Data frame with NA structure across age-period-cohort groups and strata.
#' @export

create_NA_structure <- function(df, stratify_var, age_col = "age", period_col = "period", cohort_col = "cohort", include.random = FALSE) {

  # Step 1: Validate column existence
  required_cols <- c(age_col, period_col, cohort_col)
  missing_cols <- required_cols[!required_cols %in% colnames(df)]

  if (length(missing_cols) > 0) {
    stop(paste("Error: The following columns are missing in the dataset:", paste(missing_cols, collapse=", ")))
  }

  # Step 2: Create dummy variables for the stratification variable with a proper prefix
  dummy_df <- dummy_cols(df, select_columns = stratify_var, remove_first_dummy = FALSE, ignore_na = TRUE)

  # Step 3: Get only the relevant dummy variable columns (avoid unwanted prefixes)
  stratify_levels <- grep(paste0("^", stratify_var, "_"), colnames(dummy_df), value = TRUE)

  # Step 4: Multiply each dummy variable with age, period, and cohort
  for (level in stratify_levels) {
    dummy_df[[paste0(age_col, "_", level)]] <- dummy_df[[level]] * df[[age_col]]
    dummy_df[[paste0(period_col, "_", level)]] <- dummy_df[[level]] * df[[period_col]]
    dummy_df[[paste0(cohort_col, "_", level)]] <- dummy_df[[level]] * df[[cohort_col]]
  }

  # Step 5: Replace all 0s with NA
  dummy_df[dummy_df == 0] <- NA

  # Step 6: Select only relevant columns from dummy_df (stratify variables + new columns)
  dummy_df <- dummy_df %>%
    select(all_of(stratify_levels), starts_with(paste0(age_col, "_")), starts_with(paste0(period_col, "_")), starts_with(paste0(cohort_col, "_")))

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
