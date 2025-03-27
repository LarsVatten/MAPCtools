# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# This file is meant to serve as a library for the aggregation of grouped data
# into inla.mdata objects that can be used with the agaussian likelihood in the R-INLA package

##############################################################################
### Imports:

#' @import dplyr
#' @import tidyr
#' @import INLA
#' @import fastDummies
#' @import stringr


##############################################################################

#' @title Group data frame
#'
#' @description Groups data into groups indicated by specified grouping and stratification variables.
#'
#' @param df A data frame to group.
#' @param group_by Variables in data frame to group data by.
#' @param stratify_by Variables in data frame to stratify data by.
#' @return Grouped data frame with factor levels for strata.
#' @export

group_data <- function(df, group_by, stratify_by) {

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

  # Convert all stratification variables to factors
  df <- df %>%
    mutate(across(all_of(stratify_by), as.factor))

  # Define indicator column name
  indicator_col <- "Factor level"

  # Create a single indicator variable for all stratification variables
  df <- df %>%
    mutate(`Factor level` = as.integer(as.factor(interaction(!!!syms(stratify_by), sep = "_"))))

  # Create the correspondence table (mapping between stratification variables and factor levels)
  correspondence_table <- df %>%
    select(all_of(stratify_by), `Factor level`) %>%
    distinct() %>%
    arrange(`Factor level`)  # Ensure ordered factor levels

  # Group the data by all specified grouping variables and the stratify indicator
  df_grouped <- df %>%
    group_by(across(all_of(group_by)), .data[[indicator_col]]) %>%
    rename(Stratum = `Factor level`)

  # Display correspondence table
  message("Strata are given the following factor levels:\n",
          paste(capture.output(print(correspondence_table)), collapse = "\n"))
  message("Grouped data frame into ", n_groups(df_grouped), " groups.")
  return(df_grouped)
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

group_APC_data <- function(df, age, period, stratify_by=NULL) {

  # Ensure stratify_by is a character vector (list of columns)
  if (!is.character(age)) stop("age must be a string, the name of the age column")
  if (!is.character(period)) stop("period must be a string, the name of the period column")
  for(stratum in stratify_by) {
    if (!is.character(stratum)) stop("stratify_by must be a character vector of column names.")
  }
  # Ensure the specified columns exist in the data frame
  missing_cols <- setdiff(c(age, period, stratify_by), names(df))
  if (length(missing_cols) > 0) {
    stop("The following columns are missing in the data frame: ", paste(missing_cols, collapse = ", "))
  }

  # Convert all stratification variables to factors
  df <- df %>%
    mutate(across(all_of(stratify_by), as.factor))

  # Define indicator column name
  indicator_col <- "Factor level"

  # Create a single indicator variable for all stratification variables
  df <- df %>%
    mutate(`Factor level` = as.integer(as.factor(interaction(!!!syms(stratify_by), sep = "_"))))

  # Create the correspondence table (mapping between stratification variables and factor levels)
  correspondence_table <- df %>%
    select(all_of(stratify_by), `Factor level`) %>%
    distinct() %>%
    arrange(`Factor level`)  # Ensure ordered factor levels

  # Group the data by all specified grouping variables and the stratify indicator
  df_grouped <- df %>%
    group_by(across(all_of(c(age, period))), .data[[indicator_col]]) %>%
    rename(Stratum = `Factor level`)

  # Display correspondence table
  message("Strata are given the following factor levels in the data frame:\n",
          paste(capture.output(print(correspondence_table)), collapse = "\n"))
  message("Grouped data frame into ", n_groups(df_grouped), " groups.")
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



######################################################################################
## Generate linear combinations for INLA

#' Generate linear combinations for estimation of MAPC models in INLA
#'
#' Generates the proper linear combination object that can be passed to INLA to define linear combinations of linear predictors in an MAPC model.
#'
#' Depends on the configurations of shared vs. stratum-specific time effects. Lowercase letters in "APC" indicates time effect is stratum-specific, upper-case indicates time effect is shared.
#'
#' @param apc_format APC-format with lower-case letters indicating stratum-specific time effects, upper-case indicating shared.
#' @param data Data frame for which the MAPC model will be estimated.
#'
#' @return Object of linear combinations that can be passed to INLA.
#' @export

generate_apc_lincombs <- function(apc_format, data){
  I <- max(data$age)
  J <- max(data$period)
  K <- max(data$cohort)

  all_lincombs = c()
  listed_format = str_split(apc_format, "")[[1]] # Extract individual letters


  if("a" %in% listed_format){
    age_edu_lincomb = c()
    for(l in 1:I){ #For each age

      idx = rep(NA, I)
      idx2 = rep(NA, I)
      idx[l] = 1
      idx2[l] = -1

      #Make 3 linear combinations
      lc1 <- inla.make.lincomb(age_edu_4 = idx2, age_edu_1 = idx, education_4 = -1, education_1 = 1)#, cohort_edu_1 = idx_cohorts, cohort_edu_2 = idx_cohorts, cohort_edu_3 = idx_cohorts, cohort_edu_4 = idx_cohorts)
      lc2 <- inla.make.lincomb(age_edu_4 = idx2, age_edu_2 = idx, education_4 = -1, education_2 = 1)#, cohort_edu_1 = idx_cohorts, cohort_edu_2 = idx_cohorts, cohort_edu_3 = idx_cohorts, cohort_edu_4 = idx_cohorts)
      lc3 <-inla.make.lincomb(age_edu_4 = idx2, age_edu_3 = idx, education_4 = -1, education_3 = 1)#, cohort_edu_1 = idx_cohorts, cohort_edu_2 = idx_cohorts, cohort_edu_3 = idx_cohorts, cohort_edu_4 = idx_cohorts)
      names(lc1) <- paste("LHS/GED vs BA+: Age : ", as.character(min(data$age) + l-1), sep="")
      names(lc2) <- paste("HS vs BA+: Age : ", as.character(min(data$age) + l-1),sep="")
      names(lc3) <- paste("SC/AA vs BA+: Age : ", as.character(min(data$age) + l-1), sep="")

      age_edu_lincomb <- c(age_edu_lincomb, lc1, lc2, lc3)
    }
    age_edu_lincomb = age_edu_lincomb[order(names(age_edu_lincomb))]
    all_lincombs = c(all_lincombs, age_edu_lincomb)
  }
  if("p" %in% listed_format){
    period_edu_lincomb = c()
    for(l in 1:J){ #For each age

      idx = rep(NA, J)
      idx2 = rep(NA, J)
      idx[l] = 1
      idx2[l] = -1

      #Make 3 linear combinations
      lc1 <- inla.make.lincomb(period_edu_4 = idx2, period_edu_1 = idx, education_4 = -1, education_1 = 1)#, cohort_edu_1 = idx_cohorts, cohort_edu_2 = idx_cohorts, cohort_edu_3 = idx_cohorts, cohort_edu_4 = idx_cohorts)
      lc2 <- inla.make.lincomb(period_edu_4 = idx2, period_edu_2 = idx, education_4 = -1, education_2 = 1)#, cohort_edu_1 = idx_cohorts, cohort_edu_2 = idx_cohorts, cohort_edu_3 = idx_cohorts, cohort_edu_4 = idx_cohorts)
      lc3 <-inla.make.lincomb(period_edu_4 = idx2, period_edu_3 = idx, education_4 = -1, education_3 = 1)#, cohort_edu_1 = idx_cohorts, cohort_edu_2 = idx_cohorts, cohort_edu_3 = idx_cohorts, cohort_edu_4 = idx_cohorts)
      names(lc1) <- paste("LHS/GED vs BA+: Period : ", as.character(min(data$period) + (l-1)), sep="")
      names(lc2) <- paste("HS vs BA+: Period : ", as.character(min(data$period) + (l-1)), sep="")
      names(lc3) <- paste("SC/AA vs BA+: Period : ", as.character(min(data$period) + (l-1)), sep="")

      period_edu_lincomb <- c(period_edu_lincomb, lc1, lc2, lc3)
    }
    period_edu_lincomb = period_edu_lincomb[order(names(period_edu_lincomb))]
    all_lincombs = c(all_lincombs, period_edu_lincomb)
  }
  if("c" %in% listed_format){
    cohort_edu_lincomb = c()
    for (l in 1:K) {

      idx = rep(NA, K)
      idx2 = rep(NA, K)
      idx[l] = 1
      idx2[l] = -1

      lc1 <- inla.make.lincomb(cohort_edu_4 = idx2, cohort_edu_1 = idx, education_4 = -1, education_1 = 1)
      lc2 <- inla.make.lincomb(cohort_edu_4 = idx2, cohort_edu_2 = idx, education_4 = -1, education_2 = 1)
      lc3 <-inla.make.lincomb(cohort_edu_4 = idx2, cohort_edu_3 = idx, education_4 = -1, education_3 = 1)
      names(lc1) <- paste("LHS/GED vs BA+: Cohort : ", as.character(l), sep="")
      names(lc2) <- paste("HS vs BA+: Cohort : ", as.character(l) ,sep="")
      names(lc3) <- paste("SC/AA vs BA+: Cohort : ", as.character(l), sep="")

      cohort_edu_lincomb = c(cohort_edu_lincomb, lc1, lc2, lc3)
    }
    cohort_edu_lincomb = cohort_edu_lincomb[order(names(cohort_edu_lincomb))]
    all_lincombs = c(all_lincombs, cohort_edu_lincomb)
  }

  return(all_lincombs)
}

##############################################################################
## Generate MAPC formula for INLA

#' Generate MAPC formula for INLA
#'
#' Based on APC-format, generate the proper formula to pass to INLA for fitting MAPC models.
#'
#' @param df Data frame for which MAPC models should be fit
#' @param APC_format A string where lower-case letters indicate stratum-specific time effects and upper-case letters indicate shared time effects.
#' @param stratify_var Stratification variable. At least one time effect should be stratum-specific, and at least one should be shared.
#' @param model Which prior model to use for the time effects.
#'
#'    **Defaults to "rw1" (optional).**
#' @param random_term Indicator, indicating if a random term should be included in the model
#'
#'    **Defaults to TRUE (optional).**
#'
#' @return A formula object that can be passed to INLA to fit the desired MAPC model.
#' @export

generate_MAPC_formula <- function(df, APC_format, stratify_var, model="rw1", random_term=TRUE) {

  # Define base formula components
  base_formula <- "Y ~ -1"
  stratify_levels <- paste0(stratify_var, "_", unique(na.omit(df[[stratify_var]])))  # Dynamically get levels

  # Add stratification terms (education_1, education_2, etc.)
  strata_intercepts <- paste(stratify_levels, collapse = " + ")

  # Define function template for model terms
  model_term <- function(var) paste0("f(", var, ', model = "', model, '", constr = T, scale.model = T)')

  # Define model terms dynamically based on APC_format
  formula_terms <- list(base_formula, strata_intercepts)  # Start with intercepts

  # Age effect (if 'a' is lowercase, make it stratum-specific)
  if (grepl("a", APC_format)) {
    formula_terms <- c(formula_terms, sapply(stratify_levels, function(level) model_term(paste0("age_", level))))
  } else {
    formula_terms <- c(formula_terms, model_term("age"))
  }

  # Period effect (if 'p' is lowercase, make it stratum-specific)
  if (grepl("p", APC_format)) {
    formula_terms <- c(formula_terms, sapply(stratify_levels, function(level) model_term(paste0("period_", level))))
  } else {
    formula_terms <- c(formula_terms, model_term("period"))
  }

  # Cohort effect (if 'c' is lowercase, make it stratum-specific)
  if (grepl("c", APC_format)) {
    formula_terms <- c(formula_terms, sapply(stratify_levels, function(level) model_term(paste0("cohort_", level))))
  } else {
    formula_terms <- c(formula_terms, model_term("cohort"))
  }

  # Handle random effect
  if (random_term && !("random" %in% names(df))) {
    message("[INFO] No column named 'random' was found. \nCreate a new column named 'random' with unique values for each observation to include the random term.")
    df$random <- 1:nrow(df)
  }

  if (random_term) {
    formula_terms <- c(formula_terms, 'f(random, model = "iid")')
  }

  # Construct full formula as a single string and parse it
  formula_string <- paste(formula_terms, collapse = " +\n")  # Add line breaks for readability
  return(as.formula(formula_string))
}
