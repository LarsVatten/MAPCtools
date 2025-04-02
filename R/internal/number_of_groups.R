##############################################################################
## Function to count number of distinct groups across some variables in a data frame

#' Count number of groups across a set of variables in a data frame
#'
#' Counts number of groups across specified grouping and stratification variables in a data frame.
#' At least one grouping or stratification variable must be provided.
#'
#' @param df A data frame with grouping and/or stratification variables.
#' @param group_by Variables in data frame that defines a grouping of the data.
#' @return Number of distinct groups and strata in the data frame.
#' @keywords internal

number_of_groups <- function(df, group_by) {
  n_groups <- df %>%
    distinct(c(group_by)) %>%
    nrow()
  return(n_groups)
}

##############################################################################
## Function to count number of distinct strata across some variables in a data frame

#' Count number of groups across a set of variables in a data frame
#'
#' Counts number of groups across specified grouping and stratification variables in a data frame.
#' At least one grouping or stratification variable must be provided.
#'
#' @param df A data frame with grouping and/or stratification variables.
#' @param stratify_by Variables in data frame that defines a stratification of the data.
#' @return Number of distinct strata in the data frame.
#' @keywords internal

number_of_strata <- function(df, stratify_by) {
  n_strata <- df %>%
    distinct(c(stratify_by)) %>%
    nrow()
  return(n_strata)
}

