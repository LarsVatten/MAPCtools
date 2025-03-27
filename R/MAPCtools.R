# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# This file serves as a library for analyzing age-period-cohort data,
# and for fitting Bayesian multivariate APC models with INLA. Test git

##############################################################################
### Imports:

#' @importFrom dplyr setdiff mutate
#' @import tidyr
#' @import INLA
#' @import fastDummies
#' @import stringr


##############################################################################

## Generate linear combinations for INLA

#' @title Generate APC Linear Combinations
#'
#' @description Generates the proper linear combination object that can be passed to INLA to define linear combinations of linear predictors in an MAPC model. Depends on the configurations of shared vs. stratum-specific time effects. Lowercase letters in "APC" indicates time effect is stratum-specific, upper-case indicates time effect is shared.
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
