# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# This file serves as a the source file for functions used to
# estimate Bayesian multivariate APC models with INLA.

utils::globalVariables(c(
  "age_index","period_index","cohort_index",
  "cohort","Strata","Stratum","x",
  "median_differences","hpd_lower","hpd_upper",
  "._offset_temp_"
))

##############################################################################
### Imports:

#' @importFrom dplyr setdiff mutate
#' @importFrom stringr str_split
#' @importFrom rlang quo_is_null enquo
NULL

##############################################################################

#' Generate Age-Period-Cohort Linear Combinations for INLA
#'
#' Constructs a set of linear combinations (contrasts) for age, period, and/or cohort effects
#' across different strata, relative to a specified reference strata, suitable for use with
#' \code{\link[INLA]{inla.make.lincomb}} in the R-INLA package.
#'
#' @param apc_format Character string containing any combination of \code{"a"}, \code{"p"}, \code{"c"}:
#'   \describe{
#'     \item{\code{"a"}}{include age contrasts}
#'     \item{\code{"p"}}{include period contrasts}
#'     \item{\code{"c"}}{include cohort contrasts}
#'   }
#'   e.g. \code{"ap"} to generate age and period contrasts only.
#' @param data A \code{data.frame} containing the variables specified by \code{age}, \code{period}, \code{cohort}, and \code{strata}.
#'   The age, period, and cohort variables must be integer-valued (or coercible to integer).
#' @param strata String giving the name of the factor column in \code{data} that defines strata.
#' @param reference_strata String indicating which level of \code{strata} should be used as the reference.
#' @param age String name of the column in \code{data} containing age indices (default \code{"age"}).
#' @param period String name of the column in \code{data} containing period indices (default \code{"period"}).
#' @param cohort String name of the column in \code{data} containing cohort indices (default \code{"cohort"}).
#'
#' @return A named \code{list} of linear combination objects as returned by
#'   \code{\link[INLA]{inla.make.lincomb}}. Each element corresponds to one contrast,
#'   with names of the form \dQuote{Age = x, Strata = y vs ref}, \dQuote{Period = x, Strata = y vs ref},
#'   or \dQuote{Cohort = x, Strata = y vs ref}, depending on \code{apc_format}.
#'
#' @details
#' For each specified dimension (\code{a}, \code{p}, \code{c}), the function loops over all
#' unique values of age, period, or cohort in the data, and over all strata levels except
#' the reference.  It then constructs a contrast that subtracts the effect in the reference
#' stratum from the effect in the other strata at each index.
#'
#' @export
#'
#'
# To generate apc lincombs for INLA
generate_apc_lincombs <- function(apc_format, data, strata, reference_strata,
                                  age="age", period="period", cohort="cohort"){

  if (!requireNamespace("INLA", quietly = TRUE)) {
    stop("This function requires the INLA package. Please install it using:\n",
         'install.packages("INLA", repos = c(INLA = "https://inla.r-inla-download.org/R/stable"))')
  }

  age_vals <- sort(unique(data[[age]]))
  period_vals <- sort(unique(data[[period]]))
  cohort_vals <- sort(unique(data[[cohort]]))
  I <- length(age_vals)
  J <- length(period_vals)
  K <- length(cohort_vals)

  # Strata
  all_strata <- levels(data[[strata]])
  diff_strata <- setdiff(all_strata, reference_strata)
  # Number of different strata that is compared:
  n_strata <- length(all_strata)
  n_strata_diff <- length(diff_strata)

  all_lincombs = c()
  listed_format = str_split(apc_format, "")[[1]] # Extract individual letters

  reference_intercept <- paste0(strata, "_", reference_strata)
  reference_age_effects <- paste0("age_", strata, "_", reference_strata)
  reference_period_effects <- paste0("period_", strata, "_", reference_strata)
  reference_cohort_effects <- paste0("cohort_", strata, "_", reference_strata)

  if("a" %in% listed_format){
    age_lincombs = c()
    for(l in seq_along(age_vals)){ #For each age
      idx = rep(NA, I)
      idx2 = rep(NA, I)
      idx[l] = 1
      idx2[l] = -1

      age_idx <- age_vals[l]

      for(s in 1:n_strata_diff) {
        strata2 <- diff_strata[s]
        strata2_intercept <- paste0(strata, "_", strata2)
        strata2_age_effects <- paste0("age_", strata, "_", strata2)

        terms <- setNames(list(idx2, idx, -1, 1),
                          c(reference_age_effects, strata2_age_effects,
                            reference_intercept, strata2_intercept))

        lc <- do.call(INLA::inla.make.lincomb, terms)
        names(lc) <- paste0("Age = ", age_idx, ", Strata = ", strata2, " vs ", reference_strata)
        age_lincombs <- c(age_lincombs, lc)
      }
    }
    all_lincombs = c(all_lincombs, age_lincombs)
  }
  if("p" %in% listed_format){
    period_lincombs = c()
    for(l in seq_along(period_vals)){ #For each age

      idx = rep(NA, J)
      idx2 = rep(NA, J)
      idx[l] = 1
      idx2[l] = -1

      period_idx <- period_vals[l]

      for(s in 1:n_strata_diff) {
        strata2 <- diff_strata[s]
        strata2_intercept <- paste0(strata, "_", strata2)
        strata2_period_effects <- paste0("period_", strata, "_", strata2)

        terms <- setNames(list(idx2, idx, -1, 1),
                          c(reference_period_effects, strata2_period_effects,
                            reference_intercept, strata2_intercept))

        lc <- do.call(INLA::inla.make.lincomb, terms)
        names(lc) <- paste0("Period = ", period_idx, ", Strata = ", strata2, " vs ", reference_strata)
        period_lincombs <- c(period_lincombs, lc)
      }
    }
    all_lincombs = c(all_lincombs, period_lincombs)
  }
  if("c" %in% listed_format){
    cohort_lincombs = c()
    for(l in seq_along(cohort_vals)){ #For each age

      idx = rep(NA, K)
      idx2 = rep(NA, K)
      idx[l] = 1
      idx2[l] = -1

      cohort_idx <- cohort_vals[l]

      for(s in 1:n_strata_diff) {
        strata2 <- diff_strata[s]
        strata2_intercept <- paste0(strata, "_", strata2)
        strata2_cohort_effects <- paste0("cohort_", strata, "_", strata2)

        terms <- setNames(list(idx2, idx, -1, 1),
                          c(reference_cohort_effects, strata2_cohort_effects,
                            reference_intercept, strata2_intercept))

        lc <- do.call(INLA::inla.make.lincomb, terms)
        names(lc) <- paste0("Cohort = ", cohort_idx, ", Strata = ", strata2, " vs ", reference_strata)
        cohort_lincombs <- c(cohort_lincombs, lc)
      }
    }
    all_lincombs = c(all_lincombs, cohort_lincombs)
  }
  if(length(all_lincombs) == 0) {
    warning("No linear combinations returned. Please check that your input arguments are correct.")
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
#' @param response A string, name of the column in \code{df} that represents the response variable.
#' @param stratify_var Stratification variable. At least one time effect should be stratum-specific, and at least one should be shared.
#' @param apc_prior Which prior model to use for the time effects.\cr**Defaults to "rw1" (optional).**
#' @param intercept Boolean, indicating if an overall intercept should be included in the formula.\cr**Defaults to TRUE (optional).**
#' @param random_term Indicator, indicating if a random term should be included in the model.\cr**Defaults to TRUE (optional).**
#' @param age Name of age column
#' @param period Name of period column
#' @param cohort Name of cohort column
#' @param age.stratified Base of age x strata column names. If not provided, defaults to \code{age_<strataify_var>}.\cr The default results in column names \code{age_<stratify_var>_i}, for each age index \eqn{i=1, \dots, I}.
#' @param period.stratified Base of period x strata column names. If not provided, defaults to \code{period_<stratify_var>}.\cr The default results in column names \code{period_<stratify_var>_j}, for each age index \eqn{j=1, \dots, J}.
#' @param cohort.stratified Base of cohort x strata column names. If not provided, defaults to \code{cohort_<stratify_var>}.\cr The default results in column names \code{cohort_<stratify_var>_k}, for each age index \eqn{k=1, \dots, K}.
#' @param extra.fixed Name of additional fixed effects.\cr**Defaults to NULL (optional).**
#' @param extra.random Name of additional random effects.\cr**Defaults to NULL (optional).**
#' @param random.models Models for random effects. Supported \code{INLA} models include \code{'iid'}, \code{'rw1'} and \code{'rw2'}.\cr**Defaults to NULL (optional).**
#'
#' @return A formula object that can be passed to INLA to fit the desired MAPC model.
#' @export

generate_MAPC_formula <- function(df, APC_format, response, stratify_var, apc_prior="rw1", intercept=F, random_term=TRUE,
                                  age="age", period="period", cohort="cohort",
                                  age.stratified = NULL, period.stratified = NULL, cohort.stratified = NULL,
                                  extra.fixed=NULL, extra.random=NULL, random.models=NULL) {

  # Define base formula components
  base_formula <- paste(response, "~", ifelse(intercept, "1", "-1"))
  stratify_levels <- paste0(stratify_var, "_", unique(na.omit(df[[stratify_var]])))  # Dynamically get levels

  # Add stratification terms (education_1, education_2, etc.)
  strata_intercepts <- paste(stratify_levels, collapse = " + ")

  # Define function template for model terms
  model_term <- function(var) paste0("f(", var, ', model = "', apc_prior, '", constr = T, scale.model = T)')

  # Define model terms dynamically based on APC_format
  formula_terms <- list(base_formula, strata_intercepts)  # Start with intercepts

  # Age effect (if 'a' is lowercase, make it stratum-specific)
  if (grepl("a", APC_format)) {
    formula_terms <- c(formula_terms, sapply(stratify_levels, function(level) model_term(paste0("age_", level))))
  } else if (grepl("A", APC_format)){
    formula_terms <- c(formula_terms, model_term(age))
  }

  # Period effect (if 'p' is lowercase, make it stratum-specific)
  if (grepl("p", APC_format)) {
    formula_terms <- c(formula_terms, sapply(stratify_levels, function(level) model_term(paste0("period_", level))))
  } else if (grepl("P", APC_format)){
    formula_terms <- c(formula_terms, model_term(period))
  }

  # Cohort effect (if 'c' is lowercase, make it stratum-specific)
  if (grepl("c", APC_format)) {
    formula_terms <- c(formula_terms, sapply(stratify_levels, function(level) model_term(paste0("cohort_", level))))
  } else if (grepl("C", APC_format)) {
    formula_terms <- c(formula_terms, model_term(cohort))
  }

  # Handle additional fixed effects
  if (!is.null(extra.fixed)) {
    for (fixed.term in extra.fixed) {
      if (!(fixed.term %in% names(df))) {
        message("[INFO] No column named ", fixed.term, " was found. Make sure it is there when estimating a model.")
      } else{
        formula_terms <- c(formula_terms, fixed.term)
      }
    }
  }

  # Handle additional random effects
  if(!is.null(random.models) & is.null(extra.random)) {
    stop("Random models were specified, but no random effects given in 'extra.random'. Please specify the effects.")
  }

  if(!is.null(extra.random)) {
    # No models specified
    if (is.null(random.models)) {
      message("[INFO] No models were specified for the random effects. Using default 'iid' for all extra random effects.")
      for (random.term in extra.random) {
        if (!(random.term %in% names(df))) {
          message("[INFO] No column named ", random.term, " was found. Make sure it is there when estimating a model.")
        } else{
          formula_terms <- c(formula_terms, paste0('f(', random.term, ', model="iid")'))
        }
      }
    }
    # Only one model specified, but more than one random effect
    if (length(random.models)==1 & length(extra.random) > 1) {
      message("[INFO] More than one random effect was specified, but only one model. Using the same model for all extra random effects.")
      for (random.term in extra.random) {
        if (!(random.term %in% names(df))) {
          message("[INFO] No column named ", random.term, " was found. Make sure it is there when estimating a model.")
        }
        if (random.models %in% c("rw1", "rw2")) {
          formula_terms <- c(formula_terms, paste0('f(', random.term, ', model="', random.models, '", constr=T, scale.model=T)'))
        } else {
          formula_terms <- c(formula_terms, paste0('f(', random.term, ', model="', random.models, '")'))
        }
      }
    }
    # Mismatch in lengths
    if (length(extra.random)>1 & length(random.models)>1) {
      if (length(random.models) != length(random.models)) {
        stop("Lengths of 'extra.random' and 'random.models' are not the same.")
      } else{
        for (i in seq_along(extra.random)) {
          if (!(extra.random[i] %in% names(df))) {
            message("[INFO] No column named ", extra.random[i], " was found. Make sure it is there when estimating a model.")
          }
          if (random.models[i] %in% c("rw1", "rw2")) {
            formula_terms <- c(formula_terms, paste0('f(', extra.random[i], ', model="', random.models[i], '", constr=T, scale.model=T)'))
          } else {
            formula_terms <- c(formula_terms, paste0('f(', extra.random[i], ', model="', random.models[i], '")'))
          }
        }
      }
    }
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

#################################################################################################
#' Fit a multivariable Age-Period-Cohort model using INLA
#'
#' Takes a raw data frame, converts it to APC format (1-indexed age, period, cohort),
#' masks out NA cells in the APC grid, builds the INLA formula and linear combinations,
#' and then fits the specified MAPC model via \code{INLA}.
#'
#' @param data              A \code{data.frame} containing the raw age, period, response, and stratification variables.
#' @param response          A string naming the response (outcome) variable in \code{data}.
#' @param apc_format        A specification of the APC structure (e.g. "APC", "AC", "AP", etc.), passed to the formula generator.
#' @param family            Likelihood family of the response variable
#' @param strata_col        A string naming the column in \code{data} to use for stratification (e.g. region or sex).
#' @param reference_strata  Optional; value(s) of \code{strata_col} to set as the reference level(s). Defaults to \code{NULL}.
#' @param age               Name of the age variable in \code{data}. Defaults to \code{"age"}.
#' @param period            Name of the period variable in \code{data}. Defaults to \code{"period"}.
#' @param apc_prior         A string specifying the prior for the age, period, and cohort effects (e.g. \code{"rw1"}, \code{"rw2"}). Defaults to \code{"rw1"}.
#' @param random            Logical; if \code{TRUE}, include an overall random effect in the APC model. Defaults to \code{TRUE}.
#' @param control.compute   A list of control.compute options passed to \code{INLA} (e.g. \code{config}, \code{dic}, \code{waic}, \code{cpo}). Defaults to \code{list(config=TRUE, dic=TRUE, waic=TRUE, cpo=TRUE)}.
#' @param binomial.n        For the \code{family=binomial} likelihood. Either an integer giving the number of trials for the binomial response, or the name of the column containing the number of trials for each observation.
#' @param poisson.offset    For the \code{family=poisson} likelihood. Either an integer giving the denominator for the Poisson count response, or the name of the column containing the denominator for each observation.
#' @param verbose           This is argument is passed along to the \code{inla()} function that estimates the MAPC model. If \code{verbose=TRUE}, the \code{inla}-program runs in verbose mode, which can provide more informative error messages.
#'
#' @return An named list, containing the following arguments:
#' \describe{
#'  \item{\code{model_fit}}{An object of class \code{"inla"}, containing posterior densities, posterior summaries, measures of model fit etc. See \code{\link[INLA]{inla}} for details.}
#'  \item{\code{plots}}{A named list of plots for each time effect. Extract them as \code{plots\$age}/\code{plots\$period}\code{plots\$cohort}.}
#' }
#'
#' @examples
#' \dontrun{
#' fit <- fit_MAPC(
#'   data               = mydata,
#'   response           = "deaths",
#'   apc_format         = "APC",
#'   strata_col         = "region",
#'   reference_strata   = "North",
#'   age                = "age_group",
#'   period             = "year",
#'   apc_prior          = "rw1",
#'   random             = TRUE
#' )
#' }
#'
#' @export
fit_MAPC <- function(data, response, apc_format, family, strata_col, reference_strata=NULL,
                     age="age", period="period", apc_prior="rw1", random=T,
                     control.compute = list(config = TRUE, dic = TRUE, waic = TRUE, cpo = TRUE),
                     binomial.n = NULL, poisson.offset = NULL, verbose=F) {

  if (!requireNamespace("INLA", quietly = TRUE)) {
    stop("This function requires the INLA package. Please install it using:\n",
         'install.packages("INLA", repos = c(INLA = "https://inla.r-inla-download.org/R/stable"))')
  }

  if(!is.factor(data[[strata_col]])) {stop("Strata_col needs to be a factor.")}
  if(!(reference_strata %in% levels(data[[strata_col]]))) {stop("'reference_strata' must be one of the levels of 'strata_col'.")}

  binomial.n <- enquo(binomial.n)
  poisson.offset <- enquo(poisson.offset)

  if (family == "binomial" && quo_is_null(binomial.n)) {
    message("Family is binomial, but no value for 'binomial.n' was supplied. Defaults to 1.\nProvide the number of trials if this is wrong.")
  }
  if (family == "poisson" && quo_is_null(poisson.offset)) {
    message("Family is poisson, but no value for 'poisson.offset' was supplied. Defaults to 1.")
  }

  data.APC <- data %>% as.APC.df(age, period)

  data.APC.NA <- data.APC %>% as.APC.NA.df(strata_col,
                                           age_col="age_index", period_col="period_index", cohort_col="cohort_index",
                                           include.random=random)

  inla_formula <- generate_MAPC_formula(data.APC.NA, apc_format, response, stratify_var = strata_col,
                                        apc_prior=apc_prior, random_term=random,
                                        age="age_index", period="period_index", cohort="cohort_index")

  lincombs <- generate_apc_lincombs(apc_format, data.APC.NA,
                                    strata=strata_col, reference_strata = reference_strata,
                                    age="age_index", period="period_index", cohort="cohort_index")

  binomial.vec <- if (!rlang::quo_is_null(binomial.n)) rlang::eval_tidy(binomial.n, data = data.APC.NA) else NULL
  poisson.vec  <- if (!rlang::quo_is_null(poisson.offset)) rlang::eval_tidy(poisson.offset, data = data.APC.NA) else NULL


  if(family=="binomial" & !is.null(binomial.vec)) {
    fit <- INLA::inla(
      formula = inla_formula,
      data = data.APC.NA,
      family = family,
      control.compute = control.compute,
      lincomb = lincombs,
      Ntrials = binomial.vec,
      verbose = verbose
    )
  } else if(family=="poisson" & !is.null(poisson.vec)) {
    data.APC.NA$._offset_temp_ <- poisson.vec
    fit <- INLA::inla(
      formula = inla_formula,
      data = data.APC.NA,
      family = family,
      control.compute = control.compute,
      E = ._offset_temp_,
      verbose = verbose
    )
  } else {
    fit <- INLA::inla(
      formula = inla_formula,
      data = data.APC.NA,
      family = family,
      control.compute = control.compute,
      lincomb = lincombs,
      verbose = verbose
    )
  }


  plots <- plot_lincombs(fit, apc_format, data.APC.NA,
                         strata_col = strata_col, reference_level = reference_strata,
                         family = family,
                         age="age_index", period="period_index", cohort="cohort_index")

  return(list(model_fit = fit, plots = plots))
}

