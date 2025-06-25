##############################################################################
# Suppress warnings due to global functions and tidy NSE:

utils::globalVariables(c(
  "age_index","period_index","cohort_index",
  "cohort","Strata","Stratum","x",
  "median_differences","hpd_lower","hpd_upper",
  "._offset_temp_", "strata_col_name", "apc_format",
  "model_selection_table", "transformed_prec", "E", "Ntrials",
  "survey.design"
))
##############################################################################
### Imports:

#' @importFrom dplyr setdiff mutate group_by
#' @importFrom stringr str_split
#' @importFrom rlang quo_is_null enquo eval_tidy
#' @importFrom survey svyby svymean svydesign
#' @importFrom stats qlogis
NULL

##############################################################################

#' Generate Age-Period-Cohort Linear Combinations for INLA
#'
#' Constructs a set of linear combinations (contrasts) for age, period, and/or cohort effects
#' across different strata, relative to a specified reference strata, suitable for use with
#' \code{inla.make.lincomb} from the \code{INLA} package.
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
#'   \code{inla.make.lincomb()} (\code{INLA} function). Each element corresponds to one contrast,
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
#' @param apc_hyper If the user wants non-default hyperpriors for the random time effects, this can be achieved by passing the entire
#'    prior specification as a string. If e.g. \code{hyper =  list(theta = list(prior="pc.prec", param=c(0.5,0.01)))} is desired, pass the string "\code{list(theta = list(prior="pc.prec", param=c(0.5,0.01)))}" to this argument.
#' @param intercept Boolean, indicating if an overall intercept should be included in the formula.\cr**Defaults to TRUE (optional).**
#' @param random_term Indicator, indicating if a random term should be included in the model.\cr**Defaults to TRUE (optional).**
#' @param age Name of age column
#' @param period Name of period column
#' @param cohort Name of cohort column
#' @param extra.fixed Name of additional fixed effects.\cr**Defaults to NULL (optional).**
#' @param extra.random Name of additional random effects.\cr**Defaults to NULL (optional).**
#' @param extra.models Models for additional random effects. Supported \code{INLA} models include \code{'iid'}, \code{'rw1'} and \code{'rw2'}.\cr**Defaults to NULL (optional).**
#' @param extra.hyper If the user wants non-default hyperpriors for the additional random effects, this can be achieved by passing the entire
#'    prior specification as a string. If e.g. \code{hyper =  list(theta = list(prior="pc.prec", param=c(0.5,0.01)))} is desired, pass the string "\code{list(theta = list(prior="pc.prec", param=c(0.5,0.01)))}" to this argument.
#'
#' @return A formula object that can be passed to INLA to fit the desired MAPC model.
#' @export

generate_MAPC_formula <- function(df, APC_format, response, stratify_var,
                                  age="age", period="period", cohort="cohort",
                                  intercept = FALSE,
                                  apc_prior="rw1",
                                  apc_hyper = NULL,
                                  random_term=TRUE,
                                  extra.fixed=NULL, extra.random=NULL,
                                  extra.models=NULL, extra.hyper = NULL) {

  if (!response %in% names(df)) {
    stop(sprintf("No column named '%s' in data.", response), call. = FALSE)
  }

  # 2) stratify_var
  if (!stratify_var %in% names(df)) {
    stop(sprintf("No column named '%s' in data.", stratify_var), call. = FALSE)
  }

  if (grepl("A", APC_format) && !age      %in% names(df)) stop("Missing 'age' column.")
  if (grepl("P", APC_format) && !period   %in% names(df)) stop("Missing 'period' column.")
  if (grepl("C", APC_format) && !cohort   %in% names(df)) stop("Missing 'cohort' column.")

  # Prepare hyper strings
  apc_hyper_str   <- if (!is.null(apc_hyper))   paste0(", hyper = ", apc_hyper)   else ""
  extra_hyper_str <- if (!is.null(extra.hyper)) paste0(", hyper = ", extra.hyper) else ""

  # Base formula
  base_formula    <- paste(response, "~", ifelse(intercept, "1", "-1"))
  stratify_levels <- paste0(stratify_var, "_", unique(na.omit(df[[stratify_var]])))
  strata_intercepts <- paste(stratify_levels, collapse = " + ")

  # Helper to build APC term
  model_term <- function(var) {
    paste0(
      "f(", var,
      ', model="', apc_prior, '"',
      ", constr=TRUE, scale.model=TRUE",
      apc_hyper_str,
      ")"
    )
  }

  # Start assembling
  formula_terms <- list(base_formula, strata_intercepts)

  # Age
  if (grepl("a", APC_format)) {
    formula_terms <- c(formula_terms,
                       sapply(stratify_levels, function(l) model_term(paste0("age_", l)))
    )
  } else if (grepl("A", APC_format)) {
    formula_terms <- c(formula_terms, model_term(age))
  }

  # Period
  if (grepl("p", APC_format)) {
    formula_terms <- c(formula_terms,
                       sapply(stratify_levels, function(l) model_term(paste0("period_", l)))
    )
  } else if (grepl("P", APC_format)) {
    formula_terms <- c(formula_terms, model_term(period))
  }

  # Cohort
  if (grepl("c", APC_format)) {
    formula_terms <- c(formula_terms,
                       sapply(stratify_levels, function(l) model_term(paste0("cohort_", l)))
    )
  } else if (grepl("C", APC_format)) {
    formula_terms <- c(formula_terms, model_term(cohort))
  }

  # Extra fixed
  if (!is.null(extra.fixed)) {
    for (fx in extra.fixed) {
      if (fx %in% names(df)) {
        formula_terms <- c(formula_terms, fx)
      } else {
        stop(sprintf("No column named '%s' in data.", fx), call. = FALSE)
      }
    }
  }

  # Extra random
  if (!is.null(extra.models) && is.null(extra.random)) {
    stop("Random models specified but no 'extra.random' given.")
  }
  if (!is.null(extra.random)) {
    # default iid if no model
    if (is.null(extra.models)) {
      for (rx in extra.random) {
        if (rx %in% names(df)) {
          formula_terms <- c(formula_terms,
                             paste0('f(', rx, ', model="iid"', extra_hyper_str, ')')
          )
        } else stop(sprintf("No column named '%s' in data.", rx), call. = FALSE)
      }
    }

    # exactly one extra.random, one extra.models
    if (length(extra.random)==1 && length(extra.models)==1) {
      rx <- extra.random
      if (!rx %in% names(df)) {
        stop(sprintf("No column named '%s' in data.", rx), call. = FALSE)
      }
      m  <- extra.models
      formula_terms <- c(
        formula_terms,
        paste0(
          'f(', rx,
          ', model="', m, '"',
          if (m %in% c("rw1","rw2")) ', constr=TRUE, scale.model=TRUE' else "",
          extra_hyper_str,
          ')'
        )
      )
    }

    # one model for many effects
    if (length(extra.models)==1 && length(extra.random)>1) {
      for (rx in extra.random) {
        if (rx %in% names(df)) {
          formula_terms <- c(formula_terms,
                             paste0(
                               'f(', rx,
                               ', model="', extra.models, '"',
                               if (extra.models %in% c("rw1","rw2")) ', constr=TRUE, scale.model=TRUE' else "",
                               extra_hyper_str,
                               ')'
                             )
          )
        } else stop(sprintf("No column named '%s' in data.", rx), call. = FALSE)
      }
    }

    # matched lists
    if (length(extra.random)>1 && length(extra.models)>1) {
      if (length(extra.random)!=length(extra.models)) {
        stop("Lengths of 'extra.random' and 'extra.models' differ.")
      }
      for (i in seq_along(extra.random)) {
        rxm <- extra.models[i]
        hyp <- if (!is.null(extra.hyper) && length(extra.hyper)>1) extra.hyper[i] else extra.hyper
        hy_str <- if (!is.null(hyp)) paste0(", hyper = ", hyp) else ""
        if (extra.random[i] %in% names(df)) {
          formula_terms <- c(formula_terms,
                             paste0(
                               'f(', extra.random[i],
                               ', model="', rxm, '"',
                               if (rxm %in% c("rw1","rw2")) ', constr=TRUE, scale.model=TRUE' else "",
                               hy_str,
                               ')'
                             )
          )
        } else stop(sprintf("No column named '%s' in data.", extra.random[i]), call. = FALSE)
      }
    }
  }

  # Global random
  if (random_term) {
    if (!("random" %in% names(df))) {
      df$random <- seq_len(nrow(df))
    }
    formula_terms <- c(formula_terms, 'f(random, model="iid")')
  }

  # Build & return
  formula_string <- paste(formula_terms, collapse = " +\n")
  as.formula(formula_string)
}

#################################################################################################
#' Fit a multivariable age-period-cohort model
#'
#' Fit a Bayesian multivariate age-period-cohort model, and obtain posteriors for identifiable cross-strata contrasts.
#' The method is based on Riebler and Held (2010) \doi{10.1093/biostatistics/kxp037}.
#' For handling complex survey data, we follow Mercer et al. (2014) \doi{10.1016/j.spasta.2013.12.001},
#' implemented using the \pkg{survey} package.
#'
#' @param data              A data frame containing the age, period, response, and stratification variables.
#'                          Age and period are assumed to be on the raw scale, not transformed to 1-indexed index columns.
#'                          Factor/character columns are handled, as long as they are properly sorted by \code{sort(unique(data$age/period))} (e.g. values of the form "20-25" for age groups are handled).
#' @param response          A string naming the response (outcome) variable in \code{data}.
#' @param family            A string indicating the likelihood family. The default is \code{"gaussian"} with identity link.
#'                          See \code{names(inla.models()$likelihood)} for a list of possible alternatives and use \code{inla.doc()} for detailed docs for individual families.
#' @param apc_format        A specification of the APC structure, with options:
#'                          \describe{
#'                           \item{APc}{Shared age and period effects, stratum-specific cohort effects.}
#'                           \item{ApC}{Shared age and cohort effects, stratum-specific period effects.}
#'                           \item{aPC}{Shared period and cohort effects, stratum-specific age effects.}
#'                           \item{Apc}{Shared age effects, stratum-specific period and cohort effects.}
#'                           \item{aPc}{Shared period effects, stratum-specific age and cohort effects.}
#'                           \item{apC}{Shared cohort effects, stratum-specific age and period effects.}
#'                          }
#'                          Note: It is also possible to specify models with only one or two time effects, by omitting the letters corresponding to the time effects to be excluded.
#' @param stratify_by       A string naming the column in \code{data} to use for stratification (e.g. region or sex).
#' @param reference_strata  Level of \code{stratify_by} to set as the reference level.
#' @param age               Name of the age variable in \code{data}.
#' @param period            Name of the period variable in \code{data}.
#' @param grid.factor       (Optional) Grid factor, defined as the ratio of age interval width to period interval width; defaults to 1.
#' @param apc_prior         (Optional) A string specifying the prior for the age, period, and cohort effects (e.g. \code{"rw1"}, \code{"rw2"}). Defaults to \code{"rw1"}.
#' @param extra.fixed       (Optional) If desired, the user can specify additional fixed effects to be added. This is passed as a character argument,
#'                                     specifying the name of the variable to be added. Multiple variables can be added by passing a character vector of names.
#'                                     Defaults to \code{NULL}.
#' @param extra.random      (Optional) If desired, the user can specify additional random effects to be added. This is passed as a character argument,
#'                                     specifying the name of the variable to be added. Multiple variables can be added by passing a character vector of names.
#'                                     Defaults to \code{NULL}.
#' @param extra.models      (Optional) If the user specifies one or more additional random effects to be added in \code{extra.random}, this argument can be used to specify the model to be used for the
#'                                     additional random effects. Either passed as a single string, in which case all extra random effects are assigned the same model, or a character vector
#'                                     matching the length of \code{extra.ranom}, mapping unique models to each variable in \code{extra.random}.
#'                                     If \code{NULL} and \code{extra.random} is non-empty, all extra random effects are assigned the "\code{iid}" model in \code{inla()}.
#'                                     Defaults to \code{NULL}.
#' @param extra.hyper       (Optional) If the user specifies one or more additional random effects to be added in \code{extra.random}, this argument can be used to specify the priors of the hyperparameters
#'                                     of the models used for the random effects. The hyperpriors are specified as strings that can be passed directly to the \code{hyper=...} argument in the formula
#'                                     passed to the \code{inla()}-function. See the argument \code{apc_prior} below for a concrete example. Defaults to \code{NULL}, in which case the default \code{INLA} priors are used.
#' @param include.random    (Optional) Logical; if \code{TRUE}, include an overall random effect in the APC model, to capture unobserved heterogeneity. Defaults to \code{FALSE}.
#' @param binomial.n        (Optional) For the \code{family=binomial} likelihood. Either an integer giving the number of trials for the binomial response, or the variable in \code{data} containing the number of trials for each observation.
#' @param poisson.offset    (Optional) For the \code{family=poisson} likelihood. Either an integer giving the denominator for the Poisson count response, or the variable in \code{data} containing the denominator for each observation.
#' @param inla_formula      (Optional) If desired, the user can pass its own INLA-compatible formula to define the model. If not, a formula is generated automatically, with the models and priors defined.
#' @param lincombs          (Optional) If desired, the user can pass its own INLA-compatible linear combinations to be computed by the \code{inla} program. See the \code{inla()}-function or \code{f()}-function documentations in \code{INLA} for details.
#' @param survey.design     (Optional) In the case of complex survey data, explicit handling of unequal sampling probabilities can be required.
#'                          The user can pass a \code{survey.design} object created with the \code{\link[survey]{svydesign}} function from the \pkg{survey} package.
#'                          In this case, a Gaussian model is fit for the survey adjusted estimates, based on the asymptotic normality of Hájek estimator.
#'                          The argument \code{family} should still indicate the underlying distribution of the response, and based on this, an appropriate transformation is applied to the adjusted mean estimates.
#' @param apc_hyperprior    (Optional) If the user wants non-default hyperpriors for the time effects, this can be achieved by passing the entire
#'                          prior specification as a string. If e.g. \code{hyper =  list(theta = list(prior="pc.prec", param=c(0.5,0.01)))} is desired, pass the string "\code{list(theta = list(prior="pc.prec", param=c(0.5,0.01)))}" to this argument.
#' @param control.compute   (Optional) A list of control variables passed to the \code{inla()}-function, that specifies what to be computed during model fitting. See options for \code{control.compute} in the \code{INLA} docs.
#'                          Defaults to \code{list(dic=TRUE, waic=TRUE, cpo=TRUE)}.
#'                          If posterior sampling is desired, \code{config=TRUE} must be passed as a control option inside \code{control.compute}.
#' @param verbose           (Optional) This is argument is passed along to the \code{inla()} function that estimates the MAPC model. If \code{verbose=TRUE}, the \code{inla}-program runs in verbose mode, which can provide more informative error messages.
#'
#' @return An named list, containing the following arguments:
#' \describe{
#'  \item{\code{model_fit}}{An object of class \code{"inla"}, containing posterior densities, posterior summaries, measures of model fit etc. See documentation for the \code{inla()}-function for details.}
#'  \item{\code{plots}}{A named list of plots for each time effect. Extract them as \code{plots\$age}/\code{plots\$period}\code{plots\$cohort}.}
#' }
#'
#' @details
#' This function works as a wrapper around the \code{inla()}-function from the \code{INLA} package, which executes the model fitting procedures using Integrated Neste Laplace Approximations.
#'
#' The returned object is of class \code{mapc}. S3 methods are available for:
#' - \code{print()}: Displays a concise summary of the model, including the APC format used, CPU time,
#'   number of estimated parameters (fixed, random, hyperparameters, linear combinations), and model fit scores (DIC, WAIC, log-score).
#' - \code{summary()}: Prints detailed posterior summaries of all estimated components, including fixed effects,
#'   random effects, hyperparameters, and linear combinations, as estimated by the \code{inla()}-function.
#' - \code{plot()}: Visualizes model estimates of cross-stata contrast trends, using precomputed plots stored in the object.
#'   The available plots depends on the APC-format that was used.
#'   You can control which effects to plot using the \code{which} argument (e.g. \code{which="age"} or \code{which=c("age", "period")}).
#'
#' @seealso \code{\link{fit_all_MAPC}} for fitting multiple models at once,
#'          and the function \code{inla()} from the \code{INLA} package for the estimation machinery.
#'          For complex survey data, see \code{\link[survey]{svydesign}} for the creation of a survey design object which can be passed to \code{survey.design}.
#'
#' @examples
#' \donttest{
#' data("toy_data")
#' fit <- fit_MAPC(
#'   data               = toy_data,
#'   response           = count,
#'   family             = "poisson",
#'   apc_format         = "ApC",
#'   stratify_by        = education,
#'   reference_strata   = 1,
#'   age                = age,
#'   period             = period
#' )
#'
#' # Print concise summary of the MAPC fit and the estimation procedure
#' print(fit)
#'
#' # Plot estimated cross-strata contrast trends
#' plot(fit)
#'
#' # Optional: view full summary of the model (can be long)
#' # summary(fit)
#' }
#'
#'
#' @references
#' Rue, H., Martino, S., & Chopin, N. (2009). Approximate Bayesian inference for latent Gaussian models by using Integrated Nested Laplace Approximations. *Journal of the Royal Statistical Society: Series B (Statistical Methodology)*, 71(2), 319-392. \doi{10.1111/j.1467-9868.2008.00700.x}
#' See also \url{https://www.r-inla.org} for more information about the INLA method and software.
#'
#' @export
fit_MAPC <- function(data,
                     response, family,
                     apc_format,
                     stratify_by, reference_strata=NULL,
                     age, period,
                     grid.factor = 1,
                     apc_prior="rw1",
                     extra.fixed = NULL,
                     extra.random = NULL,
                     extra.models = NULL,
                     extra.hyper = NULL,
                     include.random = FALSE,
                     binomial.n = NULL, poisson.offset = NULL,
                     inla_formula = NULL,
                     lincombs = NULL,
                     survey.design = NULL,
                     apc_hyperprior = NULL,
                     control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                     verbose = FALSE) {

  if (!requireNamespace("INLA", quietly = TRUE)) {
    stop("This function requires the INLA package. Please install it using:\n",
         'install.packages("INLA", repos = c(INLA = "https://inla.r-inla-download.org/R/stable"))')
  }

  start <- proc.time()

  # Tidy NSE
  response_q <- enquo(response)
  response_name <- as_name(response_q)
  strata_col_q <- enquo(stratify_by)
  strata_col_name <- as_name(strata_col_q)
  age_q <- enquo(age)
  age_name <- as_name(age_q)
  period_q <- enquo(period)
  period_name <- as_name(period_q)

  if(!is.factor(data[[strata_col_name]])) {stop("Strata_col needs to be a factor.")}
  if(!(reference_strata %in% levels(data[[strata_col_name]]))) {stop("'reference_strata' must be one of the levels of 'strata_col'.")}

  binomial.n <- enquo(binomial.n)
  poisson.offset <- enquo(poisson.offset)

  # If survey design
  if (! is.null(survey.design)) {
    if (! (family %in% c("gaussian","binomial","poisson"))) {
      stop('Complex survey design adjustments are only compatible with families "gaussian", "binomial" and "poisson".')
    }

    options(survey.lonely.psu = "adjust")

    resp_f <- reformulate(response_name)
    by_f   <- reformulate(c(strata_col_name, age_name, period_name))
    svydf <- svyby(
      formula = resp_f,
      by      = by_f,
      design  = survey.design,
      FUN     = svymean,
      keep.var= TRUE,
    )
    svydf <- as.data.frame(svydf)

    # pull off raw estimate & SE
    raw_mean <- svydf[[response_name]]
    raw_se   <- svydf[["se"]]

    if (family == "binomial") {
      # fix boundaries (can't take logit(0) or logit(1))
      zero_idx <- raw_mean == 0
      if (any(zero_idx)) {
        raw_mean[ zero_idx] <- 0.01
        raw_se[   zero_idx] <- 1
      }
      one_idx <- raw_mean == 1
      if (any(one_idx)) {
        raw_mean[ one_idx] <- 0.99
        raw_se[  one_idx] <- 1
      }

      # logit transform + precision
      p <- raw_mean
      svydf$transformed_mean  <- qlogis(p)
      svydf$transformed_prec  <- (p^2 * (1 - p)^2) / (raw_se^2)
    }

    if (family == "poisson") {
      # fix zeros (can't take log(0))
      zero_idx <- raw_mean == 0
      if (any(zero_idx)) {
        raw_mean[ zero_idx] <- 0.01
        raw_se[   zero_idx] <- 1
      }

      # log-transform + precision
      lambda <- raw_mean
      svydf$transformed_mean <- log(lambda)
      svydf$transformed_prec <- (lambda^2) / (raw_se^2)
    }

    if (family == "gaussian") {
      # identity link -> no transformation needed

      svydf$transformed_mean <- raw_mean
      svydf$transformed_prec <- 1 / (raw_se^2)
    }

    svydf.APC <- svydf %>% as.APC.df(!!sym(age_name), !!sym(period_name), M=grid.factor)
    svydf.APC.NA <- svydf.APC %>% as.APC.NA.df(stratify_by=!!strata_col_q,
                                               age="age_index", period="period_index", cohort="cohort_index",
                                               include.random=include.random)

    svydf.APC.NA[[strata_col_name]] <- as.factor(svydf.APC.NA[[strata_col_name]])

    svydf.APC.NA[[response_name]] <- svydf.APC.NA$transformed_mean

    data.APC.NA <- svydf.APC.NA

  } else{
    data.APC <- data %>% as.APC.df(!!sym(age_name), !!sym(period_name), M=grid.factor)

    data.APC.NA <- data.APC %>% as.APC.NA.df(stratify_by=!!strata_col_q,
                                             age="age_index", period="period_index", cohort="cohort_index",
                                             include.random=include.random)

    data.APC.NA[[strata_col_name]] <- as.factor(data.APC.NA[[strata_col_name]])
  }

  listed_format = str_split(apc_format, "")[[1]]

  subset1 <- data.APC.NA %>% filter(.data[[strata_col_name]] == levels(data.APC.NA[[strata_col_name]])[1])
  n_age <- length(unique(subset1[["age_index"]]))
  n_period <- length(unique(subset1[["period_index"]]))
  n_cohort <- length(unique(subset1[["cohort_index"]]))
  for (level in levels(data.APC.NA[[strata_col_name]])) {
    subset_temp <- data.APC.NA %>% filter(.data[[strata_col_name]] == level)
    n_age_temp <- length(unique(subset_temp[["age_index"]]))
    n_period_temp <- length(unique(subset_temp[["period_index"]]))
    n_cohort_temp <- length(unique(subset_temp[["cohort_index"]]))
    if(n_age != n_age_temp & "a" %in% listed_format) {stop("There are unobserved age indices in some strata. Can't fit model with stratum-specific age effects. The function plot_missing_data() is useful.")}
    if(n_period != n_period_temp & "p" %in% listed_format) {stop("There are unobserved period indices in some strata. Can't fit model with stratum-specific period effects. . The function plot_missing_data() is useful.")}
    if(n_cohort != n_cohort_temp & "c" %in% listed_format) {stop("There are unobserved cohort indices in some strata. Can't fit model with stratum-specific cohort effects. . The function plot_missing_data() is useful.")}
  }

  if(is.null(inla_formula)) {
    inla_formula <- generate_MAPC_formula(data.APC.NA, apc_format, response=response_name, stratify_var = strata_col_name,
                                        apc_prior=apc_prior, random_term=include.random,
                                        age="age_index", period="period_index", cohort="cohort_index",
                                        apc_hyper = apc_hyperprior,
                                        extra.fixed = extra.fixed,
                                        extra.random = extra.random,
                                        extra.models = extra.models,
                                        extra.hyper)
  } else {
    inla_formula <- inla_formula
  }

  if(is.null(lincombs)) {
    lincombs <- generate_apc_lincombs(apc_format, data.APC.NA,
                                    strata=strata_col_name, reference_strata = reference_strata,
                                    age="age_index", period="period_index", cohort="cohort_index")
  } else {
    lincombs <- lincombs
  }

  binomial.vec <- if (!quo_is_null(binomial.n)) eval_tidy(binomial.n, data = data.APC.NA) else NULL
  poisson.vec  <- if (!quo_is_null(poisson.offset)) eval_tidy(poisson.offset, data = data.APC.NA) else NULL


  if(!is.null(binomial.vec)) {data.APC.NA$Ntrials <- binomial.vec}
  if(!is.null(poisson.vec)) {data.APC.NA$E <- poisson.vec}


  if(!is.null(survey.design)) {
    fit <- INLA::inla(
      formula = inla_formula,
      data = data.APC.NA,
      family = "gaussian",
      control.compute = control.compute,
      lincomb = lincombs,
      verbose = verbose,
      control.family = list(hyper = list(prec = list(initial = log(1), fixed = TRUE))), scale = transformed_prec
    )
  } else if(family=="binomial" & !is.null(binomial.vec)) {
    fit <- INLA::inla(
      formula = inla_formula,
      data = data.APC.NA,
      family = family,
      control.compute = control.compute,
      lincomb = lincombs,
      Ntrials = Ntrials,
      verbose = verbose
    )
  } else if(family=="poisson" & !is.null(poisson.vec)) {
    fit <- INLA::inla(
      formula = inla_formula,
      data = data.APC.NA,
      family = family,
      control.compute = control.compute,
      lincomb = lincombs,
      E = E,
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

  plots <- NULL  # predefine outside tryCatch

  tryCatch({
    age_vals <- sort(unique(na.omit(data.APC.NA[[age_name]])))
    period_vals <- sort(unique(na.omit(data.APC.NA[[period_name]])))
    cohort_vals <- sort(unique(na.omit(data.APC.NA[["cohort"]])))
    plots <- plot_lincombs(fit, apc_format, data.APC.NA,
                           strata_col = strata_col_name, reference_level = reference_strata,
                           family = family,
                           age_ind = "age_index", period_ind = "period_index", cohort_ind = "cohort_index",
                           age_vals = age_vals,
                           period_vals = period_vals,
                           cohort_vals = cohort_vals)
  }, error = function(e) {
    message("Generation of plots gave an error: ", e$message)
    message("Only the model_fit is returned.")
  })

  end <- proc.time()

  tot.time <- end - start

  result <- list(model_fit = fit, plots = plots, apc_format=apc_format, time.used = tot.time)
  class(result) <- "mapc"
  return(result)
}


#################################################################################################
#' Fit all configurations of MAPC models using INLA
#'
#' Fits all configurations of shared vs. stratum-specific time effects:
#' \describe{
#'  \item{APc}{Shared age and period effects, stratum-specific cohort effects.}
#'  \item{ApC}{Shared age and cohort effects, stratum-specific period effects.}
#'  \item{aPC}{Shared period and cohort effects, stratum-specific age effects.}
#'  \item{Apc}{Shared age effects, stratum-specific period and cohort effects.}
#'  \item{aPc}{Shared period effects, stratum-specific age and cohort effects.}
#'  \item{apC}{Shared cohort effects, stratum-specific age and period effects.}
#' }
#' Uses the \code{\link{fit_MAPC}} function.
#' The multivariate APC model is based on Riebler and Held (2010) \doi{10.1093/biostatistics/kxp037}.
#' For handling complex survey data, we follow Mercer et al. (2014) \doi{10.1016/j.spasta.2013.12.001},
#' implemented using the \pkg{survey} package.
#'
#' @param data              A data frame containing the age, period, response, and stratification variables.
#'                          Age and period are assumed to be on the raw scale, not transformed to 1-indexed index columns.
#'                          Factor/character columns are handled, as long as they are properly sorted by \code{sort(unique(data$age/period))} (e.g. values of the form "20-25" for age groups are handled).
#' @param response          A string naming the response (outcome) variable in \code{data}.
#' @param family            A string indicating the likelihood family. The default is \code{"gaussian"} with identity link.
#' @param stratify_by       The column in \code{data} to use for stratification.
#' @param reference_strata  Level of \code{stratify_by} to set as the reference level.
#' @param age               The age column in \code{data}.
#' @param period            The period column in \code{data}.
#' @param grid.factor       (Optional) Grid factor, defined as the ratio of age interval width to period interval width; defaults to 1.
#' @param all_models        (Optional) Character vectors of valid APC-formats (e.g. \code{c("ApC", "apC", "APc"))}, specifying the MAPC models to be estimated.
#'                          Requirements for a valid APC-format (lowercase letter means stratum-specific, uppercase means shared):
#'                            - Only one time effect: shared/stratum-specific both fine.
#'                            - Two time effects: shared/stratum-specific both fine.
#'                            - Three time effects: either one or two must be stratum-specific.
#'                          Defaults to \code{c("apC", "aPc", "Apc", "aPC", "ApC", "APc")}.
#' @param extra.fixed       (Optional) If desired, the user can specify additional fixed effects to be added. This is passed as a character argument,
#'                                     specifying the name of the variable to be added. Multiple variables can be added by passing a character vector of names.
#'                                     Defaults to \code{NULL}.
#' @param extra.random      (Optional) If desired, the user can specify additional random effects to be added. This is passed as a character argument,
#'                                     specifying the name of the variable to be added. Multiple variables can be added by passing a character vector of names.
#'                                     Defaults to \code{NULL}.
#' @param extra.models      (Optional) If the user specifies one or more additional random effects to be added in \code{extra.random}, this argument can be used to specify the model to be used for the
#'                                     additional random effects. Either passed as a single string, in which case all extra random effects are assigned the same model, or a character vector
#'                                     matching the length of \code{extra.ranom}, mapping unique models to each variable in \code{extra.random}.
#'                                     If \code{NULL} and \code{extra.random} is non-empty, all extra random effects are assigned the "\code{iid}" model in \code{inla()}.
#'                                     Defaults to \code{NULL}.
#' @param extra.hyper       (Optional) If the user specifies one or more additional random effects to be added in \code{extra.random}, this argument can be used to specify the priors of the hyperparameters
#'                                     of the models used for the random effects. The hyperpriors are specified as strings that can be passed directly to the \code{hyper=...} argument in the formula
#'                                     passed to the \code{inla()}-function. See the argument \code{apc_prior} below for a concrete example. Defaults to \code{NULL}, in which case the default \code{INLA} priors are used.
#' @param apc_prior         (Optional) A string specifying the prior for the age, period, and cohort effects (e.g. \code{"rw1"}, \code{"rw2"}). Defaults to \code{"rw1"}.
#' @param include.random    (Optional) Logical; if \code{TRUE}, include an overall random effect in the APC model. Defaults to \code{FALSE}.
#' @param binomial.n        (Optional) For the \code{family=binomial} likelihood. Either an integer giving the number of trials for the binomial response, or the name of the column containing the number of trials for each observation.
#' @param poisson.offset    (Optional) For the \code{family=poisson} likelihood. Either an integer giving the denominator for the Poisson count response, or the name of the column containing the denominator for each observation.
#' @param apc_hyperprior    (Optional) If the user wants non-default hyperpriors for the time effects, this can be achieved by passing the entire
#'                          prior specification as a string. If e.g. \code{hyper =  list(theta = list(prior="pc.prec", param=c(0.5,0.01)))} is desired, pass the string "\code{list(theta = list(prior="pc.prec", param=c(0.5,0.01)))}" to this argument.
#' @param survey.design     (Optional) In the case of complex survey data, explicit handling of unequal sampling probabilities can be required.
#'                          The user can pass a \code{survey.design} object created with the \code{\link[survey]{svydesign}} function from the \pkg{survey} package.
#'                          In this case, a Gaussian model is fit for the survey adjusted estimates, based on the asymptotic normality of Hájek estimator.
#'                          The argument \code{family} should still indicate the underlying distribution of the response, and based on this, an appropriate transformation is applied to the adjusted mean estimates.
#' @param control.compute   (Optional) A list of control variables passed to the \code{inla()}-function, that specifies what to be computed during model fitting. See options for \code{control.compute} in the \code{INLA} docs.
#'                          Defaults to \code{list(dic=TRUE, waic=TRUE, cpo=TRUE)}.
#'                          If posterior sampling is desired, \code{config=TRUE} must be passed as a control option inside \code{control.compute}.
#' @param track.progress    (Optional) Whether to report progress of the estimation of models in the console; defaults to \code{FALSE}.
#' @param verbose           (Optional) This is argument is passed along to the \code{inla()} function that estimates the MAPC model. If \code{verbose=TRUE}, the \code{inla}-program runs in verbose mode, which can provide more informative error messages.
#'
#' @return A named list of \code{mapc} objects, one for each configuration of shared vs. stratum-specific time effects: APc, ApC, aPC, Apc, aPc, apC.
#'
#' @details
#' The returned object is of class \code{all_mapc}, which is a container for multiple \code{mapc} model fits (each typically fitted with a different APC formats).
#' It also contains a \code{model_selection} element, which holds plots summarizing comparative fit metrics (DIC, WAIC and log-scores).
#'
#' The following S3 methods are available:
#' - \code{print()}: Prints a compact summary for each individual model fit.
#' - \code{summary()}: Calls \code{summary()} on each contained \code{mapc} object, providing detailed posterior summaries.
#' - \code{plot()}: Displays model comparison plots (DIC/WAIC/log-score comparisons).
#'
#' These methods are intended to streamline multi-model workflows and allow quick comparison of results across model specifications.
#'
#' @seealso \code{\link{fit_MAPC}} for fitting a single model (more flexible; can pass your own formula and lincombs),
#'          and the function \code{inla()} from the \code{INLA} package for the estimation machinery.
#'          For complex survey data, see \code{\link[survey]{svydesign}} for the creation of a survey design object which can be passed to \code{survey.design}.
#'
#' @examples
#' \donttest{
#' data("toy_data")
#' fits <- fit_all_MAPC(
#'   data               = toy_data,
#'   response           = count,
#'   family             = "poisson",
#'   stratify_by        = education,
#'   reference_strata   = 1,
#'   age                = age,
#'   period             = period,
#'   apc_prior          = "rw2",
#'   include.random     = TRUE
#' )
#'
#' # Print concise summary of the models and estimation procedure
#' print(fits)
#'
#' # Plot comparison plots, based on comparative fit metrics
#' plot(fits)
#'
#' # Optional: view full summary of all models (can be long)
#' # summary(fits)
#' }
#'
#' @references
#' Rue, H., Martino, S., & Chopin, N. (2009). Approximate Bayesian inference for latent Gaussian models by using Integrated Nested Laplace Approximations. *Journal of the Royal Statistical Society: Series B (Statistical Methodology)*, 71(2), 319-392. \doi{10.1111/j.1467-9868.2008.00700.x}
#' See also \url{https://www.r-inla.org} for more information about the INLA method and software.
#'
#' @export
fit_all_MAPC <- function(data, response, family, stratify_by, reference_strata=NULL,
                     age="age", period="period",
                     grid.factor = 1,
                     all_models = c("apC", "aPc", "Apc", "aPC", "ApC", "APc"),
                     extra.fixed = NULL,
                     extra.random = NULL,
                     extra.models = NULL,
                     extra.hyper = NULL,
                     apc_prior="rw1",
                     include.random= FALSE,
                     binomial.n = NULL,
                     poisson.offset = NULL,
                     apc_hyperprior = NULL,
                     survey.design = NULL,
                     control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                     track.progress = FALSE,
                     verbose = FALSE) {

  tot.start <- proc.time()

  # Tidy NSE
  response_q <- enquo(response)
  response_name <- as_name(response_q)
  strata_col_q <- enquo(stratify_by)
  strata_col_name <- as_name(strata_col_q)
  age_q <- enquo(age)
  age_name <- as_name(age_q)
  period_q <- enquo(period)
  period_name <- as_name(period_q)

  binomial.n <- enquo(binomial.n)
  poisson.offset <- enquo(poisson.offset)

  if (!requireNamespace("INLA", quietly = TRUE)) {
    stop("This function requires the INLA package. Please install it using:\n",
         'install.packages("INLA", repos = c(INLA = "https://inla.r-inla-download.org/R/stable"))')
  }

  if(!is.factor(data[[strata_col_name]])) {stop("Strata_col needs to be a factor.")}
  if(!(reference_strata %in% levels(data[[strata_col_name]]))) {stop("'reference_strata' must be one of the levels of 'strata_col'.")}

  # If survey design
  if (! is.null(survey.design)) {
    if (! (family %in% c("gaussian","binomial","poisson"))) {
      stop('Complex survey design adjustments are only compatible with families "gaussian", "binomial" and "poisson".')
    }

    options(survey.lonely.psu = "adjust")

    resp_f <- reformulate(response_name)
    by_f   <- reformulate(c(strata_col_name, age_name, period_name))
    svydf <- svyby(
      formula = resp_f,
      by      = by_f,
      design  = survey.design,
      FUN     = svymean,
      keep.var= TRUE,
    )
    svydf <- as.data.frame(svydf)

    # pull off raw estimate & SE
    raw_mean <- svydf[[response_name]]
    raw_se   <- svydf[["se"]]

    if (family == "binomial") {
      # fix boundaries (can't take logit(0) or logit(1))
      zero_idx <- raw_mean == 0
      if (any(zero_idx)) {
        raw_mean[ zero_idx] <- 0.01
        raw_se[   zero_idx] <- 1
      }
      one_idx <- raw_mean == 1
      if (any(one_idx)) {
        raw_mean[ one_idx] <- 0.99
        raw_se[  one_idx] <- 1
      }

      # logit transform + precision
      p <- raw_mean
      svydf$transformed_mean  <- qlogis(p)
      svydf$transformed_prec  <- (p^2 * (1 - p)^2) / (raw_se^2)
    }

    if (family == "poisson") {
      # fix zeros (can't take log(0))
      zero_idx <- raw_mean == 0
      if (any(zero_idx)) {
        raw_mean[ zero_idx] <- 0.01
        raw_se[   zero_idx] <- 1
      }

      # log-transform + precision
      lambda <- raw_mean
      svydf$transformed_mean <- log(lambda)
      svydf$transformed_prec <- (lambda^2) / (raw_se^2)
    }

    if (family == "gaussian") {
      # identity link -> no transformation needed

      svydf$transformed_mean <- raw_mean
      svydf$transformed_prec <- 1 / (raw_se^2)
    }

    svydf.APC <- svydf %>% as.APC.df(!!sym(age_name), !!sym(period_name), M=grid.factor)
    svydf.APC.NA <- svydf.APC %>% as.APC.NA.df(stratify_by=!!strata_col_q,
                                               age="age_index", period="period_index", cohort="cohort_index",
                                               include.random=include.random)

    svydf.APC.NA[[strata_col_name]] <- as.factor(svydf.APC.NA[[strata_col_name]])

    svydf.APC.NA[[response_name]] <- svydf.APC.NA$transformed_mean

    data.APC.NA <- svydf.APC.NA

  } else{
    data.APC <- data %>% as.APC.df(!!sym(age_name), !!sym(period_name), M=grid.factor)

    data.APC.NA <- data.APC %>% as.APC.NA.df(stratify_by=!!strata_col_q,
                                             age="age_index", period="period_index", cohort="cohort_index",
                                             include.random=include.random)

    data.APC.NA[[strata_col_name]] <- as.factor(data.APC.NA[[strata_col_name]])
  }

  binomial.vec <- if (!quo_is_null(binomial.n)) eval_tidy(binomial.n, data = data.APC.NA) else NULL
  poisson.vec  <- if (!quo_is_null(poisson.offset)) eval_tidy(poisson.offset, data = data.APC.NA) else NULL

  if(!is.null(binomial.vec)) {data.APC.NA$Ntrials <- binomial.vec}
  if(!is.null(poisson.vec)) {data.APC.NA$E <- poisson.vec}

  results <- list()
  logscores <- list()
  dic_scores <- list()
  waic_scores <- list()

  check_all_models <- function(all_models) {
    if (!is.vector(all_models) || !is.character(all_models)) {
      stop("Error: 'all_models' must be a character vector.")
    }
  }

  check_all_models(all_models)

  for (apc_format in all_models) {

    listed_format = str_split(apc_format, "")[[1]]

    subset1 <- data.APC.NA %>% filter(.data[[strata_col_name]] == levels(data.APC.NA[[strata_col_name]])[1])
    n_age <- length(unique(subset1[["age_index"]]))
    n_period <- length(unique(subset1[["period_index"]]))
    n_cohort <- length(unique(subset1[["cohort_index"]]))

    skip <- FALSE  # flag to control skipping

    for (level in levels(data.APC.NA[[strata_col_name]])) {
      subset_temp <- data.APC.NA %>% filter(.data[[strata_col_name]] == level)
      n_age_temp <- length(unique(subset_temp[["age_index"]]))
      n_period_temp <- length(unique(subset_temp[["period_index"]]))
      n_cohort_temp <- length(unique(subset_temp[["cohort_index"]]))

      if(n_age != n_age_temp & "a" %in% listed_format) {
        warning(sprintf("Model '%s': Unobserved age indices in some strata. Can't fit model with stratum-specific age effects. Skipping this model.", apc_format), immediate. = TRUE)
        skip <- TRUE
        break
      }
      if(n_period != n_period_temp & "p" %in% listed_format) {
        warning(sprintf("Model '%s': Unobserved period indices in some strata. Can't fit model with stratum-specific period effects. Skipping this model.", apc_format), immediate. = TRUE)
        skip <- TRUE
        break
      }
      if(n_cohort != n_cohort_temp & "c" %in% listed_format) {
        warning(sprintf("Model '%s': Unobserved cohort indices in some strata. Can't fit model with stratum-specific cohort effects. Skipping this model.", apc_format), immediate. = TRUE)
        skip <- TRUE
        break
      }
    }
    if (skip) next

    mod.start <- proc.time()

    inla_formula <- generate_MAPC_formula(data.APC.NA, apc_format, response=response_name, stratify_var = strata_col_name,
                                          apc_prior=apc_prior, random_term=include.random,
                                          age="age_index", period="period_index", cohort="cohort_index",
                                          apc_hyper = apc_hyperprior,
                                          extra.fixed = extra.fixed,
                                          extra.random = extra.random,
                                          extra.models = extra.models,
                                          extra.hyper = extra.hyper)

    lincombs <- generate_apc_lincombs(apc_format, data.APC.NA,
                                      strata=strata_col_name, reference_strata = reference_strata,
                                      age="age_index", period="period_index", cohort="cohort_index")

    start <- proc.time()

    if(track.progress) {cat("Estimating", apc_format, "model...\n")}
    if(!is.null(survey.design)) {
      fit <- INLA::inla(
        formula = inla_formula,
        data = data.APC.NA,
        family = "gaussian",
        control.compute = control.compute,
        lincomb = lincombs,
        verbose = verbose,
        control.family = list(hyper = list(prec = list(initial = log(1), fixed = TRUE))), scale = transformed_prec
      )
    } else if(family=="binomial" & !is.null(binomial.vec)) {
      fit <- INLA::inla(
        formula = inla_formula,
        data = data.APC.NA,
        family = family,
        control.compute = control.compute,
        lincomb = lincombs,
        Ntrials = Ntrials,
        verbose = verbose
      )
    } else if(family=="poisson" & !is.null(poisson.vec)) {
      data.APC.NA$._offset_temp_ <- poisson.vec
      fit <- INLA::inla(
        formula = inla_formula,
        data = data.APC.NA,
        family = family,
        control.compute = control.compute,
        E = E,
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

    if(track.progress) {cat("...", apc_format, "model finished.\n\n")}

    tryCatch({
      age_vals <- sort(unique(na.omit(data.APC.NA[[age_name]])))
      period_vals <- sort(unique(na.omit(data.APC.NA[[period_name]])))
      cohort_vals <- sort(unique(na.omit(data.APC.NA[["cohort"]])))
      plots <- plot_lincombs(fit, apc_format, data.APC.NA,
                             strata_col = strata_col_name, reference_level = reference_strata,
                             family = family,
                             age_ind = "age_index", period_ind = "period_index", cohort_ind = "cohort_index",
                             age_vals = age_vals,
                             period_vals = period_vals,
                             cohort_vals = cohort_vals)
    }, error = function(e) {
      message("Generation of plots gave an error: ", e$message)
      message("Only the model_fit is returned.")
    })

    tryCatch({
      logscores[[apc_format]] <- -mean(log(fit$cpo$cpo), na.rm=TRUE)
    }, error = function(e) {
      message("Couldn't extract CPO score.")
      message("Error: ", e$message)
    })
    tryCatch({
      dic_scores[[apc_format]] <- fit$dic$dic
    }, error = function(e) {
      message("Couldn't extract DIC score.")
      message("Error: ", e$message)
    })
    tryCatch({
      waic_scores[[apc_format]] <- fit$waic$waic
    }, error = function(e) {
      message("Couldn't extract WAIC score.")
      message("Error: ", e$message)
    })

    mod.end <- proc.time()

    mod.time <- mod.end - mod.start

    result <- list(model_fit = fit, plots = plots, apc_format=apc_format, time.used=mod.time)
    class(result) <- "mapc"

    results[[apc_format]] <- result
  }

  tryCatch({
    model_fit_table <- model_selection_criteria_table(dic_scores = dic_scores,
                                                      waic_scores = waic_scores,
                                                      log_scores = logscores)
    DIC_plot <- plot_model_selection_criteria(dic_scores, title = "DIC scores")
    WAIC_plot <- plot_model_selection_criteria(waic_scores, title = "WAIC scores")
    logscore_plot <- plot_model_selection_criteria(logscores, title = "Log-scores")
    results[["model_selection"]] <- list(summary_table = model_fit_table,
                                         DIC_plot = DIC_plot,
                                         WAIC_plot = WAIC_plot,
                                         logscore_plot = logscore_plot)
  }, error = function(e) {
    message("Failed when trying to summarize model fit scores. Returning named lists with scores instead.")
    message(e)
    results[["model_selection"]] <- NULL
    results[["dic_scores"]] <- dic_scores
    results[["waic_scores"]] <- waic_scores
    results[["logscores"]] <- logscores
  })

  tot.end <- proc.time()

  tot.time <- tot.end - tot.start

  results$time.used <- tot.time

  class(results) <- "all_mapc"

  return(results)
}
