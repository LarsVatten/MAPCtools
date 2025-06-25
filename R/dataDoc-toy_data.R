#' Synthetic Age-Period-Cohort Dataset
#'
#' A toy dataset generated to illustrate modeling of age, period, and cohort effects, including interactions with education and sex.
#' This data simulates count outcomes (e.g., disease incidence or event counts) as a function of demographic variables using a Poisson process.
#'
#' The underlying event rate is modeled on the log scale as a linear combination of age, period, sex, education, and an age-education interaction.
#' The count outcome is drawn from a Poisson distribution with this rate. This dataset is handy for testing APC models.
#'
#' @format A data frame with 10000 rows and 7 variables:
#' \describe{
#'   \item{age}{Age of individuals, sampled uniformly from 20 to 59.}
#'   \item{period}{Calendar year of observation, sampled uniformly from 1990 to 2019.}
#'   \item{education}{Factor for education level, with levels 1, 2 and 3.}
#'   \item{sex}{Factor indicating biological sex, with levels: "male", "female".}
#'   \item{count}{Simulated event count, generated from a Poisson distribution.}
#'   \item{known_rate}{The true Poisson rate used to generate \code{count}, computed from the log-linear model.}
#'   \item{cohort}{Derived variable indicating year of birth (period - age).}
#' }
#' @details
#' @details
#' The true log-rate is computed (for observation \eqn{n}) as:
#' \deqn{
#' \log(\lambda_n)
#'   = \beta_0
#'   + \beta_{\text{period}}\,\bigl(2020 - \text{period}_n\bigr)
#'   + \beta_{\text{sex}}\,I(\text{sex}_n = \text{female}) \\[6pt]
#' \quad
#'   + \beta_{\text{edu}}\,(\text{edu level}_n)
#'   + \beta_{\text{edu-age}}\,(\text{age}_n - 20)\,(\text{edu level}_n - 1)\,I(\text{age}_n \le 40) \\[6pt]
#' \quad
#'   + \beta_{\text{edu-age}}\,(60 - \text{age}_n)\,(\text{edu level}_n - 1)\,I(\text{age}_n > 40)
#' }
#' where the rate decreases over time (periods), increases with age up to age 40, and decreases after. The coefficients used are:
#' \itemize{
#'   \item \code{intercept = 1.0}
#'   \item \code{b_period = 0.02}
#'   \item \code{b_sex = 0.5} (female effect)
#'   \item \code{b_education_base = 0.5}
#'   \item \code{b_education_age_interaction = 0.015}
#' }
#'
#' @usage data(toy_data)
#' @keywords datasets simulation APC
#' @name toy_data
#' @source Simulated data, created using base R and \pkg{tibble}.
"toy_data"
