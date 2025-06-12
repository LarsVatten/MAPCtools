#' Synthetic Age-Period-Cohort Dataset
#'
#' A toy dataset generated to illustrate modeling of age, period, and cohort effects, including interactions with education and sex.
#' This data simulates count outcomes (e.g., disease incidence or event counts) as a function of demographic variables using a Poisson process.
#'
#' The underlying event rate is modeled on the log scale as a linear combination of age, period, sex, education, and an age-education interaction.
#' The count outcome is drawn from a Poisson distribution with this rate. This dataset is ideal for testing APC-type models.
#'
#' @format A tibble with 10,000 rows and 7 variables:
#' \describe{
#'   \item{age}{Age of individuals, sampled uniformly from 20 to 59.}
#'   \item{period}{Calendar year of observation, sampled uniformly from 1990 to 2019.}
#'   \item{education}{Ordered factor for education level, with levels:
#'     \enumerate{
#'       \item Low
#'       \item Medium
#'       \item High
#'     }}
#'   \item{sex}{Factor indicating biological sex, with levels: "male", "female".}
#'   \item{count}{Simulated event count, generated from a Poisson distribution.}
#'   \item{known_rate}{The true Poisson rate used to generate \code{count}, computed from the log-linear model.}
#'   \item{cohort}{Derived variable indicating year of birth (period - age).}
#' }
#' @details
#' The true log-rate is computed as:
#' \deqn{
#' \log(\lambda) = \beta_0 + \beta_\text{period} (2020 - \text{period}) + \beta_\text{sex} \cdot I(\text{female}) +
#' \beta_\text{edu} (\text{edu level}) + \beta_\text{edu-age} \cdot (\text{age effect}) \cdot (\text{edu level})
#' }
#' where age effect increases to age 40 and decreases after. The coefficients used are:
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
