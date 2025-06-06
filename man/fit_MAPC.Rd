% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/export-ModelEstimation.R
\name{fit_MAPC}
\alias{fit_MAPC}
\title{Fit a multivariable Age-Period-Cohort model using INLA}
\usage{
fit_MAPC(
  data,
  response,
  apc_format,
  family,
  strata_col,
  reference_strata = NULL,
  age = "age",
  period = "period",
  apc_prior = "rw1",
  random = T,
  control.compute = list(config = TRUE, dic = TRUE, waic = TRUE, cpo = TRUE),
  binomial.n = NULL,
  poisson.offset = NULL,
  verbose = F
)
}
\arguments{
\item{data}{A \code{data.frame} containing the raw age, period, response, and stratification variables.}

\item{response}{A string naming the response (outcome) variable in \code{data}.}

\item{apc_format}{A specification of the APC structure (e.g. "APC", "AC", "AP", etc.), passed to the formula generator.}

\item{family}{Likelihood family of the response variable}

\item{strata_col}{A string naming the column in \code{data} to use for stratification (e.g. region or sex).}

\item{reference_strata}{Optional; value(s) of \code{strata_col} to set as the reference level(s). Defaults to \code{NULL}.}

\item{age}{Name of the age variable in \code{data}. Defaults to \code{"age"}.}

\item{period}{Name of the period variable in \code{data}. Defaults to \code{"period"}.}

\item{apc_prior}{A string specifying the prior for the age, period, and cohort effects (e.g. \code{"rw1"}, \code{"rw2"}). Defaults to \code{"rw1"}.}

\item{random}{Logical; if \code{TRUE}, include an overall random effect in the APC model. Defaults to \code{TRUE}.}

\item{control.compute}{A list of control.compute options passed to \code{INLA} (e.g. \code{config}, \code{dic}, \code{waic}, \code{cpo}). Defaults to \code{list(config=TRUE, dic=TRUE, waic=TRUE, cpo=TRUE)}.}

\item{binomial.n}{For the \code{family=binomial} likelihood. Either an integer giving the number of trials for the binomial response, or the name of the column containing the number of trials for each observation.}

\item{poisson.offset}{For the \code{family=poisson} likelihood. Either an integer giving the denominator for the Poisson count response, or the name of the column containing the denominator for each observation.}

\item{verbose}{This is argument is passed along to the \code{inla()} function that estimates the MAPC model. If \code{verbose=TRUE}, the \code{inla}-program runs in verbose mode, which can provide more informative error messages.}
}
\value{
An named list, containing the following arguments:
\describe{
\item{\code{model_fit}}{An object of class \code{"inla"}, containing posterior densities, posterior summaries, measures of model fit etc. See \code{\link[INLA]{inla}} for details.}
\item{\code{plots}}{A named list of plots for each time effect. Extract them as \code{plots\$age}/\code{plots\$period}\code{plots\$cohort}.}
}
}
\description{
Takes a raw data frame, converts it to APC format (1-indexed age, period, cohort),
masks out NA cells in the APC grid, builds the INLA formula and linear combinations,
and then fits the specified MAPC model via \code{INLA}.
}
\examples{
\dontrun{
fit <- fit_MAPC(
  data               = mydata,
  response           = "deaths",
  apc_format         = "APC",
  strata_col         = "region",
  reference_strata   = "North",
  age                = "age_group",
  period             = "year",
  apc_prior          = "rw1",
  random             = TRUE
)
}

}
