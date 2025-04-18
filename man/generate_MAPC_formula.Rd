% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MAPCtools.R
\name{generate_MAPC_formula}
\alias{generate_MAPC_formula}
\title{Generate MAPC formula for INLA}
\usage{
generate_MAPC_formula(
  df,
  APC_format,
  stratify_var,
  model = "rw1",
  random_term = TRUE
)
}
\arguments{
\item{df}{Data frame for which MAPC models should be fit}

\item{APC_format}{A string where lower-case letters indicate stratum-specific time effects and upper-case letters indicate shared time effects.}

\item{stratify_var}{Stratification variable. At least one time effect should be stratum-specific, and at least one should be shared.}

\item{model}{Which prior model to use for the time effects.

\strong{Defaults to "rw1" (optional).}}

\item{random_term}{Indicator, indicating if a random term should be included in the model

\strong{Defaults to TRUE (optional).}}
}
\value{
A formula object that can be passed to INLA to fit the desired MAPC model.
}
\description{
Based on APC-format, generate the proper formula to pass to INLA for fitting MAPC models.
}
