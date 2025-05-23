% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MAPCtools.R
\name{generate_apc_lincombs}
\alias{generate_apc_lincombs}
\title{Generate APC Linear Combinations}
\usage{
generate_apc_lincombs(apc_format, data)
}
\arguments{
\item{apc_format}{APC-format with lower-case letters indicating stratum-specific time effects, upper-case indicating shared.}

\item{data}{Data frame for which the MAPC model will be estimated.}
}
\value{
Object of linear combinations that can be passed to INLA.
}
\description{
Generates the proper linear combination object that can be passed to INLA to define linear combinations of linear predictors in an MAPC model. Depends on the configurations of shared vs. stratum-specific time effects. Lowercase letters in "APC" indicates time effect is stratum-specific, upper-case indicates time effect is shared.
}
