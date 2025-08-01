% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/export-DataPreprocessing.R
\name{as.APC.df}
\alias{as.APC.df}
\title{Add 1-indexed APC columns to data frame, handling numeric or categorical age/period}
\usage{
as.APC.df(data, age, period, age_order = NULL, period_order = NULL, M = 1)
}
\arguments{
\item{data}{Data frame with age and period columns.}

\item{age}{Age column in \code{data}.}

\item{period}{Period column in \code{data}.}

\item{age_order}{(Optional) Character vector giving the desired order of age levels.
If NULL and the \code{age} column is factor/character, uses \code{unique(sort(data[[age]]))}.}

\item{period_order}{(Optional) Vector (numeric or character) giving the desired order of periods.
If NULL and \code{period} column is a factor/character, uses \code{unique(sort(data[[period]]))}.}

\item{M}{Grid factor, defined as the ratio of age interval width to period interval width.
Defaults to 1 (i.e. assuming equal sized age and period increments).}
}
\value{
\if{html}{\out{<div class="sourceCode">}}\preformatted{           The data frame with new columns \code{age_index}, \code{period_index}, \code{cohort_index},
                   and sorted by \code{(age_index, period_index)}.
}\if{html}{\out{</div>}}
}
\description{
Add 1-indexed APC columns to data frame, handling numeric or categorical age/period
}
