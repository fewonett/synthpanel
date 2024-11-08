% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary_measures.R
\name{att_simple}
\alias{att_simple}
\title{Calculates the ATT_simple}
\usage{
att_simple(treatment_effect, treat)
}
\arguments{
\item{treatment_effect}{The vector of true individual treatment effects as generated
by the DGP functions.}

\item{treat}{The treatment indicator: 1 when if respective unit is treated in that
time perios, 0 otherwise.}
}
\value{
An integer representing the ATT_simple.
}
\description{
Calculates the ATT_simple
}
