% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simitation.R
\name{sim.chisq.gf}
\alias{sim.chisq.gf}
\title{sim.chisq.gf}
\usage{
sim.chisq.gf(
  n,
  values,
  prob = NULL,
  num.experiments = 1,
  experiment.name = "experiment",
  value.name = "x",
  seed = 91,
  vstr = "3.6"
)
}
\arguments{
\item{n}{A numeric value indicating the number of observations in each experiment.}

\item{values}{A numeric vector specifying the possible values (sample space).}

\item{prob}{A numeric vector of probabilities corresponding to the values for simulation.
If not provided, equal probabilities are assumed for all values.}

\item{num.experiments}{An integer indicating the number of simulated experiments to conduct.}

\item{experiment.name}{A character string specifying the column name for identifying each experiment in the output.}

\item{value.name}{A character string specifying the column name for the simulated values in the output.}

\item{seed}{An integer specifying the seed for reproducibility. Default is 91.}

\item{vstr}{A numeric or character string specifying the seed for random number generation to ensure reproducibility. Default is "3.6". For more details, refer to \code{\link[base]{set.seed}}.}
}
\value{
A 'data.table' containing the simulated experiments with specified
column names.
}
\description{
Simulate data for chi-squared tests of goodness of fit across experiments.
}
\examples{
simdat.chisq.gf <- sim.chisq.gf(n = 100, values = LETTERS[1:4],
 prob = c(0.4, 0.3, 0.2, 0.1), num.experiments = 2, experiment.name =
  "experiment_id", value.name = "classification", seed = 31)
}
