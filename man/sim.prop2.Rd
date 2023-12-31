% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simitation.R
\name{sim.prop2}
\alias{sim.prop2}
\title{sim.prop2}
\usage{
sim.prop2(
  nx,
  ny,
  px = 0.5,
  py = 0.5,
  num.experiments = 1,
  experiment.name = "experiment",
  group.name = "group",
  x.value = "x",
  y.value = "y",
  value.name = "value",
  seed = 3471,
  vstr = 3.6
)
}
\arguments{
\item{nx}{A numeric value for the number of observations in the x group for each experiment.}

\item{ny}{A numeric value for the number of observations in the y group for each experiment.}

\item{px}{A numeric value for the probability of success in the x group.}

\item{py}{A numeric value for the probability of success in the y group.}

\item{num.experiments}{A numeric value representing the number of simulated experiments.}

\item{experiment.name}{A character value providing the name for the column identifying the experiment.}

\item{group.name}{A character value providing the name of the column of the group labels.}

\item{x.value}{A character value specifying the label used for data in the x group (in the column labled by the group.name parameter).}

\item{y.value}{A character value specifying the label used for data in the y group (in the column labled by the group.name parameter).}

\item{value.name}{A character value specifying the name of the column that contains the value of the simulated data.}

\item{seed}{A single numeric value, interpreted as an integer, or NULL.   See help(set.seed).}

\item{vstr}{A character string containing a version number, e.g., "1.6.2". The default RNG configuration of the current R version is used if vstr is greater than the current version.  See help(set.seed).}
}
\value{
A 'data.table' with simulated data for two groups ('x' and 'y') based
        on binomial distributions. The table contains columns corresponding to
        the experiment identifier, group labels ('x' or 'y').
}
\description{
sim.prop2
}
\examples{
simdat.prop2 <- sim.prop2(nx = 30, ny = 40, px = 0.5,
py = 0.55, num.experiments = 2, experiment.name = "sim",
group.name = "treatment", x.value = "group_1", y.value = "group_2",
value.name = "correct_answer", seed = 3)
}
