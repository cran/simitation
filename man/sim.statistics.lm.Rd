% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simitation.R
\name{sim.statistics.lm}
\alias{sim.statistics.lm}
\title{sim.statistic.lm}
\usage{
sim.statistics.lm(simdat, the.formula, grouping.variables)
}
\arguments{
\item{simdat}{Data for use in multivariable regression models across one or more experiments.  Structure is in the form returned by the function simitation::simulation.steps().}

\item{the.formula}{A formula object or character value specifying the formula for the regression model.}

\item{grouping.variables}{A character vector of column names from simdat on which to group the data.  The intended regression model will be fit in groups based on this selection.}
}
\value{
A list containing:
'the.coefs': A data.table of regression coefficients for each group and predictor variable.
'summary.stats': A data.table of summary statistics for each group, typically including metrics such as R-squared, residuals, etc.
}
\description{
sim.statistic.lm
}
\examples{

step.age <- "Age ~ N(45, 10)"
step.female <- "Female ~ binary(0.53)"
step.health.percentile <- "Health.Percentile ~ U(0,100)"
step.exercise.sessions <- "Exercise.Sessions ~ Poisson(2)"
step.diet <- "Diet ~ sample(('Light', 'Moderate', 'Heavy'),
(0.2, 0.45, 0.35))"

step.healthy.lifestyle <- "Healthy.Lifestyle ~
logistic(log(0.45) - 0.1 * (Age -45) + 0.05 * Female +
0.01 * Health.Percentile + 0.5 * Exercise.Sessions - 0.1 *
(Diet == 'Moderate') - 0.4 * (Diet == 'Heavy'))"

step.weight <- "Weight ~ lm(
150 - 15 * Female +
0.5 * Age - 0.1 * Health.Percentile - 0.2 * Exercise.Sessions  +
5 * (Diet == 'Moderate') + 15 * (Diet == 'Heavy') - 2 *
Healthy.Lifestyle + N(0, 10))"

the.steps <- c(step.age, step.female, step.health.percentile,
step.exercise.sessions, step.diet, step.healthy.lifestyle, step.weight)

simdat.multivariate <- simulation.steps(the.steps = the.steps,
n = 50, num.experiments = 2, experiment.name = "sim", seed = 41)

stats.lm <- sim.statistics.lm(simdat = simdat.multivariate,
the.formula = Weight ~ Age + Female + Health.Percentile + Exercise.Sessions +
 Healthy.Lifestyle, grouping.variables = "sim")
}
