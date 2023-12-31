% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simitation.R
\name{analyze.simstudy.lm}
\alias{analyze.simstudy.lm}
\title{Analyze Simulated Linear Regression Models}
\usage{
analyze.simstudy.lm(
  the.coefs,
  summary.stats,
  conf.level = 0.95,
  the.quantiles = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975),
  coef.name = "Coefficient",
  estimate.name = "Estimate",
  lm.p.name = "Pr(>|t|)",
  f.p.name = "f.pvalue"
)
}
\arguments{
\item{the.coefs}{A data frame or data.table containing the summary table of estimated
coefficients from repeated linear regression models. It should be structured
like the output
of \code{simitation::sim.statistics.lm$the.coefs()}.}

\item{summary.stats}{A data frame or data.table containing the summary
statistics from
repeated linear regression models, similar to
\code{simitation::sim.statistics.lm$summary.stats()}.}

\item{conf.level}{A numeric value for the confidence level (1 - significance
level). Default is 0.95.}

\item{the.quantiles}{Numeric vector of quantile values for which statistics
are required.}

\item{coef.name}{Column name in 'the.coefs' that has input variable names
of the regression model.}

\item{estimate.name}{Column name in 'the.coefs' for estimated coefficients
of the regression model.}

\item{lm.p.name}{Column name in 'the.coefs' for p-values of coefficient
tests.}

\item{f.p.name}{Column name in 'summary.stats' for the F-test p-value.}
}
\value{
A list with several summary statistics for the linear
regression model.
}
\description{
This function analyzes the results of simulated linear
regression models, providing
various summary statistics about the model coefficients, fit, and other
aspects.
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

step.weight <- "Weight ~ lm(150 - 15 * Female + 0.5 * Age - 0.1 *
Health.Percentile - 0.2 * Exercise.Sessions  + 5 * (Diet == 'Moderate') +
15 * (Diet == 'Heavy') - 2 * Healthy.Lifestyle + N(0, 10))"

the.steps <- c(step.age, step.female, step.health.percentile,
step.exercise.sessions, step.diet, step.healthy.lifestyle, step.weight)

simdat.multivariate <- simulation.steps(the.steps = the.steps,
n = 50, num.experiments = 2, experiment.name = "sim", seed = 41)

stats.lm <- sim.statistics.lm(simdat = simdat.multivariate, the.formula =
Weight ~ Age + Female + Health.Percentile + Exercise.Sessions +
Healthy.Lifestyle, grouping.variables = "sim")


analysis.lm <- analyze.simstudy.lm(the.coefs = stats.lm$the.coefs,
summary.stats = stats.lm$summary.stats, conf.level = 0.95,
the.quantiles = c(0.25, 0.75), coef.name = "Coefficient",
estimate.name = "Estimate", lm.p.name = "Pr(>|t|)", f.p.name = "f.pvalue")
}
