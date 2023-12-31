% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simitation.R
\name{buildsim.logistic}
\alias{buildsim.logistic}
\title{Internal function for Simulation for Logistic Regression Data}
\usage{
buildsim.logistic(dat, the.formula, the.variable, n, num.experiments = 1)
}
\arguments{
\item{dat}{A data.frame or data.table containing the variables referenced in the.formula.}

\item{the.formula}{A character string specifying the logistic regression function, e.g., "logistic(0.5 * X + 1.2 * Y)".}

\item{the.variable}{A character string naming the variable in the generated data.}

\item{n}{An integer specifying the number of data points to generate for each experiment.}

\item{num.experiments}{An integer specifying the number of experiments to simulate. Default is 1.}
}
\value{
A data frame with simulated logistic regression values based on the given formula.
}
\description{
This internal function is designed to generate data based on a logistic regression model
specified by the provided formula. It is not intended for direct usage by end-users.
}
