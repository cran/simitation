## ---- include=FALSE-----------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, tidy = T)

## ---- include=FALSE-----------------------------------------------------------
devtools::load_all(".")

## ----setup--------------------------------------------------------------------
library(data.table)
library(simitation)

## -----------------------------------------------------------------------------
simdat.t <- sim.t(n = 25, mean = 0.3, sd = 1,  num.experiments = 2000, experiment.name = "experiment", value.name = "x", seed = 2187)
print(simdat.t)

## -----------------------------------------------------------------------------
#value.name = "x"
test.statistics.t <- sim.t.test(simdat.t = simdat.t, alternative = "greater", mu = 0, conf.level = 0.95, experiment.name = "experiment", value.name = "x")
print(test.statistics.t)

## -----------------------------------------------------------------------------
analysis.t <- analyze.simstudy.t(test.statistics.t = test.statistics.t, conf.level = 0.95, alternative = "greater", the.quantiles = c(0.025, 0.25, 0.25, 0.5, 0.75, 0.975))

print(analysis.t)

## -----------------------------------------------------------------------------
study.t <- simstudy.t(n = 25, mean = 0.3, sd = 1, num.experiments = 2000, alternative = "greater", mu = 0, conf.level = 0.95, the.quantiles = c(0.025, 0.975), experiment.name = "experiment",value.name = "x", seed = 817)
print(study.t)

## -----------------------------------------------------------------------------
simdat.t2 <- sim.t2(nx = 30, ny = 40, meanx = 0, meany = 0.2, sdx = 1, sdy = 1, num.experiments = 2000, experiment.name = "experiment", group.name = "group", x.value = "x", y.value = "y", value.name = "value", seed = 17)
print(simdat.t2)

## -----------------------------------------------------------------------------
test.statistics.t2 <- sim.t2.test(simdat.t2 = simdat.t2, alternative = "less", mu = 0, conf.level = 0.9, experiment.name = "experiment", group.name = "group", x.value = "x", y.value = "y", value.name = "value")

print(test.statistics.t2)

## -----------------------------------------------------------------------------
analysis.t2 <- analyze.simstudy.t2(test.statistics.t2 = test.statistics.t2, alternative = "less", conf.level = 0.9, the.quantiles = c(0.25, 0.5, 0.75))
print(analysis.t2)

## -----------------------------------------------------------------------------
study.t2 <- simstudy.t2(nx = 30, ny = 40, meanx = 0, meany = 0.2, sdx = 1, sdy = 1, num.experiments = 2000, alternative = "less", mu = 0, conf.level = 0.9, the.quantiles = c(0.1, 0.5, 0.9), experiment.name = "experiment_id", group.name = "category", x.value = "a", y.value = "b", value.name = "measurement", seed = 41)
print(study.t2)

## -----------------------------------------------------------------------------
simdat.prop <- sim.prop(n = 30, p = 0.45, num.experiments = 2000, experiment.name = "simulation_id", value.name = "success", seed = 104)
print(simdat.prop)

## -----------------------------------------------------------------------------
test.statistics.prop <- sim.prop.test(simdat.prop = simdat.prop, p = 0.5, alternative = "two.sided", conf.level = 0.99, correct = T, experiment.name = "simulation_id", value.name = "success")
print(test.statistics.prop)

## -----------------------------------------------------------------------------
analysis.prop <- analyze.simstudy.prop(test.statistics.prop = test.statistics.prop, alternative = "two.sided", conf.level = 0.99, the.quantiles = c(0.005, 0.995))
print(analysis.prop)

## -----------------------------------------------------------------------------
study.prop <- simstudy.prop(n = 30, p.actual = 0.42, p.hypothesized = 0.5, num.experiments = 2000, alternative = "less", conf.level = 0.92, correct = T, the.quantiles = c(0.04, 0.5, 0.96), experiment.name = "simulation_id", value.name = "success", seed = 8001)
print(study.prop)

