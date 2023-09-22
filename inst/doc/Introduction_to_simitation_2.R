## ---- include=FALSE-----------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, tidy = T)

## ----setup--------------------------------------------------------------------
library(data.table)
library(simitation)

## -----------------------------------------------------------------------------
simdat.prop2 <- sim.prop2(nx = 30, ny = 40, px = 0.5, py = 0.55, num.experiments = 2000, experiment.name = "sim", group.name = "treatment", x.value = "group_1", y.value = "group_2", value.name = "correct_answer", seed = 3)

print(simdat.prop2)

## -----------------------------------------------------------------------------
test.statistics.prop2 <- sim.prop2.test(simdat.prop2 = simdat.prop2, p = NULL, alternative = "less", conf.level = 0.95, correct = T, experiment.name = "sim", group.name = "treatment", x.value = "group_1", y.value = "group_2", value.name = "correct_answer")

print(test.statistics.prop2)

## -----------------------------------------------------------------------------
analysis.prop2 <- analyze.simstudy.prop2(test.statistics.prop2 = test.statistics.prop2, alternative = "less", conf.level = 0.95, the.quantiles = c(0.025, 0.1, 0.9, 0.975))
print(analysis.prop2)

## -----------------------------------------------------------------------------
study.prop2 <- simstudy.prop2(nx = 30, ny = 40, px = 0.5, py = 0.55, num.experiments = 2000, p = NULL, alternative = "less", conf.level = 0.95, correct = T, the.quantiles = c(0.025, 0.5, 0.975), experiment.name = "sim", group.name = "treatment", x.value = "treatment_1", y.value = "treatment_2", value.name = "correct_answer", seed = 904)
print(study.prop2)

## -----------------------------------------------------------------------------
simdat.chisq.gf <- sim.chisq.gf(n = 100, values = LETTERS[1:4], prob = c(0.4, 0.3, 0.2, 0.1), num.experiments = 2000, experiment.name = "experiment_id", value.name = "classification", seed = 31)

print(simdat.chisq.gf)

## ----warning=FALSE------------------------------------------------------------
test.statistics.chisq.test.gf <- sim.chisq.test.gf(simdat.chisq.gf = simdat.chisq.gf, hypothesized.probs = c(0.25, 0.3, 0.15, 0.3), correct = F, experiment.name = "experiment_id", value.name = "classification")

print(test.statistics.chisq.test.gf)

## -----------------------------------------------------------------------------
analysis.chisq.gf <- analyze.simstudy.chisq.test.gf(test.statistics.chisq.test.gf = test.statistics.chisq.test.gf, conf.level = 0.95, the.quantiles = c(0.25, 0.75))
print(analysis.chisq.gf)

## ----warning=FALSE------------------------------------------------------------
study.chisq.gf <- simstudy.chisq.test.gf(n = 75, values = LETTERS[1:4], actual.probs = c(0.3, 0.3, 0.2, 0.2), hypothesized.probs = rep.int(x = 0.25, times = 4), num.experiments = 40, conf.level = 0.95, correct = F, the.quantiles = c(0.25, 0.75), experiment.name = "experiment_id", value.name = "classification", seed = 61)

print(study.chisq.gf)

## -----------------------------------------------------------------------------
n <- c(50, 75, 100)
values <- LETTERS[1:4]
group.names <- sprintf("group_%d", 1:3)
probs <- matrix(data = c(0.25, 0.25, 0.25, 0.25, 0.4, 0.3, 0.2, 0.1, 0.2, 0.4, 0.2, 0.2), nrow = length(n), byrow = T)


simdat.chisq.ind <- sim.chisq.ind(n = c(50, 75, 100), values = LETTERS[1:4], probs = probs, num.experiments = 2000, experiment.name = "exp_id", group.name = "treatment_group", group.values = sprintf("group_%d", 1:3), value.name = "category", seed = 31)

print(simdat.chisq.ind)

## ----warning=FALSE------------------------------------------------------------
test.statistics.chisq.test.ind <- sim.chisq.test.ind(simdat.chisq.ind = simdat.chisq.ind, correct = T, experiment.name = "exp_id", group.name = "treatment_group", value.name = "category")

print(test.statistics.chisq.test.ind)

## -----------------------------------------------------------------------------
analysis.chisq.ind <- analyze.simstudy.chisq.test.ind(test.statistics.chisq.test.ind = test.statistics.chisq.test.ind, conf.level = 0.95, the.quantiles = c(0.025, 0.975))

print(analysis.chisq.ind)

## ----warning=FALSE------------------------------------------------------------

study.chisq.ind <- simstudy.chisq.test.ind(n = c(30, 35, 40), values = LETTERS[1:4], probs = probs, num.experiments = 2000, conf.level = 0.95, correct = T, the.quantiles = c(0.025, 0.975), experiment.name = "exp_id", group.name = "treatment_group", group.values = sprintf("group_%d", 1:3), value.name = "category", seed = 77)

print(study.chisq.ind)

## -----------------------------------------------------------------------------
step.age <- "Age ~ N(45, 10)"
step.female <- "Female ~ binary(0.53)"
step.health.percentile <- "Health.Percentile ~ U(0,100)"
step.exercise.sessions <- "Exercise.Sessions ~ Poisson(2)"

step.diet <- "Diet ~ sample(('Light', 'Moderate', 'Heavy'), (0.2, 0.45, 0.35))"

## -----------------------------------------------------------------------------
step.healthy.lifestyle <- "Healthy.Lifestyle ~ logistic(log(0.45) - 0.1 * (Age -45) + 0.05 * Female + 0.01 * Health.Percentile + 0.5 * Exercise.Sessions - 0.1 * (Diet == 'Moderate') - 0.4 * (Diet == 'Heavy'))"

## -----------------------------------------------------------------------------
step.weight <- "Weight ~ lm(150 - 15 * Female + 0.5 * Age - 0.1 * Health.Percentile - 0.2 * Exercise.Sessions  + 5 * (Diet == 'Moderate') + 15 * (Diet == 'Heavy') - 2 * Healthy.Lifestyle + N(0, 10))"

## -----------------------------------------------------------------------------
the.steps <- c(step.age, step.female, step.health.percentile, step.exercise.sessions, step.diet, step.healthy.lifestyle, step.weight)

print(the.steps)

## -----------------------------------------------------------------------------
simdat.multivariate <- simulation.steps(the.steps = the.steps, n = 50, num.experiments = 2000, experiment.name = "sim", seed = 41)

print(simdat.multivariate)

## -----------------------------------------------------------------------------
stats.lm <- sim.statistics.lm(simdat = simdat.multivariate, the.formula = Weight ~ Age + Female + Health.Percentile + Exercise.Sessions + Healthy.Lifestyle, grouping.variables = "sim")

print(stats.lm)

## -----------------------------------------------------------------------------
analysis.lm <- analyze.simstudy.lm(the.coefs = stats.lm$the.coefs, summary.stats = stats.lm$summary.stats, conf.level = 0.95, the.quantiles = c(0.25, 0.75), coef.name = "Coefficient", estimate.name = "Estimate", lm.p.name = "Pr(>|t|)", f.p.name = "f.pvalue")
print(analysis.lm)

## -----------------------------------------------------------------------------
study.lm <- simstudy.lm(the.steps = the.steps, n = 100, num.experiments = 2000, the.formula = Weight ~ Age + Female + Health.Percentile + Exercise.Sessions + Healthy.Lifestyle, conf.level = 0.95, the.quantiles = c(0.25, 0.75), experiment.name = "sim", seed = 11)

print(study.lm)

## -----------------------------------------------------------------------------
stats.logistic <- sim.statistics.logistic(simdat = simdat.multivariate, the.formula = Healthy.Lifestyle ~ Age + Female + Health.Percentile + Exercise.Sessions, grouping.variables = "sim")

print(stats.logistic)

## -----------------------------------------------------------------------------
analysis.logistic <- analyze.simstudy.logistic(the.coefs = stats.logistic$the.coefs, summary.stats = stats.logistic$summary.stats, conf.level = 0.95, the.quantiles = c(0.1, 0.9))

print(analysis.logistic)

## -----------------------------------------------------------------------------
study.logistic <- simstudy.logistic(the.steps = the.steps, n = 100, num.experiments = 2000, the.formula = Healthy.Lifestyle ~ Age + Female + Health.Percentile + Exercise.Sessions, conf.level = 0.95, the.quantiles = c(0.025, 0.1, 0.5, 0.9, 0.975), experiment.name = "sim", seed = 222)

print(study.logistic)

## -----------------------------------------------------------------------------
study.t <- simstudy.t(n = 25, mean = 0.3, sd = 1, num.experiments = 2000, alternative = "greater", mu = 0, conf.level = 0.95, the.quantiles = c(0.025, 0.975), experiment.name = "experiment",value.name = "x", seed = 817)
print(study.t)

## -----------------------------------------------------------------------------
print(study.t$sim.analysis.t$estimate.summary)

## -----------------------------------------------------------------------------
print(study.t$sim.analysis.t$stat.summary)

## -----------------------------------------------------------------------------
study.t2 <- simstudy.t2(nx = 30, ny = 40, meanx = 0, meany = 0.2, sdx = 1, sdy = 1, num.experiments = 2000, alternative = "less", mu = 0, conf.level = 0.9, the.quantiles = c(0.1, 0.5, 0.9), experiment.name = "experiment_id", group.name = "category", x.value = "a", y.value = "b", value.name = "measurement", seed = 41)
print(study.t2)

## -----------------------------------------------------------------------------
print(study.t2$sim.analysis.t2$estimate.summary)

## -----------------------------------------------------------------------------
print(study.t2$sim.analysis.t2$stat.summary)

## -----------------------------------------------------------------------------
study.prop <- simstudy.prop(n = 30, p.actual = 0.42, p.hypothesized = 0.5, num.experiments = 2000, alternative = "less", conf.level = 0.92, correct = T, the.quantiles = c(0.04, 0.5, 0.96), experiment.name = "simulation_id", value.name = "success", seed = 8001)
print(study.prop)

## -----------------------------------------------------------------------------
print(study.prop$sim.analysis.prop$estimate.summary)

## -----------------------------------------------------------------------------
print(study.prop$sim.analysis.prop$stat.summary)

## -----------------------------------------------------------------------------
print(study.prop2$sim.analysis.prop2$estimate.summary)

## -----------------------------------------------------------------------------
print(study.prop2$sim.analysis.prop2$stat.summary)

## -----------------------------------------------------------------------------
print(study.chisq.gf$sim.analysis$stat.summary)

## -----------------------------------------------------------------------------
print(study.chisq.ind$sim.analysis$stat.summary)

## -----------------------------------------------------------------------------
print(study.lm$sim.analysis$lm.estimate.summary)

## -----------------------------------------------------------------------------
print(study.logistic$sim.analysis$logistic.estimate.summary)

## -----------------------------------------------------------------------------
study.noeffect.t <- simstudy.t(n = 15, mean = 0, sd = 2, num.experiments = 2000, seed = 71)

print(study.noeffect.t$sim.analysis.t$p.value.summary)

## -----------------------------------------------------------------------------
study.noeffect.t2 <- simstudy.t2(nx = 20, ny = 25, meanx = 0, meany = 0, sdx = 1, sdy = 1, num.experiments = 2000, alternative = "greater", seed = 129)

print(study.noeffect.t2$sim.analysis.t2$p.value.summary)

## -----------------------------------------------------------------------------
study.noeffect.prop <- simstudy.prop(n = 40, p.actual = 0.25, p.hypothesized = 0.25, num.experiments = 2000, alternative = "less", conf.level = 0.95, seed = 98)

print(study.noeffect.prop$sim.analysis.prop$p.value.summary)

## -----------------------------------------------------------------------------
study.noeffect.prop2 <- simstudy.prop2(nx = 40, ny = 40, px = 0.4, py = 0.4, num.experiments = 2000, alternative = "two.sided", conf.level = 0.95, seed = 71)

print(study.noeffect.prop2$sim.analysis.prop2$p.value.summary)

## -----------------------------------------------------------------------------
study.noeffect.chisq.gf <- simstudy.chisq.test.gf(n = 100, values = LETTERS[1:5], actual.probs = rep.int(x = 0.2, times = 5), hypothesized.probs = rep.int(x = 0.2, times = 5), num.experiments = 2000, conf.level = 0.95, seed = 3)

print(study.noeffect.chisq.gf$sim.analysis$p.value.summary)

## ----warning=FALSE------------------------------------------------------------
study.noeffect.chisq.ind <- simstudy.chisq.test.ind(n = c(50, 50), values = LETTERS[1:5], probs = matrix(data = 0.2, nrow = 2, ncol = 5), num.experiments = 2000, conf.level = 0.95, seed = 8)

print(study.noeffect.chisq.ind$sim.analysis$p.value.summary)

## -----------------------------------------------------------------------------
study.noeffect.lm <- simstudy.lm(the.steps = c("Age ~ N(50,10)", "Weight ~ N(150, 10)", "Health.Score ~ lm(47 + 0.1 * Age + N(0, 5))"), n = 100, num.experiments = 2000, the.formula = Health.Score ~ Age + Weight, conf.level = 0.9, seed = 4)

print(study.noeffect.lm$sim.analysis$lm.p.summary)

## -----------------------------------------------------------------------------
study.noeffect.logistic <- simstudy.logistic(the.steps = c("Age ~ N(50,10)", "Weight ~ N(150, 10)", "Hospital ~ logistic(-2 + 0.05 * Age)"), n = 100, num.experiments = 2000, the.formula = Hospital ~ Age + Weight, conf.level = 0.4, seed = 31)

print(study.noeffect.logistic$sim.analysis$logistic.p.summary)

## -----------------------------------------------------------------------------
study.effect.t <- simstudy.t(n = 50, mean = 0.3, sd = 1, num.experiments = 2000, alternative = "greater", seed = 44)

print(study.effect.t$sim.analysis.t$p.value.summary)

## -----------------------------------------------------------------------------
study.effect.t2 <- simstudy.t2(nx = 100, ny = 100, meanx = 52, meany = 50, sdx = 5, sdy = 5, num.experiments = 2000, seed = 93, alternative = "two.sided")

print(study.effect.t2$sim.analysis.t2$p.value.summary)

## -----------------------------------------------------------------------------
study.effect.prop <- simstudy.prop(n = 300, p.actual = 0.8, p.hypothesized = 0.75, num.experiments = 2000, alternative = "greater", conf.level = 0.95, seed = 81)

print(study.effect.prop$sim.analysis.prop$p.value.summary)

## -----------------------------------------------------------------------------
study.effect.prop2 <- simstudy.prop2(nx = 500, ny = 500, px = 0.5, py = 0.45, num.experiments = 2000, alternative = "greater", conf.level = 0.95, seed = 117)

print(study.effect.prop2$sim.analysis.prop2$p.value.summary)

## -----------------------------------------------------------------------------
study.effect.chisq.gf <- simstudy.chisq.test.gf(n = 100, values = LETTERS[1:5], actual.probs = c(0.3, 0.35, 0.15, 0.1, 0.1), hypothesized.probs = rep.int(x = 0.2, times = 5), num.experiments = 2000, conf.level = 0.95, seed = 83)

print(study.effect.chisq.gf$sim.analysis$p.value.summary)

## ----warning=FALSE------------------------------------------------------------
study.effect.chisq.ind <- simstudy.chisq.test.ind(n = c(300, 300), values = LETTERS[1:5], probs = matrix(data = c(0.25, 0.25, 0.2, 0.2, 0.1, rep.int(x = 0.2, times = 5)), nrow = 2, ncol = 5, byrow = T), num.experiments = 2000, conf.level = 0.95, seed = 8)

print(study.effect.chisq.ind$sim.analysis$p.value.summary)

## -----------------------------------------------------------------------------
study.effect.lm <- simstudy.lm(the.steps = c("Age ~ N(50,10)", "Weight ~ N(150, 10)", "Health.Score ~ lm(40 + 0.1 * Age + 0.05 * Weight + N(0, 5))"), n = 100, num.experiments = 2000, the.formula = Health.Score ~ Age + Weight, conf.level = 0.9, seed = 4)

print(study.effect.lm$sim.analysis$lm.p.summary)

## -----------------------------------------------------------------------------
glm.steps <- c("Age ~ N(50,10)", "Weight ~ N(170, 15)", "Hospital ~ logistic(-2 + 0.1 * (Age-50) + 0.08 * (Weight-170))")

study.effect.logistic <- simstudy.logistic(the.steps = glm.steps, n = 100, num.experiments = 2000, the.formula = Hospital ~ Age + Weight, conf.level = 0.95, seed = 31)

print(study.effect.logistic$sim.analysis$logistic.p.summary)

