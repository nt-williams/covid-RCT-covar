# Nick Williams
# Research Biostatistician 
# Department of Population Health Sciences 
# Weill Cornell Medicine

library(survrct)

box::use(dgm = ./R/data)

c19 <- dgm$generate_data(dgm$covid(), "survival", 25435, n = 500, effect_size = 4)

fit <- survrct(Surv(days, event) ~ A + o2, target = "A", 
               data = c19, coarsen = 1, estimator = "tmle")

rmst(fit, 14)
survprob(fit, 14)

rmst(fit, 2:14)
survprob(fit, 1:14)