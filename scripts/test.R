# Nick Williams
# Research Biostatistician 
# Department of Population Health Sciences 
# Weill Cornell Medicine

library(survrct)

box::use(./R/simulate)

c19 <- data$covid()

sim <- data$generate_data(c19, "survival", 42315, n = 1000, effect_size = 5)

surv <- survrct(Surv(T, D) ~ A + age + sex + o2 + hyper + dyspnea + bilat, 
                target = "A", data = sim, estimator = "tmle")

rmst(surv, 2:14)
survprob(surv, 1:14) 

surv <- survrct(Surv(T, D) ~ A + o2, target = "A", data = sim, estimator = "tmle")
rmst(surv, 10)