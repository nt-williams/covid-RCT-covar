# Nick Williams
# Research Biostatistician 
# Department of Population Health Sciences 
# Weill Cornell Medicine

library(survrct)

box::use(./R/data)

c19 <- data$covid()

sim <- data$generate_data(c19, "survival", 42315, n = 1000, effect_size = 5)

surv <- survrct(Surv(T, D) ~ A + age + sex + o2 + hyper + dyspnea + bilat, 
                target = "A", data = sim, estimator = "tmle")
rmst(surv, 2) # just need to add drop = FALSE to the subsetting function; # when time is left NULL, default to 2 to the max time point.
survprob(surv, 1:14) 

surv <- survrct(Surv(T, D) ~ A + o2, target = "A", data = sim, estimator = "tmle")
rmst(surv, 10)