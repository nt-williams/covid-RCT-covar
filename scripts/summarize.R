# Nick Williams
# Research Biostatistician 
# Department of Population Health Sciences 
# Weill Cornell Medicine

box::use(DT = data.table, here[here], ./R/results, dgm = ./R/data, knitr, KE = kableExtra)

true2 <- dgm$truth(dgm$covid(), "survival", effect_size = 2, horizon = 14)
true4 <- dgm$truth(dgm$covid(), "survival", effect_size = 4, horizon = 14)

snsp <- readRDS(here("data", "survival_ns_prog.rds"))

snsp[abs(theta) < 2]

knitr$kable(results$summary(snsp[es == 4], true4)[order(n, mse, covar)], 
            digits = 2, format = "latex")

knitr$kable(results$summary(snsp[es == 2], true2)[order(n, mse, covar)], 
            digits = 2, format = "latex")
