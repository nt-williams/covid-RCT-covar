# Nick Williams
# Research Biostatistician
# Department of Population Health Sciences
# Weill Cornell Medicine

box::use(dgm = ./R/data)

c19o <- dgm$covid("ordinal")
c19s <- dgm$covid("survival")

ord <- 
  list(
    mw = list(
      "0" = 0, 
      "1.5" = mean(replicate(1000, dgm$truth(c19o, "ordinal", effect_size = 1.5, estimand = "mw"))), 
      "3" = mean(replicate(1000, dgm$truth(c19o, "ordinal", effect_size = 3, estimand = "mw")))
    ), 
    lor = list(
      "0" = 0, 
      "1.5" = mean(replicate(1000, dgm$truth(c19o, "ordinal", effect_size = 1.5))), 
      "3" = mean(replicate(1000, dgm$truth(c19o, "ordinal", effect_size = 3)))
    )
  )

saveRDS(ord, "./data/truth-ordinal.rds")

surv <- 
  list(
    rmst = list(
      "0" = 0, 
      "2" = mean(replicate(1000, dgm$truth(c19s, "survival", effect_size = 2))), 
      "4" = mean(replicate(1000, dgm$truth(c19s, "survival", effect_size = 4)))
    ), 
    survprob = list(
      "0" = 0, 
      "2" = mean(replicate(1000, dgm$truth(c19s, "survival", effect_size = 2, estimand = "sp"))), 
      "3" = mean(replicate(1000, dgm$truth(c19s, "survival", effect_size = 4, estimand = "sp")))
    )
  )

saveRDS(surv, "./data/truth-survival.rds")
  
