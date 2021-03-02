# Nick Williams
# Research Biostatistician 
# Department of Population Health Sciences 
# Weill Cornell Medicine

box::use(DT = data.table,
         knitr,
         here[here], 
         ./R/results, 
         dgm = ./R/data, knitr)

c19 <- dgm$covid()

set.seed(435475)

true2 <- mean(replicate(1e4, dgm$truth(c19, "survival", effect_size = 2, horizon = 14)))
true4 <- mean(replicate(1e4, dgm$truth(c19, "survival", effect_size = 4, horizon = 14)))

spns <- readRDS(here("data", "spns.rds"))
sps <- readRDS(here("data", "sps.rds"))

sps[, `:=`(covar = DT$fcase(covar == 1, 13, covar == 2, 14), 
           id = id + 108)]

res <- rbind(spns, sps)

res[, covar := DT$fcase(covar == 1, "Unadjusted", 
                        covar == 2, "Age", 
                        covar == 3, "Sex", 
                        covar == 4, "BMI", 
                        covar == 5, "Supp. O2", 
                        covar == 6, "Smoking status", 
                        covar == 7, "No. comorbid", 
                        covar == 8, "No. symptoms", 
                        covar == 9, "X-ray bilat. infilt.", 
                        covar == 10, "Age and supp. O2", 
                        covar == 11, "New covar.", 
                        covar == 12, "Original covar.", 
                        covar == 13, "LASSO, new covar.", 
                        covar == 14, "LASSO, orig. covar.")]

knitr$kable(results$summary(res[es == 0], 0)[order(n, mse, covar)], 
            digits = 2, format = "latex", booktabs = TRUE)

knitr$kable(results$summary(res[es == 2], true2)[order(n, mse, covar)], 
            digits = 2, format = "latex", booktabs = TRUE)

knitr$kable(results$summary(res[es == 4], true4)[order(n, mse, covar)], 
            digits = 2, format = "latex", booktabs = TRUE)
