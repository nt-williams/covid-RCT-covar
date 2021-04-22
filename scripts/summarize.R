# Nick Williams
# Research Biostatistician
# Department of Population Health Sciences
# Weill Cornell Medicine

box::use(data.table[...], ./R/results[...])

# truth
truth <- readRDS("./data/truth.rds")

# survival
sunadj <- readRDS("./data/sunad.rds")
spns <- readRDS("./data/spns.rds")
sps <- readRDS("./data/sps.rds")
sprf <- readRDS("./data/sprf.rds")
sprfcf <- readRDS("./data/sprfcf.rds")
spxg <- readRDS("./data/spxg.rds")
spxgcf <- readRDS("./data/spxgcf.rds")
spmr <- readRDS("./data/spmr.rds")
spmrcf <- readRDS("./data/spmrcf.rds")

spns[, covar_id := covar_id + 1]
sps[, `:=`(covar_id = covar_id + 17)]
sprf[, `:=`(covar_id = covar_id + 18)]
sprfcf[, `:=`(covar_id = covar_id + 19)]
spxg[, `:=`(covar_id = covar_id + 20)]
spxgcf[, `:=`(covar_id = covar_id + 21)]
spmr[, `:=`(covar_id = covar_id + 22)]
spmrcf[, `:=`(covar_id = covar_id + 23)]

sp <- rbind(sunadj, spns, 
            sps, sprf, sprfcf, 
            spxg, spxgcf, spmr, spmrcf, fill = TRUE)[order(n, covar_id)]

label(sp)

use <- c("Unadjusted", "GLM", "LASSO", "Random forest", 
         "(CF) Random forest", "XGBoost", "(CF) XGBoost", "MARS", "(CF) MARS")
sp <- sp[covar_id %in% use]

main <- file("./papers/tables.tex", open = "a")
supp <- file("./papers/supplementary.tex", open = "a")

for (i in c(1, 3)) {
  s <- c(0, 2, 4)[i]
  make_table(summary(sp[es == s & n %in% c(100, 500, 1500)],
                     "rmst", "rmst.std.error", truth$rmst[[i]]),
             main)
  make_table(summary(sp[es == s & n %in% c(100, 500, 1500)], 
                     "survprob", "survprob.std.error", truth$survprob[[i]]), 
             main)
}

# ordinal, not prognostic
ounadj <- readRDS("./data/ounad.rds")
onpns <- readRDS("./data/onpns.rds")
onps <- readRDS("./data/onps.rds")
onprf <- readRDS("./data/onprf.rds")
onprfcf <- readRDS("./data/onprfcf.rds")
onpxg <- readRDS("./data/onpxg.rds")
onpxgcf <- readRDS("./data/onpxgcf.rds")
onpmr <- readRDS("./data/onpmr.rds")
onpmrcf <- readRDS("./data/onpmrcf.rds")

onpns[, covar_id := covar_id + 11]
onps[, covar_id := covar_id + 17]
onprf[, covar_id := covar_id + 18]
onprfcf[, covar_id := covar_id + 19]
onpxg[, covar_id := covar_id + 20]
onpxgcf[, covar_id := covar_id + 21]
onpmr[, covar_id := covar_id + 22]
onpmrcf[, covar_id := covar_id + 23]

onp <- rbind(ounadj, onpns, onps, onprf, onprfcf, 
             onpxg, onpxgcf, onpmr, onpmrcf, fill = TRUE)[order(n, covar_id)]

label(onp)

onp <- onp[covar_id %in% use][log_or < Inf & log_or > -Inf]

for (i in c(1, 3)) {
  s <- c(0, 1.5, 3)[i]
  make_table(summary(onp[es == s & n %in% c(100, 500, 1500)],
                     "log_or", "log_or.std.error", truth$lor[[i]]),
             main)
  make_table(summary(onp[es == s & n %in% c(100, 500, 1500)], 
                     "mannwhit", "mannwhit.std.error", truth$mw[[i]]), 
             main)
}

close(main)
close(supp)
