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
  for (j in 1:2) {
    make_table(
      summary(sp[es == c(0, 2, 4)[i] & n %in% c(100, 500, 1500)],
              c("rmst", "survprob")[j], 
              c("rmst.std.error", "survprob.std.error")[j], 
              truth[[c("rmst", "survprob")[j]]][[i]]), 
      main
    )
  }
}

# ordinal
ounadj <- readRDS("./data/ounad.rds")
opns <- readRDS("./data/opns.rds")
ops <- readRDS("./data/ops.rds")
oprf <- readRDS("./data/oprf.rds")
oprfcf <- readRDS("./data/oprfcf.rds")
opxg <- readRDS("./data/opxg.rds")
opxgcf <- readRDS("./data/opxgcf.rds")
opmr <- readRDS("./data/opmr.rds")
opmrcf <- readRDS("./data/opmrcf.rds")

opns[, covar_id := covar_id + 11]
ops[, covar_id := covar_id + 17]
oprf[, covar_id := covar_id + 18]
oprfcf[, covar_id := covar_id + 19]
opxg[, covar_id := covar_id + 20]
opxgcf[, covar_id := covar_id + 21]
opmr[, covar_id := covar_id + 22]
opmrcf[, covar_id := covar_id + 23]

op <- rbind(ounadj, opns, ops, oprf, oprfcf, 
            opxg, opxgcf, opmr, opmrcf, fill = TRUE)[order(n, covar_id)]

label(op)

op <- op[covar_id %in% use][log_or < Inf & log_or > -Inf]

for (i in c(1, 3)) {
  for (j in 1:2) {
    make_table(
      summary(op[es == c(0, 1.5, 3)[i] & n %in% c(100, 500, 1500)],
              c("log_or", "mannwhit")[j], 
              c("log_or.std.error", "mannwhit.std.error")[j], 
              truth[[c("lor", "mw")[j]]][[i]], 
              c(0, 0.5)[j]), 
      main
    )
  }
}

# survival, not prognostic
sunadj <- readRDS("./data/sunad.rds")
snpns <- readRDS("./data/snpns.rds")
snps <- readRDS("./data/snps.rds")
snprf <- readRDS("./data/snprf.rds")
snprfcf <- readRDS("./data/snprfcf.rds")
snpxg <- readRDS("./data/snpxg.rds")
snpxgcf <- readRDS("./data/snpxgcf.rds")
snpmr <- readRDS("./data/snpmr.rds")
snpmrcf <- readRDS("./data/snpmrcf.rds")

snpns[, covar_id := covar_id + 11]
snps[, covar_id := covar_id + 17]
snprf[, covar_id := covar_id + 18]
snprfcf[, covar_id := covar_id + 19]
snpxg[, covar_id := covar_id + 20]
snpxgcf[, covar_id := covar_id + 21]
snpmr[, covar_id := covar_id + 22]
snpmrcf[, covar_id := covar_id + 23]

snp <- rbind(sunadj, snpns, snps, snprf, snprfcf, 
             snpxg, snpxgcf, snpmr, snpmrcf, fill = TRUE)[order(n, covar_id)]

label(snp)

snp <- snp[covar_id %in% use]

for (i in c(1, 3)) {
  for (j in 1:2) {
    make_table(
      summary(snp[es == c(0, 2, 4)[i] & n %in% c(100, 500, 1500)],
              c("rmst", "survprob")[j], 
              c("rmst.std.error", "survprob.std.error")[j], 
              truth[[c("rmst", "survprob")[j]]][[i]]), 
      main
    )
  }
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
  for (j in 1:2) {
    make_table(
      summary(onp[es == c(0, 1.5, 3)[i] & n %in% c(100, 500, 1500)],
              c("log_or", "mannwhit")[j], 
              c("log_or.std.error", "mannwhit.std.error")[j], 
              truth[[c("lor", "mw")[j]]][[i]], 
              c(0, 0.5)[j]), 
      main
    )
  }
}

close(main)
close(supp)
