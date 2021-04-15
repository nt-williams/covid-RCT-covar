# Nick Williams
# Research Biostatistician
# Department of Population Health Sciences
# Weill Cornell Medicine

box::use(data.table[...], ./R/results[...])

# truth
truth <- readRDS("./data/truth.rds")

# survival
sunadj <- readRDS("./data/sunadj.rds")
snps <- readRDS("./data/snps.rds")
snprf <- readRDS("./data/snprf.rds")
snprfcf <- readRDS("./data/snprfcf.rds")
snpxg <- readRDS("./data/snpxg.rds")
snpxgcf <- readRDS("./data/snpxgcf.rds")
snpmr <- readRDS("./data/snpmr.rds")
snpmrcf <- readRDS("./data/snpmrcf.rds")

snps[, covar_id := covar_id + 17]
snprf[, covar_id := covar_id + 18]
snprfcf[, covar_id := covar_id + 19]
snpxg[, covar_id := covar_id + 20]
snpxgcf[, covar_id := covar_id + 21]
snpmr[, covar_id := covar_id + 22]
snpmrcf[, covar_id := covar_id + 23]

snp <- rbind(sunadj, snps, snprf, snprfcf, snpxg, 
             snpxgcf, snpmr, snpmrcf, fill = TRUE)[order(n, covar_id)]

label(snp)

make_table(summary(snp[es == 0], "rmst", "rmst.std.error", 0), interp$sp1, wrte)
make_table(summary(snp[es == 2], "rmst", "rmst.std.error", truth$rmst$`2`), interp$sp1, wrte)
make_table(summary(snp[es == 4], "rmst", "rmst.std.error", truth$rmst$`4`), interp$sp1, wrte)
make_table(summary(snp[es == 0], "survprob", "survprob.std.error", 0), interp$sp2, wrte)
make_table(summary(snp[es == 2], "survprob", "survprob.std.error", truth$survprob$`2`), interp$sp2, wrte)
make_table(summary(snp[es == 4], "survprob", "survprob.std.error", truth$survprob$`4`), interp$sp2, wrte)

# ordinal
ounadj <- readRDS("./data/ounadj.rds")
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

summary(onp[es == 0], "log_or", "log_or.std.error", 0)
summary(onp[es == 1.5], "log_or", "log_or.std.error", truth$lor$`1.5`)
summary(onp[es == 3 & log_or < Inf & log_or > -Inf], "log_or", "log_or.std.error", truth$lor$`3`)
summary(onp[es == 0], "mannwhit", "mannwhit.std.error", 0.5, 0.5)
summary(onp[es == 1.5], "mannwhit", "mannwhit.std.error", truth$mw$`1.5`, 0.5)
summary(onp[es == 3], "mannwhit", "mannwhit.std.error", truth$mw$`3`, 0.5)
