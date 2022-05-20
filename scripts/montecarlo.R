# Nick Williams
# Research Biostatistician
# Department of Population Health Sciences
# Weill Cornell Medicine

library(data.table)
library(tidyverse)
library(rsimsum)

source("R/results.R")

truth <- readRDS("./data/truth.rds")
names(truth) <- c("mannwhit", "log_or", "rmst", "survprob")

print_for_paper <- function(data) {
  data <- mutate(data, across(c("bias", "var", "mse", "power", "rel_eff"), 
                              \(x) if_else(x < 0.01, "<0.01", format(x, digits = 1, nsmall = 2))))
  glue::glue("& & & {data$power} & {data$mse} & {data$var} & {data$bias} & {data$rel_eff} \\\\ \\hline \\addlinespace ")
}

max_mce <- function(data, est) {
  group_by(data, n, es) |> 
    nest() |> 
    mutate(truth = truth[[est]][[as.character(es)]], 
           me = map(data, \(x) mcse(x, est, paste0(est, ".std.error"), truth, "covar_id", "Unadjusted"))) |> 
    unnest(me) |> 
    select(-data) |> 
    ungroup() |> 
    arrange(es, n) |> 
    mutate(across(c("mse", "var"), \(x) x * n)) |> 
    filter(n %in% c(100, 500, 1500), es %in% c(0, 3, 4)) |>
    group_by(n, es) |>
    nest() |>
    (\(data) map(data$data, print_for_paper))()
}

# prognostic --------------------------------------------------------------

# |- survival
sunadj <- readRDS("./data/sunad.rds")
spns <- readRDS("./data/spns.rds")
sps <- readRDS("./data/sps.rds")
sprf <- readRDS("./data/sprf.rds")
sprfcf <- readRDS("./data/sprfcf.rds")
spxg <- readRDS("./data/spxg.rds")
spxgcf <- readRDS("./data/spxgcf.rds")
spmr <- readRDS("./data/spmr.rds")
spmrcf <- readRDS("./data/spmrcf.rds")

spns <- spns[covar_id == 11, ]

spns[, covar_id := covar_id + 1]
sps[, `:=`(covar_id = covar_id + 17)]
sprf[, `:=`(covar_id = covar_id + 18)]
sprfcf[, `:=`(covar_id = covar_id + 19)]
spxg[, `:=`(covar_id = covar_id + 20)]
spxgcf[, `:=`(covar_id = covar_id + 21)]
spmr[, `:=`(covar_id = covar_id + 22)]
spmrcf[, `:=`(covar_id = covar_id + 23)]

sp <- rbind(sunadj, spns, sps, sprf, sprfcf, spxg, spxgcf, spmr, spmrcf, fill = TRUE)[order(n, covar_id)]
label(sp)

use <- c("Unadjusted", "LR", "LASSO", "RF", "CF-RF", "XGBoost", "CF-XGBoost", "MARS", "CF-MARS")
sp <- sp[covar_id %in% use]

max_mce(sp, "rmst")
max_mce(sp, "survprob")

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

opns <- opns[covar_id == 11, ]

opns[, covar_id := covar_id + 1]
ops[, covar_id := covar_id + 17]
oprf[, covar_id := covar_id + 18]
oprfcf[, covar_id := covar_id + 19]
opxg[, covar_id := covar_id + 20]
opxgcf[, covar_id := covar_id + 21]
opmr[, covar_id := covar_id + 22]
opmrcf[, covar_id := covar_id + 23]

op <- rbind(ounadj, opns, ops, oprf, oprfcf, opxg, opxgcf, opmr, opmrcf, fill = TRUE)[order(n, covar_id)]
label(op)

op <- op[covar_id %in% use][log_or < Inf & log_or > -Inf]

max_mce(op, "log_or")
max_mce(op, "mannwhit")

# n/prognostic ------------------------------------------------------------

# survival
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

snp <- rbind(sunadj, snpns, snps, snprf, snprfcf, snpxg, snpxgcf, snpmr, snpmrcf, fill = TRUE)[order(n, covar_id)]
label(snp)

snp <- snp[covar_id %in% use]

max_mce(snp, "rmst")

# ordinal
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

onp <- rbind(ounadj, onpns, onps, onprf, onprfcf, onpxg, onpxgcf, onpmr, onpmrcf, fill = TRUE)[order(n, covar_id)]
label(onp)

onp <- onp[covar_id %in% use][log_or < Inf & log_or > -Inf]