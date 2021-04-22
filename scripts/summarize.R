# Nick Williams
# Research Biostatistician
# Department of Population Health Sciences
# Weill Cornell Medicine

box::use(data.table[...], ./R/results[...])

# truth
truth <- readRDS("./data/truth.rds")

# survival
spns <- readRDS("./data/spns.rds")
sps <- readRDS("./data/sps.rds")
sprf <- readRDS("./data/sprf.rds")
sprfncf <- readRDS("./data/sprf_ncf.rds")
spmrcf <- readRDS("./data/spmrcf.rds")
snpns <- readRDS("./data/snpns.rds")
snps <- readRDS("./data/snps.rds")
snprfncf <- readRDS("./data/snprf_ncf.rds")

sps[, `:=`(covar_id = covar_id + 15)]
sprfncf[, `:=`(covar_id = covar_id + 18)]
sprf[, `:=`(covar_id = covar_id + 19)]
sp <- rbind(spns, sps, sprfncf, sprf, fill = TRUE)[order(n, covar_id)]

snps[, `:=`(covar_id = covar_id + 15)]
snprfncf[, `:=`(covar_id = covar_id + 18)]
snp <- rbind(snpns, snps, sprfncf, fill = TRUE)[order(n, covar_id)]

label(sp)
label(snp)

sp <- sp[!(covar_id %in% c("Set A", "Set B", "LASSO, A", "LASSO, B", "Age & supp. O2"))]
snp <- snp[!(covar_id %in% c("Set A", "Set B", "LASSO, A", "LASSO, B", "Age & supp. O2"))]

# ordinal
opns <- readRDS("./data/opns.rds")
ops <- readRDS("./data/ops.rds")
oprf <- readRDS("./data/oprf.rds")
oprfncf <- readRDS("./data/oprf_ncf.rds")
onpns <- readRDS("./data/onpns.rds")
onps <- readRDS("./data/onps.rds")
onprfncf <- readRDS("./data/onprf_ncf.rds")

ops[, `:=`(covar_id = covar_id + 15)]
oprfncf[, `:=`(covar_id = covar_id + 18)]
oprf[, `:=`(covar_id = covar_id + 19)]
op <- rbind(opns, ops, oprfncf, oprf, fill = TRUE)[order(n, covar_id)]

onps[, `:=`(covar_id = covar_id + 15)]
onprfncf[, `:=`(covar_id = covar_id + 18)]
onp <- rbind(onpns, onps, onprfncf, fill = TRUE)[order(n, covar_id)]

label(op)
label(onp)

op <- op[!(covar_id %in% c("Set A", "Set B", "LASSO, A", "LASSO, B", "Age & supp. O2"))]
onp <- onp[!(covar_id %in% c("Set A", "Set B", "LASSO, A", "LASSO, B", "Age & supp. O2"))]

# summaries
wrte <- file("./papers/tables.tex", open = "a")

interp <- list(sp1 = "Results for the restricted mean survival time in a hospitalized population.", 
               sp2 = "Results for the survival probability in a hospitalized population.", 
               snp1 = "Results for the restricted mean survival time in a hospitalized population when covariates are not predictive of the outcome.", 
               snp2 = "Results for the survival probability in a hospitalized population when covariates are not predictive of the outcome.", 
               op1 = "Results for the log odds ratio in a hospitalized population.", 
               op2 = "Results for the Mann-Whitney statistic in a hospitalized population.", 
               onp1 = "Results for the log odds ratio in a hospitalized population when covariates are not predictive of the outcome.", 
               onp2 = "Results for the Mann-Whitney statistic in a hospitalized population when covariates are not predictive of the outcome.")

make_table(summary(sp[es == 0], "rmst", "rmst.std.error", 0), interp$sp1, wrte)
make_table(summary(sp[es == 2], "rmst", "rmst.std.error", truth$rmst$`2`), interp$sp1, wrte)
make_table(summary(sp[es == 4], "rmst", "rmst.std.error", truth$rmst$`4`), interp$sp1, wrte)
make_table(summary(sp[es == 0], "survprob", "survprob.std.error", 0), interp$sp2, wrte)
make_table(summary(sp[es == 2], "survprob", "survprob.std.error", truth$survprob$`2`), interp$sp2, wrte)
make_table(summary(sp[es == 4], "survprob", "survprob.std.error", truth$survprob$`4`), interp$sp2, wrte)
make_table(summary(snp[es == 0], "rmst", "rmst.std.error", 0), interp$snp1, wrte)
make_table(summary(snp[es == 2], "rmst", "rmst.std.error", truth$rmst$`2`), interp$snp1, wrte)
make_table(summary(snp[es == 4], "rmst", "rmst.std.error", truth$rmst$`4`), interp$snp1, wrte)
make_table(summary(snp[es == 0], "survprob", "survprob.std.error", 0), interp$snp2, wrte)
make_table(summary(snp[es == 2], "survprob", "survprob.std.error", truth$survprob$`2`), interp$snp2, wrte)
make_table(summary(snp[es == 4], "survprob", "survprob.std.error", truth$survprob$`4`), interp$snp2, wrte)
make_table(summary(op[es == 0 & log_or > -10 & log_or < 10], "log_or", "log_or.std.error", 0), interp$op1, wrte)
make_table(summary(op[es == 1.5 & log_or > -10 & log_or < 10], "log_or", "log_or.std.error", truth$lor$`1.5`),interp$op1, wrte)
make_table(summary(op[es == 3 & log_or > -10 & log_or < 10], "log_or", "log_or.std.error", truth$lor$`3`), interp$op1, wrte)
make_table(summary(op[es == 0], "mannwhit", "mannwhit.std.error", 0.5, 0.5), interp$op2, wrte)
make_table(summary(op[es == 1.5], "mannwhit", "mannwhit.std.error", truth$mw$`1.5`, 0.5), interp$op2, wrte)
make_table(summary(op[es == 3], "mannwhit", "mannwhit.std.error", truth$mw$`3`, 0.5), interp$op2, wrte)
make_table(summary(onp[es == 0 & log_or > -10 & log_or < 10], "log_or", "log_or.std.error", 0), interp$onp1, wrte)
make_table(summary(onp[es == 1.5 & log_or > -10 & log_or < 10], "log_or", "log_or.std.error", truth$lor$`1.5`), interp$onp1, wrte)
make_table(summary(onp[es == 3 & log_or > -10 & log_or < 10], "log_or", "log_or.std.error", truth$lor$`3`), interp$onp1, wrte)
make_table(summary(onp[es == 0], "mannwhit", "mannwhit.std.error", 0.5, 0.5), interp$onp2, wrte)
make_table(summary(onp[es == 1.5], "mannwhit", "mannwhit.std.error", truth$mw$`1.5`, 0.5),interp$onp2, wrte)
make_table(summary(onp[es == 3], "mannwhit", "mannwhit.std.error", truth$mw$`3`, 0.5), interp$onp2, wrte)

