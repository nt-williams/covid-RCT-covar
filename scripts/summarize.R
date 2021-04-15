# Nick Williams
# Research Biostatistician
# Department of Population Health Sciences
# Weill Cornell Medicine

box::use(data.table[...], ./R/results[...])

# truth
truth <- readRDS("./data/truth.rds")

# survival
sunadj <- readRDS("./data/sunadj.rds")
spns <- readRDS("./data/spns.rds")
sps <- readRDS("./data/sps.rds")
sprf <- readRDS("./data/sprf.rds")
sprfcf <- readRDS("./data/sprfcf.rds")
spxg <- readRDS("./data/spxg.rds")
spxgcf <- readRDS("./data/spxgcf.rds")
spmr <- readRDS("./data/spmr.rds")
spmrcf <- readRDS("./data/spmrcf.rds")

spns <- spns[covar_id != 1, ]

sps[, `:=`(covar_id = covar_id + 17)]
sprf[, `:=`(covar_id = covar_id + 18)]
sprfcf[, `:=`(covar_id = covar_id + 19)]
spxg[, `:=`(covar_id = covar_id + 20)]
spxgcf[, `:=`(covar_id = covar_id + 21)]
spmr[, `:=`(covar_id = covar_id + 22)]
spmrcf[, `:=`(covar_id = covar_id + 23)]

sp <- rbind(sunadj, spns, sps, sprf, sprfcf, 
            spxg, spxgcf, spmr, spmrcf, fill = TRUE)[order(n, covar_id)]

label(sp)

main <- c("Unadjusted", "GLM", "LASSO", "Random forest", "(CF) Random forest", "XGBoost", "(CF) XGBoost", "MARS", "(CF) MARS")
sp <- sp[covar_id %in% main]

summary(sp[es == 0], "rmst", "rmst.std.error", 0)
summary(sp[es == 2], "rmst", "rmst.std.error", truth$rmst$`2`)
summary(sp[es == 4], "rmst", "rmst.std.error", truth$rmst$`4`)
summary(sp[es == 0], "survprob", "survprob.std.error", 0)
summary(sp[es == 2], "survprob", "survprob.std.error", truth$survprob$`2`)
summary(sp[es == 4], "survprob", "survprob.std.error", truth$survprob$`4`)

# ordinal
ounadj <- readRDS("./data/ounadj.rds")
opns <- readRDS("./data/opns.rds")
ops <- readRDS("./data/ops.rds")
oprf <- readRDS("./data/oprf.rds")
oprfcf <- readRDS("./data/oprfcf.rds")
opxg <- readRDS("./data/opxg.rds")
opxgcf <- readRDS("./data/opxgcf.rds")
opmr <- readRDS("./data/opmr.rds")
opmrcf <- readRDS("./data/opmrcf.rds")

opns <- opns[covar_id != 1, ]

ops[, `:=`(covar_id = covar_id + 17)]
oprf[, `:=`(covar_id = covar_id + 18)]
oprfcf[, `:=`(covar_id = covar_id + 19)]
opxg[, `:=`(covar_id = covar_id + 20)]
opxgcf[, `:=`(covar_id = covar_id + 21)]
opmr[, `:=`(covar_id = covar_id + 22)]
opmrcf[, `:=`(covar_id = covar_id + 23)]

op <- rbind(ounadj, opns, ops, oprf, oprfcf, 
            opxg, opxgcf, opmr, opmrcf, fill = TRUE)[order(n, covar_id)]

label(op)

op <- op[covar_id %in% main]

summary(op[es == 0], "log_or", "log_or.std.error", 0)
summary(op[es == 1.5 & log_or < Inf], "log_or", "log_or.std.error", truth$lor$`1.5`)
summary(op[es == 3 & log_or < Inf], "log_or", "log_or.std.error", truth$lor$`3`)
summary(op[es == 0], "mannwhit", "mannwhit.std.error", 0.5, 0.5)
summary(op[es == 1.5], "mannwhit", "mannwhit.std.error", truth$mw$`1.5`, 0.5)
summary(op[es == 3], "mannwhit", "mannwhit.std.error", truth$mw$`3`, 0.5)

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

