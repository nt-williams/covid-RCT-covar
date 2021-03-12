# Nick Williams
# Research Biostatistician
# Department of Population Health Sciences
# Weill Cornell Medicine

box::use(data.table[...], knitr[kable], ./R/results[summary, label])

# truth
truth <- readRDS("./data/truth.rds")

# survival
spns <- readRDS("./data/spns.rds")
sps <- readRDS("./data/sps.rds")
snpns <- readRDS("./data/snpns.rds")
snps <- readRDS("./data/snps.rds")

sps[, `:=`(covar_id = covar_id + 15)]
sp <- rbind(spns, sps)[order(n, covar_id)]

snps[, `:=`(covar_id = covar_id + 15)]
snp <- rbind(snpns, snps)[order(n, covar_id)]

label(sp)
label(snp)

# ordinal
opns <- readRDS("./data/opns.rds")
ops <- readRDS("./data/ops.rds")
onpns <- readRDS("./data/onpns.rds")
onps <- readRDS("./data/onps.rds")

ops[, `:=`(covar_id = covar_id + 15)]
op <- rbind(opns, ops)[order(n, covar_id)]

onps[, `:=`(covar_id = covar_id + 15)]
onp <- rbind(opns, ops)[order(n, covar_id)]

label(op)
label(onp)

# summaries
sp.rmst.0 <- summary(sp[es == 0], "rmst", "rmst.std.error", 0)
sp.rmst.2 <- summary(sp[es == 2], "rmst", "rmst.std.error", truth$rmst$`2`)
sp.rmst.4 <- summary(sp[es == 4], "rmst", "rmst.std.error", truth$rmst$`4`)

sp.survprob.0 <- summary(sp[es == 0], "survprob", "survprob.std.error", 0)
sp.survprob.2 <- summary(sp[es == 2], "survprob", "survprob.std.error", truth$survprob$`2`)
sp.survprob.4 <- summary(sp[es == 4], "survprob", "survprob.std.error", truth$survprob$`4`)

snp.rmst.0 <- summary(snp[es == 0], "rmst", "rmst.std.error", 0)
snp.rmst.2 <- summary(snp[es == 2], "rmst", "rmst.std.error", truth$rmst$`2`)
snp.rmst.4 <- summary(snp[es == 4], "rmst", "rmst.std.error", truth$rmst$`4`)

snp.survprob.0 <- summary(snp[es == 0], "survprob", "survprob.std.error", 0)
snp.survprob.2 <- summary(snp[es == 2], "survprob", "survprob.std.error", truth$survprob$`2`)
snp.survprob.4 <- summary(snp[es == 4], "survprob", "survprob.std.error", truth$survprob$`4`)

op.lor.0 <- summary(op[es == 0], "log_or", "log_or.std.error", 0)
op.lor.1.5 <- summary(op[es == 1.5], "log_or", "log_or.std.error", truth$lor$`1.5`)
op.lor.3 <- summary(op[es == 3 & log_or < 2], "log_or", "log_or.std.error", truth$lor$`3`)

op.mw.0 <- summary(op[es == 0], "mannwhit", "mannwhit.std.error", 0.5)
op.mw.1.5 <- summary(op[es == 1.5], "mannwhit", "mannwhit.std.error", truth$lor$`1.5`)
op.mw.3 <- summary(op[es == 3], "mannwhit", "mannwhit.std.error", truth$mw$`3`)

onp.lor.0 <- summary(onp[es == 0], "log_or", "log_or.std.error", 0)
onp.lor.1.5 <- summary(onp[es == 1.5], "log_or", "log_or.std.error", truth$lor$`1.5`)
onp.lor.3 <- summary(onp[es == 3 & log_or < 2], "log_or", "log_or.std.error", truth$lor$`3`)

onp.mw.0 <- summary(onp[es == 0], "mannwhit", "mannwhit.std.error", 0.5)
onp.mw.1.5 <- summary(onp[es == 1.5], "mannwhit", "mannwhit.std.error", truth$lor$`1.5`)
onp.mw.3 <- summary(onp[es == 3], "mannwhit", "mannwhit.std.error", truth$mw$`3`)
