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
snpns <- readRDS("./data/snpns.rds")
snps <- readRDS("./data/snps.rds")

sps[, `:=`(covar_id = covar_id + 15)]
sp <- rbind(spns, sps)[order(n, covar_id)]

snps[, `:=`(covar_id = covar_id + 15)]
snp <- rbind(snpns, snps)[order(n, covar_id)]

label(sp)
label(snp)

sp <- sp[!(covar_id %in% c("Set A", "Set B", "LASSO, A", "LASSO, B", "Age & supp. O2"))]
snp <- snp[!(covar_id %in% c("Set A", "Set B", "LASSO, A", "LASSO, B", "Age & supp. O2"))]

# ordinal
opns <- readRDS("./data/opns.rds")
ops <- readRDS("./data/ops.rds")
onpns <- readRDS("./data/onpns.rds")
onps <- readRDS("./data/onps.rds")

ops[, `:=`(covar_id = covar_id + 15)]
op <- rbind(opns, ops)[order(n, covar_id)]

onps[, `:=`(covar_id = covar_id + 15)]
onp <- rbind(onpns, onps)[order(n, covar_id)]

label(op)
label(onp)

op <- op[!(covar_id %in% c("Set A", "Set B", "LASSO, A", "LASSO, B", "Age & supp. O2"))]
onp <- onp[!(covar_id %in% c("Set A", "Set B", "LASSO, A", "LASSO, B", "Age & supp. O2"))]

# summaries
wrte <- file("./papers/test.tex", open = "a")

make_table(summary(sp[es == 0], "rmst", "rmst.std.error", 0), wrte)
make_table(summary(sp[es == 2], "rmst", "rmst.std.error", truth$rmst$`2`), wrte)
make_table(summary(sp[es == 4], "rmst", "rmst.std.error", truth$rmst$`4`), wrte)
make_table(summary(sp[es == 0], "survprob", "survprob.std.error", 0), wrte)
make_table(summary(sp[es == 2], "survprob", "survprob.std.error", truth$survprob$`2`), wrte)
make_table(summary(sp[es == 4], "survprob", "survprob.std.error", truth$survprob$`4`), wrte)
make_table(summary(snp[es == 0], "rmst", "rmst.std.error", 0), wrte)
make_table(summary(snp[es == 2], "rmst", "rmst.std.error", truth$rmst$`2`), wrte)
make_table(summary(snp[es == 4], "rmst", "rmst.std.error", truth$rmst$`4`), wrte)
make_table(summary(snp[es == 0], "survprob", "survprob.std.error", 0), wrte)
make_table(summary(snp[es == 2], "survprob", "survprob.std.error", truth$survprob$`2`), wrte)
make_table(summary(snp[es == 4], "survprob", "survprob.std.error", truth$survprob$`4`), wrte)
make_table(summary(op[es == 0 & log_or > -10 & log_or < 10], "log_or", "log_or.std.error", 0), wrte)
make_table(summary(op[es == 1.5 & log_or > -10 & log_or < 10], "log_or", "log_or.std.error", truth$lor$`1.5`), wrte)
make_table(summary(op[es == 3 & log_or > -10 & log_or < 10], "log_or", "log_or.std.error", truth$lor$`3`), wrte)
make_table(summary(op[es == 0], "mannwhit", "mannwhit.std.error", 0.5, 0.5), wrte)
make_table(summary(op[es == 1.5], "mannwhit", "mannwhit.std.error", truth$mw$`1.5`, 0.5), wrte)
make_table(summary(op[es == 3], "mannwhit", "mannwhit.std.error", truth$mw$`3`, 0.5), wrte)
make_table(summary(onp[es == 0 & log_or > -10 & log_or < 10], "log_or", "log_or.std.error", 0), wrte)
make_table(summary(onp[es == 1.5 & log_or > -10 & log_or < 10], "log_or", "log_or.std.error", truth$lor$`1.5`), wrte)
make_table(summary(onp[es == 3 & log_or > -10 & log_or < 10], "log_or", "log_or.std.error", truth$lor$`3`), wrte)
make_table(summary(onp[es == 0], "mannwhit", "mannwhit.std.error", 0.5, 0.5), wrte)
make_table(summary(onp[es == 1.5], "mannwhit", "mannwhit.std.error", truth$mw$`1.5`, 0.5), wrte)
make_table(summary(onp[es == 3], "mannwhit", "mannwhit.std.error", truth$mw$`3`, 0.5), wrte)
