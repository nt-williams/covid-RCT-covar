# Nick Williams
# Research Biostatistician
# Department of Population Health Sciences
# Weill Cornell Medicine

box::use(data.table[...], ./R/results[...])

# truth
truth <- readRDS("./data/truth.rds")

snpns <- readRDS("./data/snpns.rds")
snps <- readRDS("./data/snps.rds")
snprfncf <- readRDS("./data/snprf_ncf.rds")

snps[, `:=`(covar_id = covar_id + 15)]
snprfncf[, `:=`(covar_id = covar_id + 18)]
snp <- rbind(snpns, snps, sprfncf, fill = TRUE)[order(n, covar_id)]

label(snp)

snp <- snp[!(covar_id %in% c("Set A", "Set B", "LASSO, A", "LASSO, B", "Age & supp. O2"))]

onpns <- readRDS("./data/onpns.rds")
onps <- readRDS("./data/onps.rds")
onprfncf <- readRDS("./data/onprf_ncf.rds")

onps[, `:=`(covar_id = covar_id + 15)]
onprfncf[, `:=`(covar_id = covar_id + 18)]
onp <- rbind(onpns, onps, onprfncf, fill = TRUE)[order(n, covar_id)]

label(onp)

onp <- onp[!(covar_id %in% c("Set A", "Set B", "LASSO, A", "LASSO, B", "Age & supp. O2"))]