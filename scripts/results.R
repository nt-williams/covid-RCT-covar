# Nick Williams
# Research Biostatistician 
# Department of Population Health Sciences 
# Weill Cornell Medicine

.libPaths("/home/niw4001/R_local")

setwd("/home/niw4001/covid-RCT-covar")

box::use(./R/results, 
         dgm = ./R/data, 
         data.table[rbindlist, setDT], 
         here[here], 
         glue[glue], 
         config[get])

args <- commandArgs(trailingOnly = TRUE)
config <- get(file = here("scripts", "config.yml"), config = args[1])

tasks <- expand.grid(covar = seq_along(config$covar), prog = config$prog,
                     lasso = config$lasso, n = config$nobs, es = config$es)
tasks$id <- 1:nrow(tasks)

setDT(tasks)

res <- list()
for (i in 1:nrow(tasks)) {
  files <- grep(glue("^{config$type}_{tasks$covar[i]}_{tasks$n[i]}_{tasks$es[i]}_{tasks$prog[i]}_{tasks$lasso[i]}"), list.files("data/res"), value = TRUE)
  out <- lapply(here("data", "res", files), readRDS)
  res[[i]] <- results$clean(config$type, out)
}

res <- merge(tasks, rbindlist(res, idcol = "id"))

saveRDS(res, here("data", paste0(args[1], ".rds")))
