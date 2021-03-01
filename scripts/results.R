# Nick Williams
# Research Biostatistician 
# Department of Population Health Sciences 
# Weill Cornell Medicine

box::use(./R/results, 
         dgm = ./R/data, 
         data.table[rbindlist, setDT], 
         fs[dir_ls], 
         here[here], 
         glue[glue], 
         config[get])

config <- get(file = here("scripts", "config.yml"), config = "sps")

tasks <- expand.grid(covar = seq_along(config$covar), prog = config$prog,
                     lasso = config$lasso, n = config$nobs, es = config$es)
tasks$id <- 1:nrow(tasks)

setDT(tasks)

true2 <- dgm$truth(dgm$covid(), "survival", effect_size = 2, horizon = 14)
true4 <- dgm$truth(dgm$covid(), "survival", effect_size = 4, horizon = 14)

res <- list()
for (i in 1:nrow(tasks)) {
  out <- lapply(dir_ls(here("data", "res"), 
                       regex = glue("survival_{tasks$covar[i]}_{tasks$n[i]}_{tasks$es[i]}_{tasks$prog[i]}_{tasks$lasso[i]}*")), 
                readRDS)
  res[[i]] <- results$clean_surv(out, tasks$lasso[i])
}

res <- merge(tasks, rbindlist(res, idcol = "id"))

results$summary(res[es == 4], true4)[order(n, mse, covar)]
results$summary(res[es == 2], true2)[order(n, mse, covar)]
