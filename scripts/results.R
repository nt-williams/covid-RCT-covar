# Nick Williams
# Research Biostatistician 
# Department of Population Health Sciences 
# Weill Cornell Medicine

box::use(./R/results, 
         dgm = ./R/data, 
         data.table[rbindlist], 
         fs[dir_ls], 
         here[here], 
         glue[glue])

covar <- list(c("none"), c("age"), c("sex"), c("o2"), c("dyspnea"), c("hyper"), c("bilat"))

true <- dgm$truth(dgm$covid(), "survival", effect_size = 4, horizon = 14)

res <- list()
for (i in seq_along(covar)) {
  out <- lapply(dir_ls(here("data", "res"), regex = glue("survival_{i}_*")), readRDS)
  res[[covar[[i]]]] <- results$clean(out)
}

res <- rbindlist(res, idcol = "adj")

results$summary(res, true)[order(mse)]
