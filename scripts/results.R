# Nick Williams
# Research Biostatistician 
# Department of Population Health Sciences 
# Weill Cornell Medicine

box::use(./R/results, 
         dgm = ./R/data, 
         data.table[rbindlist, setDT], 
         fs[dir_ls], 
         here[here], 
         glue[glue])

covar <- list(c("none"), c("age"), c("sex"), c("bmi"), c("o2"), c("smoke"), 
              c("num_comorbid"), c("num_symptoms"), c("bilat"), c("age", "o2"), 
              c("age", "sex", "bmi", "o2", "smoke", "num_comorbid", "num_symptoms", "bilat"))
n <- c(250, 500, 1500)
effect_size <- c(2, 4)

tasks <- expand.grid(covar = seq_along(covar), n = n, es = effect_size)
tasks$id <- 1:nrow(tasks)

setDT(tasks)

true <- dgm$truth(dgm$covid(), "survival", effect_size = 4, horizon = 14)

res <- list()
for (i in 1:nrow(tasks)) {
  out <- lapply(dir_ls(here("data", "res"), regex = glue("survival_{tasks$covar[i]}_{tasks$n[i]}_{tasks$es[i]}_*")), readRDS)
  res[[i]] <- results$clean(out)
}

res <- merge(tasks, rbindlist(res, idcol = "id"))

results$summary(res, true)[]
