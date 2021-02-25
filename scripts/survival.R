# Nick Williams
# Research Biostatistician 
# Department of Population Health Sciences 
# Weill Cornell Medicine

.libPaths("/home/niw4001/R_local")

library(future)

box::use(./R/simulate)

id <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
machines <- 1000

reps <- 250
n <- 500
effect_size <- 4
covar <- list(c("none"), c("age"), c("sex"), c("o2"), c("dyspnea"), c("hyper"), c("bilat"))
covar_id <- seq_along(covar)

tasks <- expand.grid(type = "survival", covar_id = covar_id, 
                     seed = sample(5346436, reps), n = n, effect_size = effect_size, 
                     stringsAsFactors = FALSE)

plan(multisession, workers = 8)

simulate$partition(tasks, covar, 1, 1, here::here("data", "res"))
