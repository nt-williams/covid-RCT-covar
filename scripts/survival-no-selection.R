# Nick Williams
# Research Biostatistician 
# Department of Population Health Sciences 
# Weill Cornell Medicine

.libPaths("/home/niw4001/R_local")

library(future)

box::use(./R/simulate, here[here])

id <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
machines <- 1000
op <- here("data", "res")

reps <- 1
n <- c(250, 500, 1500)
effect_size <- c(2, 4)

covar <- list(c("none"), c("age"), c("sex"), c("bmi"), c("o2"), c("smoke"), 
              c("num_comorbid"), c("num_symptoms"), c("bilat"), c("age", "o2"), 
              c("age", "sex", "bmi", "o2", "smoke", "num_comorbid", "num_symptoms", "bilat"))
covar_id <- seq_along(covar)

tasks <- expand.grid(type = "survival", covar_id = covar_id, 
                     seed = sample(5346436, reps), n = n, effect_size = effect_size, 
                     stringsAsFactors = FALSE)

plan(multisession, workers = 8)

simulate$partition(tasks, covar, id, machines, op)

quit("no")