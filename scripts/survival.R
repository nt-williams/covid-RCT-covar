# Nick Williams
# Research Biostatistician 
# Department of Population Health Sciences 
# Weill Cornell Medicine

library(future)

box::use(./R/simulate)

reps <- 250

covar <- list(c("none"), c("age"), c("sex"), c("o2"), c("dyspnea"), c("hyper"), c("bilat"))
covar_id <- seq_along(covar)

tasks <- expand.grid(type = "survival", covar_id = covar_id, 
                     seed = sample(5346436, reps), n = 500, effect_size = 4, 
                     stringsAsFactors = FALSE)

plan(multisession, workers = 8)

simulate$partition(tasks, covar, 1, 1, here::here("data", "res"))
