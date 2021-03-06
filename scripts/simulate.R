# Nick Williams
# Research Biostatistician 
# Department of Population Health Sciences 
# Weill Cornell Medicine

# .libPaths("/home/niw4001/R_local")

box::use(./R/simulate, config[get], future[...])

args <- commandArgs(trailingOnly = TRUE)
config <- get(file = "./scripts/config.yml", config = args[1])

id <- 1
# id <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
op <- "./data/res"
mch <- 1
# mch <- config$machines

tasks <- expand.grid(type = config$type, covar_id = seq_along(config$covar),
                     lasso = config$lasso, prog = config$prog, 
                     seed = sample(5346436, config$reps), 
                     n = config$nobs, effect_size = config$es, stringsAsFactors = FALSE)

plan(multisession, workers = 8)

simulate$partition(tasks, config$covar, id, mch, op)

quit("no")