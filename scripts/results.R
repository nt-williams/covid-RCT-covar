# Nick Williams
# Research Biostatistician 
# Department of Population Health Sciences 
# Weill Cornell Medicine

.libPaths("/home/niw4001/R_local")

setwd("/home/niw4001/covid-RCT-covar")

box::use(data.table[rbindlist, as.data.table], 
         here[here], 
         glue[glue], 
         config[get], 
         purrr[map_depth, possibly])

pos_adt <- possibly(as.data.table, NULL)

args <- commandArgs(trailingOnly = TRUE)
config <- get(file = here("scripts", "config.yml"), config = args[1])

files <- grep(glue("^{config$type}_{config$prog}_{config$algo}_{config$crossfit}"), 
              list.files("./data/res"), value = TRUE)

res <- lapply(here("data", "res", files), readRDS)
res <- rbindlist(map_depth(map_depth(res, 2, pos_adt), 1, rbindlist))

saveRDS(res, here("data", paste0(args[1], ".rds")))
