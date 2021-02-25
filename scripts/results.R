# Nick Williams
# Research Biostatistician 
# Department of Population Health Sciences 
# Weill Cornell Medicine

library(survrct)

km <- lapply(fs::dir_ls(here::here("data", "res"), regex = "survival_1_*"), readRDS)
