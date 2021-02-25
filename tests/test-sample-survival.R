box::use(./R/data)

covid <- readRDS(here::here("data", "private", "covid.rds"))

data$generate_data(covid, "survival", 32423, n = 50, effect_size = 4)
