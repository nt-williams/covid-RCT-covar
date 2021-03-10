# Nick Williams
# Research Biostatistician 
# Department of Population Health Sciences 
# Weill Cornell Medicine

box::use(./R/simulate, ./R/results, dgm = ./R/data, config[get])

c19 <- dgm$covid("ordinal")
tmp <- dgm$generate_data(c19, "ordinal", TRUE, 43253, n = 500, effect_size = 3)

x <- lapply(1:50, function(x) {
  simulate$simulate(c19, "ordinal", "none", TRUE, sample(44353, 1), 
                    n = 500, effect_size = 3, lasso = FALSE)
})

results$clean("ordinal", x)

c19 <- dgm$covid("survival")
tmp <- dgm$generate_data(c19, "survival", TRUE, 43253, n = 500, effect_size = 3)

x <- lapply(1:50, function(x) {
  simulate$simulate(c19, "survival", c("age", "o2", "bmi"), TRUE, sample(44353, 1), 
                    n = 500, effect_size = 3, lasso = FALSE)
})

results$clean("survival", x)
