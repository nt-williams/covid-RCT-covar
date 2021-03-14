# Nick Williams
# Research Biostatistician 
# Department of Population Health Sciences 
# Weill Cornell Medicine

box::use(./R/simulate, ./R/results, dgm = ./R/data, config[get])

c19 <- dgm$covid("ordinal")
tmp <- dgm$generate_data(c19, "ordinal", FALSE, 24122, n = 100, effect_size = 3)

library(adjrct)

meta <- ordinalrct(state_ordinal ~ A, target = "A", estimator = "tmle", data = tmp)
log_or(meta)
pmf(meta)

meta <- ordinalrct(state_ordinal ~ A + age + o2 + dyspnea + hyper + smoke, target = "A", estimator = "tmle", data = tmp, lasso = TRUE)
log_or(meta)
pmf(meta)

test <- function() {
  seed <- sample(44353, 1)
  print(simulate$simulate(c19, "ordinal", "none",
                    TRUE, seed, 
                    n = 100, effect_size = 3, lasso = TRUE))
  cat(seed)
}




simulate$simulate(c19, "ordinal", "none",
                  TRUE, sample(44353, 1), 
                  n = 100, effect_size = 3, lasso = TRUE)

x <- lapply(1:10, function(x) {
  simulate$simulate(c19, "ordinal", c("age", "sex", "bmi", "o2", "smoke", "num_comorbid", "num_symptoms", "bilat"),
                    TRUE, sample(44353, 1), 
                    n = 100, effect_size = 3, lasso = TRUE)
})


c19 <- dgm$covid("survival")
tmp <- dgm$generate_data(c19, "survival", TRUE, 5436, n = 100, effect_size = 3)

meta <- adjrct::survrct(Surv(days, event) ~ A, target = "A", estimator = "tmle", data = tmp)
adjrct::survprob(meta, 7)

x <- lapply(1:50, function(x) {
  simulate$simulate(c19, "survival", "none", TRUE, sample(44353, 1), 
                    n = 100, effect_size = 3, lasso = FALSE)
})

results$clean("survival", x)

box::use(./R/simulate, config[get], future[...])

config <- get(file = "./scripts/config.yml", config = "spns")

id <- 1
op <- "./data/res"
mch <- config$machines

tasks <- expand.grid(type = config$type, covar_id = seq_along(config$covar),
                     lasso = config$lasso, prog = config$prog, 
                     seed = sample(5346436, config$reps), 
                     n = config$nobs, effect_size = config$es, stringsAsFactors = FALSE)

simulate$partition(tasks, config$covar, id, mch, op)
