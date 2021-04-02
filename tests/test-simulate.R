# Nick Williams
# Research Biostatistician 
# Department of Population Health Sciences 
# Weill Cornell Medicine

box::use(./R/simulate, ./R/results, dgm = ./R/data, config[get], adjrct[...])

c19 <- dgm$covid("ordinal")
tmp <- dgm$generate_data(c19, "ordinal", FALSE, 24122, n = 1500, effect_size = 3)

meta <- ordinalrct(state_ordinal ~ A +age+sex+bmi+o2+smoke+num_comorbid+num_symptoms+bilat+dyspnea+hyper, 
                   target = "A", estimator = "tmle", data = tmp, algo = "xgboost", crossfit = FALSE)
mannwhitney(meta)

unadj <- ordinalrct(state_ordinal ~ A, target = "A", estimator = "tmle", data = tmp)
log_or(unadj)

x <- lapply(1:10, function(x) {
  simulate$simulate(c19, "ordinal", c("age", "sex", "bmi", "o2", "smoke", "num_comorbid", "num_symptoms", "bilat"),
                    TRUE, sample(44353, 1), n = 1500, effect_size = 3, algo = "earth", crossfit = TRUE)
})

# survival ----------------------------------------------------------------

c19 <- dgm$covid("survival")
tmp <- dgm$generate_data(c19, "survival", TRUE, 5436, n = 1000, effect_size = 4)

meta <- survrct(Surv(days, event) ~ A + age + sex + bmi + smoke + bilat + o2, algo = "xgboost",
                target = "A", estimator = "tmle", data = tmp, crossfit = TRUE)
survprob(meta, 7)

x <- lapply(1:10, function(x) {
  simulate$simulate(c19, "survival", c("age", "sex", "bmi", "o2", "smoke", "num_comorbid", "num_symptoms", "bilat"),
                    TRUE, sample(44353, 1), n = 1500, effect_size = 3, algo = "earth", crossfit = TRUE)
})

# config ------------------------------------------------------------------

box::use(./R/simulate, config[get])

config <- get(file = "./scripts/config.yml", config = "spns")

id <- 1
op <- "./data/res"
mch <- config$machines

tasks <- expand.grid(type = config$type, covar_id = seq_along(config$covar),
                     lasso = config$lasso, prog = config$prog, 
                     seed = sample(5346436, config$reps), 
                     n = config$nobs, effect_size = config$es, stringsAsFactors = FALSE)

simulate$partition(tasks, config$covar, id, mch, op)
