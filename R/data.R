box::use(here[here], data.table[setDT, copy], stats, teachingApps[rbeta4])

#' @export
generate_data <- function(data, type = c("survival", "binary", "ordinal"), 
                          prognostic = TRUE, seed, ...) {
  args <- list(...)
  switch(match.arg(type), 
         survival = gds(data, args$n, args$effect_size, prognostic, seed), 
         ordinal = gdo(data, args$n, args$effect_size, prognostic, seed))
}

gds <- function(data, n, effect_size, prognostic, seed) {
  set.seed(seed)
  if (prognostic) {
    boot <- setDT(data[sample(nrow(data), n, replace = TRUE), ])
  } else {
    hold <- data[, .(days, event)]
    boot <- copy(data)
    boot[, `:=`(days = NULL, event = NULL)]
    boot <- cbind(boot[sample(nrow(boot)), ], hold)
    boot <- boot[sample(nrow(boot), n, replace = TRUE), ]
  }
  K <- max(boot$days)
  boot[, A := stats$rbinom(n, 1, 0.5)]
  boot[event == 1 & A == 1, days := days + round(stats$rchisq(.N, df = effect_size), 0)]
  boot[, days := pmin(days, K)]
  boot[, id := 1:n]
  cens <- stats$runif(n) < 0.05
  C <- sample(1:max(boot$days), n, replace = TRUE)
  boot[C < days & cens == 1, 
       `:=`(days = C[C < boot$days & cens == 1], 
            event = 0)]
  boot[]
}

plot_beta <- function(shape1, shape2) {
  betar <- function(x) {
    dbeta4(x, 0, 5, shape1, shape2)
  }
  curve(betar, 0, 5, 1000)
}

gdo <- function(data, n, effect_size, prognostic, seed) {
  set.seed(seed)
  if (prognostic) {
    boot <- setDT(data[sample(nrow(data), n, replace = TRUE), ])
  } else {
    hold <- data[, .(state_ordinal)]
    boot <- copy(data)
    boot[, state := NULL]
    boot <- cbind(boot[sample(nrow(boot)), ], hold)
    boot <- boot[sample(nrow(boot), n, replace = TRUE), ]
  }
  K <- min(boot$state_ordinal)
  boot[, A := stats$rbinom(n, 1, 0.5)]
  if (effect_size != 0) {
    boot[A == 1, state_ordinal := state_ordinal - 
           round(rbeta4(.N, 0, 5, effect_size, 15, seed = seed), 0)]
    boot[, state_ordinal := pmax(state_ordinal, K)]
  }
  boot[]
}

#' @export
covid <- function(type = c("survival", "ordinal")) {
  switch(match.arg(type), 
         survival = readRDS(here("data", "private", "covid-update.rds")), 
         ordinal = readRDS(here("data", "private", "covid_14day_ordinal.rds")))
}

#' @export
truth <- function(data, type = c("survival", "binary", "ordinal"), ...) {
  args <- list(...)
  switch(match.arg(type), 
         survival = tds(data, args$effect_size, args$horizon), 
         ordinal = tdo(data, args$s1, args$s2))
}

tds <- function(data, effect_size, horizon) {
  mean(pmin(data$days + round(stats$rchisq(length(data$days), df = effect_size), 0), horizon) - 
         pmin(data$days, horizon))
}

tdo <- function(data, effect_size) {
  states_1 <- pmax(data$state_ordinal - 
                     round(rbeta4(nrow(data), 0, 5, effect_size, 15, sample(53443, 1)), 0))
  states_0 <- data$state_ordinal
  probs_1 <- vector("numeric", 6)
  probs_0 <- probs_1
  for (i in 0:5) {
    probs_1[i + 1] <- mean(states_1 <= i)
    probs_0[i + 1] <- mean(states_0 <= i)
  }
  
  lo_1 <- vector("numeric", 5)
  lo_0 <- lo_1
  for (i in 1:5) {
    lo_1[i] <- log(probs_1[i] / (1 - probs_1[i]))
    lo_0[i] <- log(probs_0[i] / (1 - probs_0[i]))
  }
  mean(lo_1) - mean(lo_0)
}
