box::use(here[here], data.table[setDT, copy], stats, teachingApps[rbeta4, dbeta4], graphics[curve])

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
    # boot <- cbind(boot[sample(nrow(boot)), ], hold)
    boot <- cbind(boot[sample(nrow(boot), n, replace = TRUE), ], 
                  hold[sample(nrow(hold), n, replace = TRUE), ])
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

#' @export
plot_beta <- function(effect_size) {
  betar <- function(x) {
    dbeta4(x, 0, 5, effect_size, 5)
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
    boot[, `:=`(state = NULL, state_ordinal = NULL)]
    # boot <- cbind(boot[sample(nrow(boot)), ], hold)
    boot <- cbind(boot[sample(nrow(boot), n, replace = TRUE), ], 
                  hold[sample(nrow(hold), n, replace = TRUE), ])
  }
  K <- min(boot$state_ordinal)
  boot[, A := stats$rbinom(n, 1, 0.5)]
  if (effect_size != 0) {
    boot[A == 1, state_ordinal := state_ordinal - 
           round(rbeta4(.N, 0, 5, effect_size, 15, seed = seed), 0)]
    boot[, state_ordinal := pmax(state_ordinal, K)]
  }
  boot[, state_ordinal := ordered(state_ordinal)][]
}

#' @export
covid <- function(type = c("survival", "ordinal"), private = TRUE) {
  if (private) {
    switch(match.arg(type), 
           survival = readRDS(here("data", "private", "covid-survival.rds")), 
           ordinal = readRDS(here("data", "private", "covid-ordinal.rds")))
  } else {
    switch(match.arg(type), 
           survival = readRDS(here("data", "public", "c19.tte.rds")), 
           ordinal = readRDS(here("data", "public", "c19.ordinal.rds")))
  }
}

#' @export
truth <- function(data, type = c("survival", "ordinal"), ...) {
  args <- list(...)
  switch(match.arg(type), 
         survival = tds(data, args$effect_size, args$estimand), 
         ordinal = tdo(data, args$effect_size, args$estimand))
}

tds <- function(data, effect_size, estimand = c("rmst", "sp")) {
  if (match.arg(estimand) == "rmst") {
    return(mean(pmin(data$days + round(stats$rchisq(length(data$days), df = effect_size), 0), 14) - 
                  pmin(data$days, 14)))
  }
  time_1 <- data$days + round(stats$rchisq(length(data$days), df = effect_size), 0)
  time_0 <- data$days
  mean(time_1 > 7) - mean(time_0 > 7)
}

tdo <- function(data, effect_size, estimand = c("lor", "mw")) {
  states_1 <- pmax(data$state_ordinal - round(rbeta4(nrow(data), 0, 5, effect_size, 15, sample(53443, 1)), 0), 0)
  states_0 <- data$state_ordinal
  cdf_1 <- vector("numeric", 6)
  cdf_0 <- cdf_1
  for (i in 0:5) {
    cdf_1[i + 1] <- mean(states_1 <= i)
    cdf_0[i + 1] <- mean(states_0 <= i)
  }
  
  if (match.arg(estimand) == "mw") {
    pmf_1 <- vector("numeric", 6)
    pmf_0 <- pmf_1
    for (i in 0:5) {
      pmf_1[i + 1] <- mean(states_1 == i)
      pmf_0[i + 1] <- mean(states_0 == i)
    }
    
    return(sum((c(0, cdf_0)[-6] + 0.5*pmf_0) * pmf_1))
  }
  
  lo_1 <- vector("numeric", 5)
  lo_0 <- lo_1
  for (i in 1:5) {
    lo_1[i] <- log(cdf_1[i] / (1 - cdf_1[i]))
    lo_0[i] <- log(cdf_0[i] / (1 - cdf_0[i]))
  }
  mean(lo_1) - mean(lo_0)
}
