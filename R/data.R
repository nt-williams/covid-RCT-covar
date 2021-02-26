#' @export
generate_data <- function(data, type = c("survival", "binary", "ordinal"), seed, ...) {
  args <- list(...)
  switch(match.arg(type), 
         survival = gds(data, args$n, args$effect_size, seed))
}

gds <- function(data, n, effect_size, seed) {
  set.seed(seed)
  boot <- data.table::setDT(data[sample(nrow(data), n, replace = TRUE), ])
  K <- max(boot$days)
  boot[, A := stats::rbinom(n, 1, 0.5)]
  boot[event == 1 & A == 1, days := days + round(stats::rchisq(.N, df = effect_size), 0)]
  boot[, days := pmin(days, K)]
  boot[, id := 1:n]
  cens <- stats::runif(n) < 0.05
  C <- sample(1:max(boot$days), n, replace = TRUE)
  boot[C < days & cens == 1, 
       `:=`(days = C[C < boot$days & cens == 1], 
            event = 0)]
  boot[]
}

#' @export
covid <- function() {
  readRDS("./data/private/covid-update.rds")
}

#' @export
truth <- function(data, type = c("survival", "binary", "ordinal"), ...) {
  args <- list(...)
  switch(match.arg(type), 
         survival = tds(data, args$effect_size, args$horizon))
}

tds <- function(data, effect_size, horizon) {
  mean(pmin(data$days + round(stats::rchisq(length(data$days), df = effect_size), 0), horizon) - 
         pmin(data$days, horizon))
}