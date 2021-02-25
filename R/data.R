#' @export
generate_data <- function(data, type = c("survival", "binary", "ordinal"), seed, ...) {
  args <- list(...)
  switch(match.arg(type), 
         survival = gds(data, args$n, args$effect_size, seed))
}

gds <- function(data, n, effect_size, seed) {
  set.seed(seed)
  boot <- data.table::setDT(data[sample(nrow(data), n, replace = TRUE), ])
  K <- max(boot$T)
  boot[, A := stats::rbinom(n, 1, 0.5)]
  boot[D == 1 & A == 1, T := T + round(stats::rchisq(.N, df = effect_size), 0)]
  boot[, T := pmin(T, K)]
  boot[, id := 1:n]
  cens <- stats::runif(n) < 0.05
  C <- sample(1:max(boot$T), n, replace = TRUE)
  boot[C < T & cens == 1, 
       `:=`(T = C[C < boot$T & cens == 1], 
            D = 0)]
  boot[]
}

#' @export
covid <- function() {
  readRDS("./data/private/covid.rds")
}

#' @export
truth <- function(data, type = c("survival", "binary", "ordinal"), ...) {
  args <- list(...)
  switch(match.arg(type), 
         survival = tds(data, args$effect_size, args$horizon))
}

tds <- function(data, effect_size, horizon) {
  mean(pmin(data$T + round(stats::rchisq(length(data$T), df = effect_size), 0), horizon) - 
         pmin(data$T, horizon))
}