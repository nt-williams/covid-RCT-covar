Var <- function(x) {
  sum((x - mean(x))^2) / (length(x) - 1)
}

MSE <- function(x, truth) {
  sum((x - truth)^2) / length(x)
}

mcse_bias <- function(x) {
  n <- length(x) 
  sqrt(Var(x) / n)
}

mcse_var <- function(x) {
  n <- length(x)
  Var(x) * sqrt(2 / (n - 1))
}

mcse_rel_eff <- function(x, ref) {
  n <- length(x)
  rho <- cor(x, ref)
  (2 * Var(x) / Var(ref)) * sqrt((1 - rho^2) / (n - 1))
}

mcse_mse <- function(x, truth) {
  n <- length(x)
  sqrt(sum(((x - truth)^2 - MSE(x, truth))^2) / (n * (n - 1)))
}

mcse_power <- function(x, se) {
  pow <- sum(abs(x) >= (qnorm(0.975) * se)) / length(x)
  sqrt(pow * (1 - pow) / length(x))
}

mcse <- function(data, theta, se, truth, method, ref) {
  methods <- split(data, data[[method]])
  
  out <- map_dfr(methods, function(dat) {
    dat <- na.omit(dat)
    for_cor <- intersect(dat[["row"]], methods[[ref]][["row"]])
    data.frame(
      covar_id = unique(dat[[method]]),
      bias = mcse_bias(dat[[theta]]), 
      var = mcse_var(dat[[theta]]), 
      mse = mcse_mse(dat[[theta]], truth),
      power = mcse_power(dat[[theta]], dat[[se]]), 
      rel_eff = mcse_rel_eff(subset(dat, row %in% for_cor)[[theta]], 
                             subset(methods[[ref]], row %in% for_cor)[[theta]])
    ) |> 
      mutate(across(-covar_id, \(x) if_else(is.nan(x), 0, x)))
  })
  
  summarise(out, across(-covar_id, max))
}
