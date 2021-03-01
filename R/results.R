box::use(dt = data.table, purrr[map_dfr], stats[qnorm, var])

#' @export
clean_surv <- function(fits, lasso = FALSE) {
  map_dfr(fits, function(fit) {
    if (lasso && class(fit) != "rmst") {
      out <- dt$data.table(theta = fit$res$estimates[[1]]$theta, 
                           std.error = fit$res$estimates[[1]]$std.error)
      return(out)
    }
    dt$data.table(theta = fit$estimates[[1]]$theta, 
                  std.error = fit$estimates[[1]]$std.error)
  })
}

#' @export
summary <- function(data, truth) {
  out <- data[, .(power = mean(abs(theta / std.error) > qnorm(1 - 0.05 / 2)), 
           mse = mean((theta - truth)^2), 
           bias = mean(theta - truth), 
           var = var(theta)), .(covar, n, es)]
  ref <- rep(out[covar == 1 & n == out$n & es == out$es, mse], each = length(unique(out$covar)))
  out[, rel.eff := mse / ref][]
}
