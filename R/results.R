box::use(dt = data.table, purrr[map_dfr], stats[qnorm, var])

#' @export
clean <- function(fits) {
  map_dfr(fits, function(fit) {
    dt$data.table(theta = fit$estimates[[1]]$theta, 
                  std.error = fit$estimates[[1]]$std.error)
  })
}

#' @export
summary <- function(data, truth) {
  data[, .(power = mean(abs(theta / std.error) > qnorm(1 - 0.05 / 2)), 
           mse = mean((theta - truth)^2), 
           bias = mean(theta - truth), 
           var = var(theta)), adj
       ][, rel.eff := mse / .SD[adj == "none", mse]]
}