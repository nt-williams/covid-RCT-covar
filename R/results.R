box::use(dt = data.table, purrr[map_dfr], stats[qnorm, var])

#' @export
clean <- function(type = c("survival", "ordinal"), fits, ...) {
  args <- list(...)
  switch(match.arg(type), 
         survival = clean_surv(fits), 
         ordinal = clean_ord(fits))
}

#' @export
clean_surv <- function(fits) {
  map_dfr(fits, function(fit) {
      out <- try(dt$data.table(rmst = fit$res$rmst$estimates[[1]]$theta, 
                               rmst.std.error = fit$res$rmst$estimates[[1]]$std.error, 
                               survprob = fit$res$survprob$estimates[[1]]$theta, 
                               survprob.std.error = fit$res$survprob$estimates[[1]]$std.error))
      if (!(inherits(out, "try-error"))) {
        return(out)
      }
  })
}

#' @export
clean_ord <- function(fits) {
  map_dfr(fits, function(fit) {
    out <- try(dt$data.table(log_or = fit$res$log_or$estimates$lor$theta, 
                             log_or.std.error = fit$res$log_or$estimates$std.error, 
                             mannwhit = fit$res$mannwhit$estimates$theta, 
                             mannwhit.std.error = fit$res$mannwhit$estimates$std.error[1, ]))
    if (!(inherits(out, "try-error"))) {
      return(out)
    }
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
