box::use(dt = data.table, purrr[map_dfr], stats[qnorm, var])

#' @export
clean <- function(type = c("survival", "ordinal"), fits, ...) {
  args <- list(...)
  if (match.arg(type) == "survial") {
    clean_surv(fits, args$lasso)
  }
}

#' @export
clean_surv <- function(fits, lasso = FALSE) {
  map_dfr(fits, function(fit) {
    if (lasso && class(fit) != "rmst") {
      out <- try(dt$data.table(theta = fit$res$estimates[[1]]$theta, 
                               std.error = fit$res$estimates[[1]]$std.error))
      if (!(inherits(out, "try-error"))) {
        return(out)
      }
    }
    out <- try(dt$data.table(theta = fit$estimates[[1]]$theta, 
                             std.error = fit$estimates[[1]]$std.error))
    if (!(inherits(out, "try-error"))) {
      return(out)
    }
  })
}

clean_surv_select <- function(fits) {
  haz <- map_dfr(fits, ~ as.data.table(as.list(.x$hazard[, 1] != 0)))
  cens <- map_dfr(fits, ~ as.data.table(as.list(.x$cens[, 1] != 0)))
  trt <- map_dfr(fits, ~ as.data.table(as.list(.x$treatment[, 1] != 0)))
  list(hazard = haz, 
       cens = cens, 
       trt = trt)
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
