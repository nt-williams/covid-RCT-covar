box::use(dt = data.table, purrr[map_dfr], stats[qnorm, var], knitr[kable])

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
summary <- function(data, var, std, truth, null = 0) {
  out <- dt$copy(data)
  dt$setnames(out, c(var, std), c("theta", "std.error"))
  out <- out[, .(power = mean(abs((theta - null) / std.error) > qnorm(1 - 0.05 / 2), na.rm = TRUE), 
                 mse = mean((theta - truth)^2, na.rm = TRUE) * n, 
                 bias = mean(theta - truth, na.rm = TRUE)), .(covar_id, n, es)]
  ref <- rep(out[covar_id == "Unadjusted" & n == out$n & es == out$es, mse], 
             each = length(unique(out$covar_id)))
  out[, rel.eff := mse / ref][, es := truth][order(n, mse)]
}

#' @export
label <- function(data) {
  data[, covar_id := dt$fcase(covar_id == 1, "Unadjusted", 
                              covar_id == 2, "Age", 
                              covar_id == 3, "Sex", 
                              covar_id == 4, "BMI", 
                              covar_id == 5, "Supp. O2", 
                              covar_id == 6, "Smoking status", 
                              covar_id == 7, "No. comorbid", 
                              covar_id == 8, "No. symptoms", 
                              covar_id == 9, "X-ray bilat. infilt.", 
                              covar_id == 10, "Dyspnea", 
                              covar_id == 11, "Hypertension", 
                              covar_id == 12, "Age & supp. O2", 
                              covar_id == 13, "Set A", 
                              covar_id == 14, "Set B", 
                              covar_id == 15, "All", 
                              covar_id == 16, "LASSO, A", 
                              covar_id == 17, "LASSO, B", 
                              covar_id == 18, "LASSO, all")]
}

#' @export
kbt <- function(x) {
  kable(x, digits = 2, format = "latex", linesep = "")
}
