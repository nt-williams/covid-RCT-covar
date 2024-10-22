box::use(dt = data.table, purrr[map_dfr], stats[qnorm, var], knitr[kable])

#' @export
summary <- function(data, var, std, truth, null = 0) {
  out <- dt$copy(data)
  cols <- c("es", "power", "mse", "var", "bias", "rel.eff")
  dt$setnames(out, c(var, std), c("theta", "std.error"))
  mu_theta <- mean(out$theta)
  out <- out[, .(power = mean(abs((theta - null) / std.error) > qnorm(1 - 0.05 / 2), na.rm = TRUE), 
                 mse = mean((theta - truth)^2, na.rm = TRUE) * n, 
                 var = (mean((theta - truth)^2, na.rm = TRUE) - mean(theta - truth, na.rm = TRUE)^2) * n,
                 # var = var(theta, na.rm = TRUE) * n,
                 bias = mean(theta, na.rm = TRUE) - truth), .(covar_id, n, es)]
  ref <- rep(out[covar_id == "Unadjusted" & n == out$n & es == out$es, mse], 
             each = length(unique(out$covar_id)))
  out[, rel.eff := mse / ref
      ][, es := truth
        ][order(n, covar_id), 
          ][, (cols) := lapply(.SD, function(x) round(x, 2)), .SDcols = cols][]
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
                              covar_id == 12, "LR", 
                              covar_id == 18, "LASSO", 
                              covar_id == 19, "RF", 
                              covar_id == 20, "CF-RF", 
                              covar_id == 21, "XGBoost", 
                              covar_id == 22, "CF-XGBoost", 
                              covar_id == 23, "MARS", 
                              covar_id == 24, "CF-MARS")]
}

#' @export
make_table <- function(data, con) {
  data$n <- as.character(data$n)
  #data$covar_id <- dt$fifelse(data$covar_id == "LASSO", "$\\ell_1$-LR", data$covar_id)
  data$covar_id <- dt$fifelse(data$covar_id == "LASSO", sprintf("\U2113"), data$covar_id)
  brew::brew("./scripts/main.brew", output = con)
}
