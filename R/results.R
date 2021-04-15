box::use(dt = data.table, purrr[map_dfr], stats[qnorm, var], knitr[kable])

#' @export
summary <- function(data, var, std, truth, null = 0) {
  out <- dt$copy(data)
  cols <- c("es", "power", "mse", "bias", "rel.eff")
  dt$setnames(out, c(var, std), c("theta", "std.error"))
  out <- out[, .(power = mean(abs((theta - null) / std.error) > qnorm(1 - 0.05 / 2), na.rm = TRUE), 
                 mse = mean((theta - truth)^2, na.rm = TRUE) * n, 
                 bias = mean(theta - truth, na.rm = TRUE)), .(covar_id, n, es)]
  ref <- rep(out[covar_id == "Unadjusted" & n == out$n & es == out$es, mse], 
             each = length(unique(out$covar_id)))
  out[, rel.eff := mse / ref
      ][, es := truth
        ][order(n, mse), 
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
                              covar_id == 12, "GLM", 
                              covar_id == 18, "LASSO", 
                              covar_id == 19, "Random forest", 
                              covar_id == 20, "(CF) Random forest", 
                              covar_id == 21, "XGBoost", 
                              covar_id == 22, "(CF) XGBoost", 
                              covar_id == 23, "MARS", 
                              covar_id == 24, "(CF) MARS")]
}

#' @export
make_table <- function(data, caption, con) {
  data[, n := as.character(n)]
  .data <- format(as.data.frame(data), nsmall = 2)
  brew::brew("./scripts/table.brew", output = con)
}

kbt <- function(x) {
  kable(x, digits = 2, format = "latex", linesep = "")
}
