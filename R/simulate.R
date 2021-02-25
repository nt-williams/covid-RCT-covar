box::use(./data, survrct, stats, future, here)

#' @export
partition <- function(tasks, covar, id, machines, outpath) {
  rows <- (1:nrow(tasks))[(0:(nrow(tasks) - 1)) %/% (nrow(tasks) / machines) + 1 == id]
  out <- list()
  globals <- append(ls(), c("rows", "r", "c19"))
  
  for (r in 1:length(rows)) {
    row <- rows[[r]]
    if (tasks$type[row] == "survival") {
      c19 <- data$covid()
    } 
    out[[r]] <- future$future({
      try(
        simulate(c19, tasks$type[r], covar[[tasks$covar_id[r]]], tasks$seed[r], 
                 n = tasks$n[r], effect_size = tasks$effect_size[r])
      )
    }, globals = globals, seed = TRUE)
    saveRDS(future$value(out[[r]]), 
            here$here(outpath, paste0(tasks$type[r], "_", 
                                      covar[[tasks$covar_id[r]]], "_", 
                                      n = tasks$n[r], "_", 
                                      tasks$effect_size[r], "_", 
                                      r, ".rds")))
  }
}

simulate <- function(.data, type = c("survival", "binary", "ordinal"), covar, seed, ...) {
  cnt <- match.arg(type)
  args <- list(...)
  if (cnt == "survival") {
    dat <- data$generate_data(.data, cnt, seed, n = args$n, effect_size = args$effect_size)
    if (covar == "none") {
      covar <- NULL
      estimator <- "km"
    } else {
      estimator <- "tmle"
    }
    f <- stats$as.formula(paste0("Surv(T, D) ~ ", paste(c("A", covar), collapse = " + ")))
    surv <- suppressWarnings(survrct$survrct(f, "A", data = dat, estimator = estimator))
    survrct$rmst(surv, 14)
  }
}
