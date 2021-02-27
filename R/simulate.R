box::use(dgm = ./data, survrct, stats, future)

#' @export
partition <- function(tasks, covar, id, machines, outpath) {
  rows <- (1:nrow(tasks))[(0:(nrow(tasks) - 1)) %/% (nrow(tasks) / machines) + 1 == id]
  out <- list()
  globals <- append(ls(), c("rows", "r", "c19", "simulate"))
  
  for (r in 1:length(rows)) {
    row <- rows[[r]]
    if (tasks$type[row] == "survival") {
      c19 <- dgm$covid()
    } 
    out[[r]] <- future$future({
      try(
        simulate(c19, tasks$type[row], covar[[tasks$covar_id[row]]], tasks$seed[row], 
                 n = tasks$n[row], effect_size = tasks$effect_size[row])
      )
    }, globals = globals, seed = TRUE)
    saveRDS(future$value(out[[r]]), 
            file.path(outpath, paste0(tasks$type[row], "_", 
                                      tasks$covar_id[row], "_", 
                                      n = tasks$n[row], "_", 
                                      tasks$effect_size[row], "_", 
                                      row, ".rds")))
  }
}

simulate <- function(.data, type = c("survival", "binary", "ordinal"), covar, seed, ...) {
  cnt <- match.arg(type)
  args <- list(...)
  if (cnt == "survival") {
    dat <- dgm$generate_data(.data, cnt, seed, n = args$n, effect_size = args$effect_size)
    if (covar == "none") {
      covar <- NULL
      estimator <- "km"
    } else {
      estimator <- "tmle"
    }
    f <- stats$as.formula(paste0("Surv(days, event) ~ ", paste(c("A", covar), collapse = " + ")))
    surv <- suppressWarnings(survrct$survrct(f, "A", data = dat, estimator = estimator))
    survrct$rmst(surv, 14)
  }
}
