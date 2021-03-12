box::use(dgm = ./data, adjrct[...], stats)

#' @export
partition <- function(tasks, covar, id, machines, outpath) {
  rows <- (1:nrow(tasks))[(0:(nrow(tasks) - 1)) %/% (nrow(tasks) / machines) + 1 == id]
  out <- list()
  
  for (r in 1:length(rows)) {
    row <- rows[[r]]
    if (tasks$type[row] == "survival") {
      c19 <- dgm$covid("survival")
    } else if (tasks$type[row] == "ordinal") {
      c19 <- dgm$covid("ordinal")
    }
    out[[r]] <- try(
        simulate(c19, tasks$type[row], covar[[tasks$covar_id[row]]], tasks$prog[row], tasks$seed[row], 
                 lasso = tasks$lasso[row], n = tasks$n[row], effect_size = tasks$effect_size[row])
      )
    
    if (!inherits(out[[r]], "try-error")) {
      out[[r]]["row"] <- row
      out[[r]]["covar_id"] <- tasks$covar_id[row]
      out[[r]]["n"] <- tasks$n[row]
      out[[r]]["es"] <- tasks$effect_size[row]
      out[[r]]["progs"] <- tasks$prog[row]
      out[[r]]["lasso"] <- tasks$lasso[row]
    }
  }
  saveRDS(out, 
          file.path(outpath, paste0(tasks$type[row], "_", 
                                    tasks$prog[row], "_",
                                    tasks$lasso[row], "_",
                                    id, ".rds")))
}

#' @export
simulate <- function(.data, type = c("survival", "ordinal"), 
                     covar, prognostic, seed, ...) {
  cnt <- match.arg(type)
  args <- list(...)
  if (cnt == "survival") {
    dat <- dgm$generate_data(.data, cnt, prognostic, seed, n = args$n, effect_size = args$effect_size)
    if (covar[1] == "none") {
      covar <- NULL
      estimator <- "km"
    } else {
      estimator <- "tmle"
    }
    f <- stats$as.formula(paste0("Surv(days, event) ~ ", paste(c("A", covar), collapse = " + ")))
    surv <- suppressWarnings(survrct(f, "A", data = dat, estimator = estimator, lasso = args$lasso))
    est_rmst <- rmst(surv, 14)
    est_sp <- survprob(surv, 7)
    # if (args$lasso && estimator == "tmle") {
    #   fits <- get_fits(surv)
    #   out <- list(res = list(rmst = est_rmst, survprob = est_sp), 
    #               hazard = as.matrix(stats$coef(fits$Hazard)), 
    #               cens = as.matrix(stats$coef(fits$Censoring)), 
    #               treatment = as.matrix(stats$coef(fits$Treatment)))
    #   return(out)
    # }
    return(list(rmst = est_rmst$estimates[[1]]$theta, 
                rmst.std.error = est_rmst$estimates[[1]]$std.error,
                survprob = est_sp$estimates[[1]]$theta, 
                survprob.std.error = est_sp$estimates[[1]]$std.error))
  } else if (cnt == "ordinal") {
    dat <- dgm$generate_data(.data, cnt, prognostic, seed, n = args$n, 
                             effect_size = args$effect_size)

    if (covar[1] == "none") {
      f <- stats$as.formula("state_ordinal ~ A")
    } else {
      f <- stats$as.formula(paste0("state_ordinal ~ ", paste(c("A", covar), collapse = " + ")))
    }
    ord <- ordinalrct(f, target = "A", data = dat, estimator = "tmle", lasso = args$lasso)
    est_lor <- log_or(ord)
    est_mw <- mannwhitney(ord)
    # if (args$lasso) {
    #   fits <- get_fits(ord)
    #   out <- list(res = list(log_or = est_lor, mannwhit = est_mw), 
    #               hazard = as.matrix(stats$coef(fits$Hazard)), 
    #               treatment = as.matrix(stats$coef(fits$Treatment)))
    #   return(out)
    # }
    return(list(log_or = est_lor$estimates$lor$theta, 
                log_or.std.error = est_lor$estimates$std.error,
                mannwhit = est_mw$estimates$theta, 
                mannwhit.std.error = as.vector(est_mw$estimates$std.error)))
  }
}
