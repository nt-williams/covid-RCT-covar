# Nick Williams
# Research Biostatistician
# Department of Population Health Sciences
# Weill Cornell Medicine

library(data.table)
library(ggplot2)
source("R/results.R")

main <- file("./papers/tables.tex", open = "a")
truth <- readRDS("./data/truth.rds")

# prognostic --------------------------------------------------------------

# survival
sunadj <- readRDS("./data/sunad.rds")
spns <- readRDS("./data/spns.rds")
sps <- readRDS("./data/sps.rds")
sprf <- readRDS("./data/sprf.rds")
sprfcf <- readRDS("./data/sprfcf.rds")
spxg <- readRDS("./data/spxg.rds")
spxgcf <- readRDS("./data/spxgcf.rds")
spmr <- readRDS("./data/spmr.rds")
spmrcf <- readRDS("./data/spmrcf.rds")

spns <- spns[covar_id == 11, ]

spns[, covar_id := covar_id + 1]
sps[, `:=`(covar_id = covar_id + 17)]
sprf[, `:=`(covar_id = covar_id + 18)]
sprfcf[, `:=`(covar_id = covar_id + 19)]
spxg[, `:=`(covar_id = covar_id + 20)]
spxgcf[, `:=`(covar_id = covar_id + 21)]
spmr[, `:=`(covar_id = covar_id + 22)]
spmrcf[, `:=`(covar_id = covar_id + 23)]

sp <- rbind(sunadj, spns, sps, sprf, sprfcf, spxg, spxgcf, spmr, spmrcf, fill = TRUE)[order(n, covar_id)]
label(sp)

use <- c("Unadjusted", "LR", "LASSO", "RF", "CF-RF", "XGBoost", "CF-XGBoost", "MARS", "CF-MARS")
sp <- sp[covar_id %in% use]

# ordinal
ounadj <- readRDS("./data/ounad.rds")
opns <- readRDS("./data/opns.rds")
ops <- readRDS("./data/ops.rds")
oprf <- readRDS("./data/oprf.rds")
oprfcf <- readRDS("./data/oprfcf.rds")
opxg <- readRDS("./data/opxg.rds")
opxgcf <- readRDS("./data/opxgcf.rds")
opmr <- readRDS("./data/opmr.rds")
opmrcf <- readRDS("./data/opmrcf.rds")

opns <- opns[covar_id == 11, ]

opns[, covar_id := covar_id + 1]
ops[, covar_id := covar_id + 17]
oprf[, covar_id := covar_id + 18]
oprfcf[, covar_id := covar_id + 19]
opxg[, covar_id := covar_id + 20]
opxgcf[, covar_id := covar_id + 21]
opmr[, covar_id := covar_id + 22]
opmrcf[, covar_id := covar_id + 23]

op <- rbind(ounadj, opns, ops, oprf, oprfcf, opxg, opxgcf, opmr, opmrcf, fill = TRUE)[order(n, covar_id)]
label(op)

op <- op[covar_id %in% use][log_or < Inf & log_or > -Inf]

# D.1: Tables 1 & 2
for (i in 1:2) {
  tab <- summary(
    sp[es == 4 & n %in% c(100, 500, 1500)],
    c("rmst", "survprob")[i],
    c("rmst.std.error", "survprob.std.error")[i],
    truth[[c("rmst", "survprob")[i]]][[3]]
  )
  
  cols1 <- c("es", "power", "bias", "rel.eff")
  cols2 <- c("mse", "var")
  tab <- as.data.frame(tab)
  
  if (i == 1) {
    tab[, cols1] <- format(tab[, cols1], nsmall = 2)
    tab[, cols2] <- format(tab[, cols2], digits = 0)
  } else {
    tab[, c(cols1, cols2)] <- format(tab[, c(cols1, cols2)], nsmall = 2)
  }
  
  make_table(tab, main)
}

# D.1: Tables 3 & 4
for (i in 1:2) {
  tab <- summary(
    op[es == 3 & n %in% c(100, 500, 1500)],
    c("log_or", "mannwhit")[i], 
    c("log_or.std.error", "mannwhit.std.error")[i], 
    truth[[c("lor", "mw")[i]]][[3]], 
    c(0, 0.5)[i]
  )
  
  cols1 <- c("es", "power", "bias", "rel.eff")
  cols2 <- c("mse", "var")
  tab <- as.data.frame(tab)
  
  if (i == 1) {
    tab[, cols1] <- format(tab[, cols1], nsmall = 2)
    tab[, cols2] <- format(tab[, cols2], digits = 0)
  } else {
    tab[, c(cols1, cols2)] <- format(tab[, c(cols1, cols2)], nsmall = 2)
  }
  
  make_table(tab, main)
}

# D.2: Tables 5 & 6
for (i in 1:2) {
  tab <- summary(
    sp[es == 0 & n %in% c(100, 500, 1500)],
    c("rmst", "survprob")[i],
    c("rmst.std.error", "survprob.std.error")[i],
    truth[[c("rmst", "survprob")[i]]][[1]]
  )
  
  cols1 <- c("es", "power", "bias", "rel.eff")
  cols2 <- c("mse", "var")
  tab <- as.data.frame(tab)
  
  if (i == 1) {
    tab[, cols1] <- format(tab[, cols1], nsmall = 2)
    tab[, cols2] <- format(tab[, cols2], digits = 0)
  } else {
    tab[, c(cols1, cols2)] <- format(tab[, c(cols1, cols2)], nsmall = 2)
  }
  
  make_table(tab, main)
}

# D.2: Tables 7 & 8
for (i in 1:2) {
  tab <- summary(
    op[es == 0 & n %in% c(100, 500, 1500)],
    c("log_or", "mannwhit")[i], 
    c("log_or.std.error", "mannwhit.std.error")[i], 
    truth[[c("lor", "mw")[i]]][[1]], 
    c(0, 0.5)[i]
  )
  
  cols1 <- c("es", "power", "bias", "rel.eff")
  cols2 <- c("mse", "var")
  tab <- as.data.frame(tab)
  
  if (i == 1) {
    tab[, cols1] <- format(tab[, cols1], nsmall = 2)
    tab[, cols2] <- format(tab[, cols2], digits = 0)
  } else {
    tab[, c(cols1, cols2)] <- format(tab[, c(cols1, cols2)], nsmall = 2)
  }
  
  make_table(tab, main)
}

# n/prognostic ------------------------------------------------------------

# survival
sunadj <- readRDS("./data/sunad.rds")
snpns <- readRDS("./data/snpns.rds")
snps <- readRDS("./data/snps.rds")
snprf <- readRDS("./data/snprf.rds")
snprfcf <- readRDS("./data/snprfcf.rds")
snpxg <- readRDS("./data/snpxg.rds")
snpxgcf <- readRDS("./data/snpxgcf.rds")
snpmr <- readRDS("./data/snpmr.rds")
snpmrcf <- readRDS("./data/snpmrcf.rds")

snpns[, covar_id := covar_id + 11]
snps[, covar_id := covar_id + 17]
snprf[, covar_id := covar_id + 18]
snprfcf[, covar_id := covar_id + 19]
snpxg[, covar_id := covar_id + 20]
snpxgcf[, covar_id := covar_id + 21]
snpmr[, covar_id := covar_id + 22]
snpmrcf[, covar_id := covar_id + 23]

snp <- rbind(sunadj, snpns, snps, snprf, snprfcf, snpxg, snpxgcf, snpmr, snpmrcf, fill = TRUE)[order(n, covar_id)]
label(snp)

snp <- snp[covar_id %in% use]

# ordinal
ounadj <- readRDS("./data/ounad.rds")
onpns <- readRDS("./data/onpns.rds")
onps <- readRDS("./data/onps.rds")
onprf <- readRDS("./data/onprf.rds")
onprfcf <- readRDS("./data/onprfcf.rds")
onpxg <- readRDS("./data/onpxg.rds")
onpxgcf <- readRDS("./data/onpxgcf.rds")
onpmr <- readRDS("./data/onpmr.rds")
onpmrcf <- readRDS("./data/onpmrcf.rds")

onpns[, covar_id := covar_id + 11]
onps[, covar_id := covar_id + 17]
onprf[, covar_id := covar_id + 18]
onprfcf[, covar_id := covar_id + 19]
onpxg[, covar_id := covar_id + 20]
onpxgcf[, covar_id := covar_id + 21]
onpmr[, covar_id := covar_id + 22]
onpmrcf[, covar_id := covar_id + 23]

onp <- rbind(ounadj, onpns, onps, onprf, onprfcf, onpxg, onpxgcf, onpmr, onpmrcf, fill = TRUE)[order(n, covar_id)]
label(onp)

onp <- onp[covar_id %in% use][log_or < Inf & log_or > -Inf]

# D.3: Tables 9 & 10
for (i in 1:2) {
  tab <- summary(
    snp[es == 4 & n %in% c(100, 500, 1500)],
    c("rmst", "survprob")[i],
    c("rmst.std.error", "survprob.std.error")[i],
    truth[[c("rmst", "survprob")[i]]][[3]]
  )
  
  cols1 <- c("es", "power", "bias", "rel.eff")
  cols2 <- c("mse", "var")
  tab <- as.data.frame(tab)
  
  if (i == 1) {
    tab[, cols1] <- format(tab[, cols1], nsmall = 2)
    tab[, cols2] <- format(tab[, cols2], digits = 0)
  } else {
    tab[, c(cols1, cols2)] <- format(tab[, c(cols1, cols2)], nsmall = 2)
  }
  
  make_table(tab, main)
}

# D.3: Tables 11 & 12
for (i in 1:2) {
  tab <- summary(
    onp[es == 3 & n %in% c(100, 500, 1500)],
    c("log_or", "mannwhit")[i], 
    c("log_or.std.error", "mannwhit.std.error")[i], 
    truth[[c("lor", "mw")[i]]][[3]], 
    c(0, 0.5)[i]
  )
  
  cols1 <- c("es", "power", "bias", "rel.eff")
  cols2 <- c("mse", "var")
  tab <- as.data.frame(tab)
  
  if (i == 1) {
    tab[, cols1] <- format(tab[, cols1], nsmall = 2)
    tab[, cols2] <- format(tab[, cols2], digits = 0)
  } else {
    tab[, c(cols1, cols2)] <- format(tab[, c(cols1, cols2)], nsmall = 2)
  }
  
  make_table(tab, main)
}

# D.4: Tables 13 & 14
for (i in 1:2) {
  tab <- summary(
    snp[es == 0 & n %in% c(100, 500, 1500)],
    c("rmst", "survprob")[i],
    c("rmst.std.error", "survprob.std.error")[i],
    truth[[c("rmst", "survprob")[i]]][[1]]
  )
  
  cols1 <- c("es", "power", "bias", "rel.eff")
  cols2 <- c("mse", "var")
  tab <- as.data.frame(tab)
  
  if (i == 1) {
    tab[, cols1] <- format(tab[, cols1], nsmall = 2)
    tab[, cols2] <- format(tab[, cols2], digits = 0)
  } else {
    tab[, c(cols1, cols2)] <- format(tab[, c(cols1, cols2)], nsmall = 2)
  }
  
  make_table(tab, main)
}

# D.4: Tables 15 & 16
for (i in 1:2) {
  tab <- summary(
    onp[es == 0 & n %in% c(100, 500, 1500)],
    c("log_or", "mannwhit")[i], 
    c("log_or.std.error", "mannwhit.std.error")[i], 
    truth[[c("lor", "mw")[i]]][[1]], 
    c(0, 0.5)[i]
  )
  
  cols1 <- c("es", "power", "bias", "rel.eff")
  cols2 <- c("mse", "var")
  tab <- as.data.frame(tab)
  
  if (i == 1) {
    tab[, cols1] <- format(tab[, cols1], nsmall = 2)
    tab[, cols2] <- format(tab[, cols2], digits = 0)
  } else {
    tab[, c(cols1, cols2)] <- format(tab[, c(cols1, cols2)], nsmall = 2)
  }
  
  make_table(tab, main)
}

close(main)

# plots -------------------------------------------------------------------

to_plot <- list()

for (i in 1:2) {
  tab <- summary(
    sp[es == 0 & n %in% c(100, 500, 1500)],
    c("rmst", "survprob")[i],
    c("rmst.std.error", "survprob.std.error")[i],
    truth[[c("rmst", "survprob")[i]]][[1]]
  )
  tab$estimand <- c("RMST", "RD")[i]
  tab$prognostic <- TRUE
  to_plot <- append(to_plot, list(tab))
}

for (i in 1:2) {
  tab <- summary(
    op[es == 0 & n %in% c(100, 500, 1500)],
    c("log_or", "mannwhit")[i], 
    c("log_or.std.error", "mannwhit.std.error")[i], 
    truth[[c("lor", "mw")[i]]][[1]], 
    c(0, 0.5)[i]
  )
  tab$estimand <- c("LOR", "MW")[i]
  tab$prognostic <- TRUE
  to_plot <- append(to_plot, list(tab))
}

for (i in 1:2) {
  tab <- summary(
    sp[es == 4 & n %in% c(100, 500, 1500)],
    c("rmst", "survprob")[i],
    c("rmst.std.error", "survprob.std.error")[i],
    truth[[c("rmst", "survprob")[i]]][[3]]
  )
  tab$estimand <- c("RMST", "RD")[i]
  tab$prognostic <- TRUE
  to_plot <- append(to_plot, list(tab))
}

for (i in 1:2) {
  tab <- summary(
    op[es == 3 & n %in% c(100, 500, 1500)],
    c("log_or", "mannwhit")[i], 
    c("log_or.std.error", "mannwhit.std.error")[i], 
    truth[[c("lor", "mw")[i]]][[3]], 
    c(0, 0.5)[i]
  )
  tab$estimand <- c("LOR", "MW")[i]
  tab$prognostic <- TRUE
  to_plot <- append(to_plot, list(tab))
}

for (i in 1:2) {
  tab <- summary(
    snp[es == 0 & n %in% c(100, 500, 1500)],
    c("rmst", "survprob")[i],
    c("rmst.std.error", "survprob.std.error")[i],
    truth[[c("rmst", "survprob")[i]]][[1]]
  )
  tab$estimand <- c("RMST", "RD")[i]
  tab$prognostic <- FALSE
  to_plot <- append(to_plot, list(tab))
}

for (i in 1:2) {
  tab <- summary(
    onp[es == 0 & n %in% c(100, 500, 1500)],
    c("log_or", "mannwhit")[i], 
    c("log_or.std.error", "mannwhit.std.error")[i], 
    truth[[c("lor", "mw")[i]]][[1]], 
    c(0, 0.5)[i]
  )
  tab$estimand <- c("LOR", "MW")[i]
  tab$prognostic <- FALSE
  to_plot <- append(to_plot, list(tab))
}

for (i in 1:2) {
  tab <- summary(
    snp[es == 4 & n %in% c(100, 500, 1500)],
    c("rmst", "survprob")[i],
    c("rmst.std.error", "survprob.std.error")[i],
    truth[[c("rmst", "survprob")[i]]][[3]]
  )
  tab$estimand <- c("RMST", "RD")[i]
  tab$prognostic <- FALSE
  to_plot <- append(to_plot, list(tab))
}

for (i in 1:2) {
  tab <- summary(
    onp[es == 3 & n %in% c(100, 500, 1500)],
    c("log_or", "mannwhit")[i], 
    c("log_or.std.error", "mannwhit.std.error")[i], 
    truth[[c("lor", "mw")[i]]][[3]], 
    c(0, 0.5)[i]
  )
  tab$estimand <- c("LOR", "MW")[i]
  tab$prognostic <- FALSE
  to_plot <- append(to_plot, list(tab))
}

to_plot <- rbindlist(to_plot)

to_plot$covar_id <- ifelse(to_plot$covar_id == "LASSO", paste0("\u2113", "1-LR"), to_plot$covar_id)

ragg::agg_png("figures/ivan_prez_eff_null_prognostic.pdf", width = 8, height = 4.5, units = "cm", res = 400)
to_plot[estimand %in% c("RMST", "RD") & 
          !(covar_id %in% c("XGBoost", "MARS", "RF")), 
        ][, covar_id := fifelse(startsWith(covar_id, "CF-"), gsub("CF-", "", covar_id), covar_id)
          ] |> 
  ggplot(aes(x = reorder(covar_id, -rel.eff, FUN = sum), y = 1 / rel.eff, fill = factor(n))) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(rows = vars(estimand)) + 
  coord_cartesian(ylim = c(0, 2)) + 
  #scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  labs(
    x = NULL, 
    y = "Rel. Efficiency", 
    fill = "n"
  ) + 
  theme_bw(base_size = 5, 
           base_line_size = 0.2,
           base_rect_size = 0.2) + 
  theme(strip.background = element_rect(fill = "white", color = "white"), 
        panel.spacing.x = unit(4, "mm"), 
        legend.key.size = unit(2, "mm"))
dev.off()

base_plot1 <- function(data) {
  ggplot(data, aes(x = reorder(covar_id, -rel.eff, FUN = sum), y = rel.eff, fill = factor(n))) + 
    geom_bar(stat = "identity", position = "dodge") + 
    facet_grid(rows = vars(estimand)) + 
    coord_cartesian(ylim = c(0, 2)) + 
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    labs(
      x = NULL, 
      y = "Rel. Efficiency", 
      fill = "n"
    ) + 
    theme_bw(base_size = 5, 
             base_line_size = 0.2,
             base_rect_size = 0.2) + 
    theme(strip.background = element_rect(fill = "white", color = "white"), 
          panel.spacing.x = unit(4, "mm"), 
          legend.key.size = unit(2, "mm"))
}

ragg::agg_png("figures/eff_null_prognostic.pdf", width = 8, height = 4.5, units = "cm", res = 400)
base_plot1(to_plot[(es == 0 | es == 0.5) & prognostic, ])
dev.off()

ragg::agg_png("figures/eff_positive_prognostic.pdf", width = 8, height = 4.5, units = "cm", res = 400)
base_plot1(to_plot[(es != 0 & es != 0.5) & prognostic, ])
dev.off()

ragg::agg_png("figures/eff_null_not_prognostic.pdf", width = 8, height = 4.5, units = "cm", res = 400)
base_plot1(to_plot[(es == 0 | es == 0.5) & !prognostic, ])
dev.off()

ragg::agg_png("figures/eff_positive_not_prognostic.pdf", width = 8, height = 4.5, units = "cm", res = 400)
base_plot1(to_plot[(es != 0 & es != 0.5) & !prognostic, ])
dev.off()

base_plot2 <- function(data) {
  ggplot(data, aes(y = power, fill = factor(n))) + 
    facet_grid(rows = vars(estimand)) + 
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    labs(
      x = NULL, 
      y = expression(paste("P(Reject ", H[0], " )")), 
      fill = "n"
    ) +
    theme_bw(base_size = 5,
             base_line_size = 0.2,
             base_rect_size = 0.2) +
    theme(strip.background = element_rect(fill = "white", color = "white"),
          panel.spacing.x = unit(4, "mm"),
          legend.key.size = unit(2, "mm"))
}

ragg::agg_png("figures/power_null_prognostic.pdf", width = 8, height = 4.5, units = "cm", res = 400)
base_plot2(to_plot[(es == 0 | es == 0.5) & prognostic, ]) + 
  geom_bar(aes(x = reorder(covar_id, -power)), stat = "identity", position = "dodge") + 
  geom_hline(yintercept = 0.05, size = 0.15, linetype = "dashed")
dev.off()

ragg::agg_png("figures/power_positive_prognostic.pdf", width = 8, height = 4.5, units = "cm", res = 400)
base_plot2(to_plot[(es != 0 & es != 0.5) & prognostic, ]) + 
  geom_bar(aes(x = reorder(covar_id, power, FUN = sum)), stat = "identity", position = "dodge") 
dev.off()

ragg::agg_png("figures/power_null_not_prognostic.pdf", width = 8, height = 4.5, units = "cm", res = 400)
base_plot2(to_plot[(es == 0 | es == 0.5) & !prognostic, ]) + 
  geom_bar(aes(x = reorder(covar_id, -power)), stat = "identity", position = "dodge") + 
  geom_hline(yintercept = 0.05, size = 0.15, linetype = "dashed")
dev.off()

ragg::agg_png("figures/power_positive_not_prognostic.pdf", width = 8, height = 4.5, units = "cm", res = 400)
base_plot2(to_plot[(es != 0 & es != 0.5) & !prognostic, ]) + 
  geom_bar(aes(x = reorder(covar_id, power, FUN = sum)), stat = "identity", position = "dodge") 
dev.off()
