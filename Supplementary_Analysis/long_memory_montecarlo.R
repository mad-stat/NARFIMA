################################################################################################
## S04  Monte Carlo check of the long-memory estimators (reviewer Comment 2a: "whether the
##      estimation procedure or algorithm is working correctly")
##
##  DGP: ARFIMA(0, d, 0), n = 274 (the paper's training sample), d in {0, 0.3, 0.45, 0.8, 1.0, 1.2},
##       Gaussian innovations; series with d >= 1/2 are cumulated sums of ARFIMA(0, d - 1, 0).
##  Estimators (m = floor(n^0.6) = 29): the published setup longmemo::WhittleEst ("Whittle (published)"),
##       GPH, bias-reduced GPH, local Whittle, exact local Whittle (ELW), feasible ELW (FELW) and
##       local Whittle on first differences + 1.
##  Reported: mean estimate, bias, RMSE and coverage of the nominal 95% interval d_hat +/- 1.96 s.e.
##  Usage (from Supplementary_Analysis/): Rscript long_memory_montecarlo.R [replications, default 1000]
################################################################################################

cmd <- commandArgs(trailingOnly = FALSE)
here <- dirname(normalizePath(sub("^--file=", "", cmd[grep("^--file=", cmd)])))
source(file.path(here, "R", "long_memory_estimators.R"))
args <- commandArgs(trailingOnly = TRUE)
R <- if (length(args)) as.integer(args[1]) else 1000L
out <- file.path(here, "results", "long_memory")
dir.create(out, recursive = TRUE, showWarnings = FALSE)

n <- 274
m <- floor(n^0.6)
d_grid <- c(0, 0.3, 0.45, 0.8, 1.0, 1.2)

one_rep <- function(d) {
  y <- sim_arfima(n, d)
  w <- whittle_bounded(y)
  est <- list(`Whittle (published setup)` = c(w$d, w$se),
              GPH = unlist(gph(y, m)[c("d", "se")]),
              `BR-GPH` = unlist(gph_bias_reduced(y, m)[c("d", "se")]),
              LW = unlist(local_whittle(y, m)[c("d", "se")]),
              ELW = unlist(elw(y, m)[c("d", "se")]),
              FELW = unlist(felw(y, m)[c("d", "se")]),
              `LW on differences + 1` = { l <- local_whittle(diff(y), m); c(l$d + 1, l$se) })
  do.call(rbind, lapply(names(est), function(k) data.frame(Estimator = k, d_hat = est[[k]][1], se = est[[k]][2])))
}

RNGkind("L'Ecuyer-CMRG")
set.seed(2026)
t0 <- Sys.time()
sims <- parallel::mclapply(d_grid, function(d) {
  r <- do.call(rbind, lapply(seq_len(R), function(i) cbind(rep = i, one_rep(d))))
  cbind(d_true = d, r)
}, mc.cores = min(length(d_grid), parallel::detectCores()), mc.set.seed = TRUE)
sims <- do.call(rbind, sims)
write.csv(sims, file.path(out, "S04_montecarlo_draws.csv"), row.names = FALSE)

summ <- do.call(rbind, lapply(split(sims, list(sims$d_true, sims$Estimator), drop = TRUE), function(s) {
  data.frame(d_true = s$d_true[1], Estimator = s$Estimator[1], Replications = nrow(s),
             Mean_d_hat = mean(s$d_hat), Bias = mean(s$d_hat) - s$d_true[1],
             RMSE = sqrt(mean((s$d_hat - s$d_true[1])^2)),
             Coverage_95 = mean(abs(s$d_hat - s$d_true[1]) <= 1.96 * s$se))
}))
summ <- summ[order(summ$Estimator, summ$d_true), ]
write.csv(summ, file.path(out, "S04_montecarlo_summary.csv"), row.names = FALSE)
cat(sprintf("Monte Carlo: %d replications x %d values of d, n = %d, m = %d, %.0f s\n", R, length(d_grid), n, m,
            as.numeric(difftime(Sys.time(), t0, units = "secs"))))
print(format(summ, digits = 3), row.names = FALSE)
