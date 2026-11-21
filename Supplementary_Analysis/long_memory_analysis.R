################################################################################################
## Long-memory analysis for the SNDE revision (reviewer Comment 2a)
##
##  S02  Check of the published Whittle table (Table_12_Whittle_Estimator.R): longmemo::WhittleEst
##       estimates H = d + 1/2 on (0, 1) and stops at H = 0.99; the published z-statistic is H / SE.
##  S03  Estimates of d for exchange rates (levels and first differences) and all candidate covariates:
##       GPH, bias-reduced GPH, local Whittle, exact local Whittle, feasible ELW (unknown mean),
##       at bandwidths m = n^0.5, n^0.6, n^0.7; ARFIMA(p, d, q) by approximate ML on first differences
##       (BIC over p, q in 0..2); tests of d = 0, d = 1/2 and d = 1; ADF, PP and KPSS; Lo's modified R/S.
##  S05  Spurious long memory and stability: Qu (2011) test, rolling-window FELW estimates of d,
##       Bai-Perron break tests on the first differences of the exchange rates.
##
##  Sample: the training sample of the 48-month horizon, 1997-01 to 2019-10 (n = 274), as in the paper.
##  Usage (from Supplementary_Analysis/):  Rscript long_memory_analysis.R
##  Needs: readxl, fracdiff, longmemo, arfima, tseries, strucchange.
################################################################################################

suppressPackageStartupMessages({
  library(readxl); library(fracdiff); library(tseries); library(strucchange)
})
cmd <- commandArgs(trailingOnly = FALSE)
here <- dirname(normalizePath(sub("^--file=", "", cmd[grep("^--file=", cmd)])))
source(file.path(here, "R", "long_memory_estimators.R"))
repo <- dirname(here)
out <- file.path(here, "results", "long_memory")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
set.seed(100)

## ------------------------------------------------------------------------------------- data
n_test <- 48
read_all <- function(country) {
  d <- read_excel(file.path(repo, "Dataset", "Dataset_All_Exogenous", paste0(country, "_Data.xlsx")))
  d <- d[!is.na(d[[1]]), ]
  head(d, nrow(d) - n_test)                                 # 1997-01 .. 2019-10
}
## keep the span with observations (as zoo::na.approx in Table_12_Whittle_Estimator.R) and interpolate inside it
interp <- function(x) {
  ok <- which(!is.na(x)); x <- x[min(ok):max(ok)]
  if (anyNA(x)) approx(seq_along(x), x, seq_along(x))$y else x
}
na_note <- function(x, dates) {
  if (!anyNA(x)) return("")
  ok <- which(!is.na(x))
  sprintf("observed %s to %s only (n = %d)", format(dates[min(ok)], "%Y-%m"), format(dates[max(ok)], "%Y-%m"),
          max(ok) - min(ok) + 1)
}

series <- list(); info <- list()
add <- function(country, name, x, note = "") {
  series[[length(series) + 1]] <<- as.numeric(x)
  info[[length(info) + 1]] <<- data.frame(Country = country, Series = name, n = length(x), Note = note)
}
codes <- c(Brazil = "brazil", Russia = "russia", India = "india", China = "china")
for (cty in names(codes)) {
  d <- read_all(cty); cc <- codes[[cty]]
  er <- d[[grep("^spot_ER", names(d))]]
  add(cty, "Exchange Rate", er)
  add(cty, "Exchange Rate (first difference)", diff(er))
  sr <- d[[paste0("SR_interest_rate_", cc)]]
  add(cty, "Short-term Interest Rate", interp(sr), na_note(sr, as.Date(d[[1]])))
  add(cty, "Short-term IRD", d[[grep("^SR_Interest_rate_diff", names(d))]])
  add(cty, "CPI Inflation", d[[grep(paste0("^CPI_Inflation_", tools::toTitleCase(cc)), names(d), ignore.case = TRUE)]])
  add(cty, "CPI Inflation Differential", d[[grep("^CPI_inflation_diff", names(d))]])
  add(cty, "GPR", d[[paste0("gprc_", cc)]])
}
g <- read_all("Brazil")
add("Global", "GEPU", g[["global_EPU(PPP)"]])
add("Global", "US EMV", g[["US_EMV"]])
add("Global", "US MPU", g[["US_MPU"]])
add("Global", "Oil Price Growth Rate", g[["Oil_price_growth_rate_WTI"]])
add("Global", "US Short-term Interest Rate", g[["SR_interest_rate_USA"]])
add("Global", "US CPI Inflation", g[["CPI_inflation_USA"]])
info <- do.call(rbind, info)
stopifnot(all(sapply(series, function(x) !anyNA(x))))
write.csv(info, file.path(out, "series_used.csv"), row.names = FALSE)

## ------------------------------------------------------------- S02: published Whittle table
s02 <- do.call(rbind, lapply(seq_along(series), function(i) {
  if (grepl("first difference", info$Series[i])) return(NULL)
  w <- whittle_bounded(series[[i]])
  data.frame(info[i, 1:2], H_hat = w$H, d_hat = w$d, SE = w$se, z_published_H_over_SE = w$z_reported,
             z_for_H0_d_eq_0 = w$z_d0, At_upper_bound_H_0.99 = w$at_bound)
}))
set.seed(1); rw <- whittle_bounded(cumsum(rnorm(274))); set.seed(1); wn <- whittle_bounded(rnorm(274))
s02 <- rbind(s02,
  data.frame(Country = "Simulated", Series = "Random walk (true d = 1)", H_hat = rw$H, d_hat = rw$d, SE = rw$se,
             z_published_H_over_SE = rw$z_reported, z_for_H0_d_eq_0 = rw$z_d0, At_upper_bound_H_0.99 = rw$at_bound),
  data.frame(Country = "Simulated", Series = "White noise (true d = 0)", H_hat = wn$H, d_hat = wn$d, SE = wn$se,
             z_published_H_over_SE = wn$z_reported, z_for_H0_d_eq_0 = wn$z_d0, At_upper_bound_H_0.99 = wn$at_bound))
write.csv(s02, file.path(out, "S02_whittle_table_check.csv"), row.names = FALSE)

## ------------------------------------------------------------------------- S03: estimates
bw <- c(0.5, 0.6, 0.7)
est_one <- function(x, m) {
  list(GPH = gph(x, m), `BR-GPH` = gph_bias_reduced(x, m), LW = local_whittle(x, m),
       ELW = elw(x, m), FELW = felw(x, m))
}
s03 <- do.call(rbind, lapply(seq_along(series), function(i) {
  x <- series[[i]]; n <- length(x)
  do.call(rbind, lapply(bw, function(b) {
    m <- floor(n^b); e <- est_one(x, m)
    do.call(rbind, lapply(names(e), function(k) {
      d <- e[[k]]$d; se <- e[[k]]$se
      data.frame(info[i, 1:2], Bandwidth = sprintf("n^%.1f", b), m = m, Estimator = k, d_hat = d, SE = se,
                 CI95_low = d - 1.96 * se, CI95_high = d + 1.96 * se,
                 p_H0_d0 = 2 * pnorm(-abs(d / se)), p_H0_d0.5 = 2 * pnorm(-abs((d - 0.5) / se)),
                 p_H0_d1 = 2 * pnorm(-abs((d - 1) / se)))
    }))
  }))
}))
write.csv(s03, file.path(out, "S03_long_memory_estimates_all_bandwidths.csv"), row.names = FALSE)

## parametric ARFIMA(p, d, q) on first differences of the exchange rates: exact Gaussian ML
## (arfima package, Veenstra & McLeod), d in (-1, 1/2), BIC selection over p, q in 0..2.
## (fracdiff cannot be used here: its optimiser fails when the d-range includes negative values.)
s03_ml <- do.call(rbind, lapply(names(codes), function(cty) {
  x <- diff(series[[which(info$Country == cty & info$Series == "Exchange Rate")]])
  fits <- list()
  for (p in 0:2) for (q in 0:2) {
    f <- try(suppressWarnings(arfima::arfima(x, order = c(p, 0, q), quiet = TRUE)), silent = TRUE)
    if (!inherits(f, "try-error")) {
      cf <- summary(f)$coef[[1]]
      fits[[paste(p, q)]] <- list(p = p, q = q, bic = BIC(f), d = cf["d.f", "Estimate"], se = cf["d.f", "Std. Error"])
    }
  }
  b <- fits[[which.min(sapply(fits, `[[`, "bic"))]]
  data.frame(Country = cty, Model = sprintf("ARFIMA(%d,d,%d) on first differences", b$p, b$q),
             d_diff = b$d, SE = b$se, d_level = 1 + b$d,
             CI95_level_low = 1 + b$d - 1.96 * b$se, CI95_level_high = 1 + b$d + 1.96 * b$se,
             p_H0_d_level_1 = 2 * pnorm(-abs(b$d / b$se)), BIC = b$bic)
}))
write.csv(s03_ml, file.path(out, "S03_arfima_ml_first_differences.csv"), row.names = FALSE)

## classical unit-root / stationarity tests and Lo's modified R/S
s03_ur <- do.call(rbind, lapply(seq_along(series), function(i) {
  x <- series[[i]]
  adf <- suppressWarnings(adf.test(x)); pp <- suppressWarnings(pp.test(x)); kp <- suppressWarnings(kpss.test(x, null = "Level"))
  rs <- modified_rs(x)
  data.frame(info[i, 1:2], ADF_stat = unname(adf$statistic), ADF_p = adf$p.value,
             PP_stat = unname(pp$statistic), PP_p = pp$p.value,
             KPSS_stat = unname(kp$statistic), KPSS_p = kp$p.value,
             Lo_modified_RS_V = rs$V, Lo_q = rs$q, Lo_reject_short_memory_5pct = rs$reject_5pct)
}))
write.csv(s03_ur, file.path(out, "S03_unit_root_stationarity_RS_tests.csv"), row.names = FALSE)

## ---------------------------------------------------------------- S05: spurious long memory
main <- subset(s03, Bandwidth == "n^0.6" & Estimator == "FELW")
s05_qu <- do.call(rbind, lapply(seq_along(series), function(i) {
  if (info$Series[i] == "Exchange Rate") return(NULL)      # covered by the first-difference row
  x <- series[[i]]
  d_felw <- main$d_hat[main$Country == info$Country[i] & main$Series == info$Series[i]]
  use_diff <- d_felw >= 0.5 && !grepl("first difference", info$Series[i])
  if (use_diff) x <- diff(x)
  m <- floor(length(x)^0.7)
  qt <- qu_test(x, m)
  data.frame(info[i, 1:2], Applied_to = ifelse(use_diff, "first difference", "level"), m = m,
             d_LW = qt$d_lw, Qu_W = qt$W, Crit_5pct = 1.155, Reject_true_long_memory_5pct = qt$reject_5pct)
}))
write.csv(s05_qu, file.path(out, "S05_qu_test.csv"), row.names = FALSE)

## rolling FELW estimates of d for the exchange-rate levels (120-month windows, step 6 months)
dates_all <- as.Date(read_all("Brazil")[[1]])
roll <- do.call(rbind, lapply(names(codes), function(cty) {
  x <- series[[which(info$Country == cty & info$Series == "Exchange Rate")]]
  w <- 120; ends <- seq(w, length(x), by = 6)
  do.call(rbind, lapply(ends, function(e) {
    f <- felw(x[(e - w + 1):e], floor(w^0.65))
    data.frame(Country = cty, Window_end = dates_all[e], d_hat = f$d, SE = f$se)
  }))
}))
write.csv(roll, file.path(out, "S05_rolling_felw_d.csv"), row.names = FALSE)
png(file.path(out, "S05_rolling_felw_d.png"), width = 2000, height = 1400, res = 220)
op <- par(mfrow = c(2, 2), mar = c(4, 4.2, 2.5, 1))
for (cty in names(codes)) {
  r <- roll[roll$Country == cty, ]
  plot(r$Window_end, r$d_hat, type = "l", lwd = 2, ylim = c(0, 2), xlab = "End of 120-month window",
       ylab = expression(hat(d)~"(FELW)"), main = cty)
  polygon(c(r$Window_end, rev(r$Window_end)), c(r$d_hat - 1.96 * r$SE, rev(r$d_hat + 1.96 * r$SE)),
          col = adjustcolor("steelblue", 0.25), border = NA)
  abline(h = c(0.5, 1), lty = c(3, 2), col = "grey30")
}
par(op); invisible(dev.off())

## Bai-Perron multiple break test in the mean of the first differences (trimming 15%, BIC)
s05_bp <- do.call(rbind, lapply(names(codes), function(cty) {
  dy <- diff(series[[which(info$Country == cty & info$Series == "Exchange Rate")]])
  bp <- breakpoints(dy ~ 1, h = 0.15)
  nb <- which.min(summary(bp)$RSS["BIC", ]) - 1
  bd <- if (nb > 0) format(dates_all[-1][breakpoints(bp, breaks = nb)$breakpoints], "%Y-%m") else character(0)
  data.frame(Country = cty, Breaks_BIC = nb, Break_dates = paste(bd, collapse = ", "))
}))
write.csv(s05_bp, file.path(out, "S05_bai_perron_first_differences.csv"), row.names = FALSE)

writeLines(capture.output(sessionInfo()), file.path(out, "R_sessionInfo.txt"))

## ------------------------------------------------------------------------------- console summary
cat("\nS02 (published Whittle table re-run): series at the H = 0.99 bound:",
    sum(s02$At_upper_bound_H_0.99[s02$Country != "Simulated"]), "of", sum(s02$Country != "Simulated"), "\n")
cat("\nS03 exchange rates, m = n^0.6:\n")
print(format(subset(s03, grepl("^Exchange", Series) & Bandwidth == "n^0.6")[, c("Country", "Series", "Estimator", "d_hat", "SE", "p_H0_d0", "p_H0_d1")], digits = 3), row.names = FALSE)
cat("\nARFIMA ML on first differences:\n"); print(format(s03_ml, digits = 3), row.names = FALSE)
cat("\nQu test:\n"); print(format(s05_qu, digits = 3), row.names = FALSE)
cat("\nBai-Perron:\n"); print(s05_bp, row.names = FALSE)
