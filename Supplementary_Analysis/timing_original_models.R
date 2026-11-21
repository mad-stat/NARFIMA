################################################################################################
## Computational cost of the published R models (reviewer Comment 1)
##
##  Re-runs the model calls of Table_06-09_Evaluation_Comparison_*.R only to record wall-clock time
##  (fit + forecast) and the number of estimated parameters; the published accuracy figures are not
##  touched. Models: Naive, AR, ARIMAx, ARNNx, ARFIMAx, ETS, SETAR, TBATS, GARCH, BSTSx and NARFIMA
##  (published (p, q, k, skip) per country and horizon, 1000-network ensemble).
##  Also: NARFIMA accuracy and cost against the ensemble size R = 1, 10, 50, 100, 500, 1000
##  (RMSE relative to R = 1000 in the same run).
##
##  Usage (from Supplementary_Analysis/):  Rscript timing_original_models.R
##  Needs: forecast, nnet, tsDyn, tseries, rugarch, bsts, readxl, Metrics; Code/NARFIMA.R from the repository.
################################################################################################
suppressPackageStartupMessages({
  library(forecast); library(tsDyn); library(tseries); library(rugarch); library(bsts); library(readxl); library(Metrics)
})
cmd <- commandArgs(trailingOnly = FALSE)
here <- dirname(normalizePath(sub("^--file=", "", cmd[grep("^--file=", cmd)])))
repo <- dirname(here)
source(file.path(repo, "Code", "NARFIMA.R"))
out <- file.path(here, "results", "computational_cost")
dir.create(out, recursive = TRUE, showWarnings = FALSE)

countries <- c("Brazil", "Russia", "India", "China")
horizons <- c(1, 3, 6, 12, 24, 48)
## published NARFIMA (p, q, k, skip) - manuscript Table 5 / Table_06-09 scripts
spec <- list(
  Brazil = list(`1` = c(5, 4, 4, 0), `3` = c(2, 5, 2, 0), `6` = c(1, 2, 1, 0), `12` = c(1, 1, 1, 0), `24` = c(2, 5, 1, 0), `48` = c(4, 2, 1, 1)),
  Russia = list(`1` = c(1, 2, 2, 1), `3` = c(1, 1, 1, 0), `6` = c(1, 1, 1, 0), `12` = c(5, 2, 5, 1), `24` = c(1, 1, 5, 1), `48` = c(3, 2, 1, 0)),
  India  = list(`1` = c(2, 1, 1, 1), `3` = c(4, 3, 4, 1), `6` = c(4, 2, 1, 1), `12` = c(1, 3, 4, 1), `24` = c(5, 4, 1, 1), `48` = c(2, 4, 4, 1)),
  China  = list(`1` = c(5, 4, 1, 1), `3` = c(1, 2, 1, 1), `6` = c(1, 1, 2, 0), `12` = c(5, 4, 1, 1), `24` = c(1, 2, 4, 1), `48` = c(4, 1, 2, 1)))

timeit <- function(expr) {
  t0 <- proc.time()[["elapsed"]]
  val <- force(expr)
  list(value = val, sec = proc.time()[["elapsed"]] - t0)
}
nnet_params <- function(models) length(models[[1]]$wts) * length(models)

rows <- list(); ens <- list()
for (country in countries) {
  data <- read_excel(file.path(repo, "Dataset", "Dataset_Selected_Exogenous", paste0(country, "_Data.xlsx")))
  y <- ts(data[[2]])
  reg <- as.matrix(data[, c(4, 3, 5, 6, 7)])                 # as in the Table_06-09 scripts
  for (h in horizons) {
    train <- subset(y, end = length(y) - h); test <- subset(y, start = length(y) - h + 1)
    xtr <- reg[1:length(train), ]
    xte <- reg[1:h, , drop = FALSE]; colnames(xte) <- colnames(xtr)  # same construction as the scripts
    rec <- function(model, t, k) rows[[length(rows) + 1]] <<- data.frame(Country = country, Horizon = h, Model = model,
                                                                        Seconds_total = round(t, 3), N_params = k)
    set.seed(100)
    t <- timeit({ f <- naive(train, h = h) }); rec("Naïve", t$sec, 0)
    t <- timeit({ m <- ar(train); predict(m, n.ahead = h) }); rec("AR", t$sec, length(ar(train)$ar) + 1)
    t <- timeit({ m <- auto.arima(train, xreg = xtr); forecast(m, h = h, xreg = xte) }); rec("ARIMAx", t$sec, length(coef(auto.arima(train, xreg = xtr))))
    set.seed(100)
    t <- timeit({ m <- nnetar(train, xreg = xtr); forecast(m, h = h, xreg = xte) })
    m <- nnetar(train, xreg = xtr); rec("ARNNx", t$sec, nnet_params(m$model))
    t <- timeit({ m <- arfima(train, xreg = xtr); forecast(m, h = h, xreg = xte) })
    m_af <- arfima(train, xreg = xtr); rec("ARFIMAx", t$sec, length(coef(m_af)) + ncol(xtr))
    t <- timeit({ m <- ets(train); forecast(m, h = h) }); rec("ETS", t$sec, length(ets(train)$par))
    t <- timeit({ m <- setar(train, m = 4); predict(m, n.ahead = h) }); rec("SETAR", t$sec, length(coef(setar(train, m = 4))))
    t <- timeit({ m <- tbats(train); forecast(m, h = h) }); rec("TBATS", t$sec, NA)
    t <- timeit({
      sp <- ugarchspec(variance.model = list(garchOrder = c(1, 1)), mean.model = list(armaOrder = c(0, 0)))
      fit <- ugarchfit(sp, data = train); ugarchforecast(fit, n.ahead = h) })
    rec("GARCH", t$sec, 4)
    t <- timeit({
      ss <- AddSemilocalLinearTrend(list(), train); ss <- AddSeasonal(ss, train, nseasons = 12)
      b <- bsts(train ~ ., state.specification = ss, niter = 1000, ping = 0, seed = 100, data = cbind(xtr, train))
      predict(b, horizon = h, burn = SuggestBurn(.1, b), newdata = xte) })
    rec("BSTSx", t$sec, NA)
    ## NARFIMA: ARFIMAx first stage + 1000-network ensemble, published specification
    s <- spec[[country]][[as.character(h)]]
    t <- timeit({
      af <- arfima(train, xreg = xtr); er <- residuals(af); er[is.na(er)] <- 0
      set.seed(100)
      nm <- auto_narfima(train, er, p = s[1], q = s[2], size = s[3], lambda = 0, lambdae = 0, repeats = 1000,
                         skip = as.logical(s[4]), xreg = xtr)
      list(model = nm, fc = forecast_narfima(nm, PI = FALSE, h = h, xreg = xte)) })
    nm <- t$value$model
    rec("NARFIMA", t$sec, nnet_params(nm$model) + length(coef(m_af)) + ncol(xtr))
    ## ensemble size: refit with R networks (same seed, so networks 1..R are shared)
    rmse_full <- rmse(as.numeric(test), as.numeric(t$value$fc$mean))
    for (R in c(1, 10, 50, 100, 500, 1000)) {
      tt <- timeit({
        af <- arfima(train, xreg = xtr); er <- residuals(af); er[is.na(er)] <- 0
        set.seed(100)
        nr <- auto_narfima(train, er, p = s[1], q = s[2], size = s[3], lambda = 0, lambdae = 0, repeats = R,
                           skip = as.logical(s[4]), xreg = xtr)
        forecast_narfima(nr, PI = FALSE, h = h, xreg = xte) })
      ens[[length(ens) + 1]] <- data.frame(Country = country, Horizon = h, R = R, Seconds_total = round(tt$sec, 3),
                                           RMSE = rmse(as.numeric(test), as.numeric(tt$value$mean)),
                                           RMSE_relative_to_R1000 = rmse(as.numeric(test), as.numeric(tt$value$mean)) / rmse_full)
    }
    cat(sprintf("%-7s h=%2d done (NARFIMA %.1fs, BSTSx %.1fs)\n", country, h, t$sec,
                rows[[length(rows) - 1]]$Seconds_total))
  }
}
write.csv(do.call(rbind, rows), file.path(out, "timing_original_R_models.csv"), row.names = FALSE)
write.csv(do.call(rbind, ens), file.path(out, "narfima_ensemble_size.csv"), row.names = FALSE)
writeLines(c(capture.output(sessionInfo()), "", paste("CPU:", system("sysctl -n machdep.cpu.brand_string", intern = TRUE)),
             paste("Cores:", parallel::detectCores()),
             paste("RAM (GB):", round(as.numeric(system("sysctl -n hw.memsize", intern = TRUE)) / 2^30))),
           file.path(out, "R_sessionInfo_and_hardware.txt"))
