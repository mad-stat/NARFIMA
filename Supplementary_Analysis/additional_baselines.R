################################################################################################
## Additional baselines requested by the SNDE reviewer (Comment 1) - R part
##
##  (1) Hybrid baselines
##      * ARIMA-ANN  (Zhang, 2003): y_hat = ARIMA forecast + ANN forecast of the ARIMA residuals
##      * ARFIMA-ANN              : y_hat = ARFIMA forecast + ANN forecast of the ARFIMA residuals
##      As in their original formulations, both hybrids are univariate. The linear stage is
##      forecast::auto.arima() / forecast::arfima(); the ANN stage is forecast::nnetar() fitted to the
##      in-sample residuals (lag order and hidden size chosen by nnetar's defaults), averaged over
##      20 networks x 5 seeds.
##  (1b) ARFIMAx-ANN (ablation variant): NARFIMA's own first stage, forecast::arfima(train, xreg = covariates),
##      plus an ANN forecast of its residuals (same NNAR set-up as the hybrids above), combined additively.
##      It differs from NARFIMA only in how the residuals are used (added vs fed to the network with lags
##      and covariates). Covariates, including the test-period matrix reg[1:h, ], are built exactly as in
##      Table_06-09 and Ablation_Study.R, so the linear stage is the published ARFIMAx/NARFIMA first stage.
##  (2) EWNet (Panja et al., 2023; ensemble wavelet neural network). The functions are taken unchanged
##      from github.com/mad-stat/Epicasting/blob/main/Models/EWNet.R (Haar MODWT with floor(log(n))
##      levels and periodic boundary; nnetar with p lags and 500 repeats per component; forecasts summed;
##      p in 1..5 chosen by validation MASE as in ewnet_val()). Validation window = last max(h, 12)
##      training months (common protocol of all additional baselines). Univariate, as published.
##  (3) Evaluation of ALL additional baselines (these plus CatBoost, LightGBM, TiDE, GRU, LSTM from
##      additional_baselines.py) with exactly the same evaluate() function as Table_06-09 scripts.
##
##  Splits: identical to Tables 6-9 (training = first 322 - h months, test = last h months).
##  Usage (from Supplementary_Analysis/):
##      Rscript additional_baselines.R                          # all R models, then evaluate every forecast file
##      Rscript additional_baselines.R --models=EWNet            # selected R models only (comma-separated)
##      Rscript additional_baselines.R --evaluate-only
##  Needs: forecast, nnet, Metrics, readxl, wavelets (EWNet), parallel (base).
################################################################################################

suppressPackageStartupMessages({
  library(forecast)
  library(readxl)
  library(Metrics)
})

cmd <- commandArgs(trailingOnly = FALSE)
script_dir <- dirname(normalizePath(sub("^--file=", "", cmd[grep("^--file=", cmd)])))
evaluate_only <- "--evaluate-only" %in% cmd
r_models <- c("ARIMA-ANN", "ARFIMA-ANN", "EWNet", "ARFIMAx-ANN")
sel <- grep("^--models=", cmd, value = TRUE)
if (length(sel)) r_models <- strsplit(sub("^--models=", "", sel), ",")[[1]]
repo <- dirname(script_dir)
data_dir <- file.path(repo, "Dataset", "Dataset_Selected_Exogenous")
out_dir <- file.path(script_dir, "results", "additional_baselines")
dir.create(file.path(out_dir, "forecasts"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(out_dir, "logs"), recursive = TRUE, showWarnings = FALSE)

countries <- c("Brazil", "Russia", "India", "China")
horizons <- c(1, 3, 6, 12, 24, 48)
seeds <- 100:104
nn_repeats <- 20
all_models <- c("CatBoost", "LightGBM", "TiDE", "GRU", "LSTM", "ARIMA-ANN", "ARFIMA-ANN", "EWNet", "ARFIMAx-ANN")

## Same evaluation function as in Table_06-09_Evaluation_Comparison_*.R
evaluate <- function(test, pred, model) {
  data.frame(MODEL = model,
             MAPE = mape(test, pred) * 100,
             SMAPE = smape(test, pred),
             MAE = mae(test, pred),
             MASE = mase(test, pred),
             RMSE = rmse(test, pred))
}

load_country <- function(country) {
  d <- read_excel(file.path(data_dir, paste0(country, "_Data.xlsx")))
  list(date = as.Date(d[[1]]), y = ts(d[[2]]),
       reg = as.matrix(d[, c(4, 3, 5, 6, 7)]))                 # covariate columns as in Table_06-09 and Ablation_Study.R
}

elapsed <- function() proc.time()[["elapsed"]]

## ------------------------------------------------------------------------------------- hybrids
fit_hybrid <- function(train, h, first_stage) {
  t0 <- elapsed()
  fit1 <- if (first_stage == "ARIMA") auto.arima(train) else arfima(train)
  linear_fc <- as.numeric(forecast(fit1, h = h)$mean)
  res <- residuals(fit1)
  res[is.na(res)] <- 0
  t1 <- elapsed()
  nn_fc <- sapply(seeds, function(s) {
    set.seed(s)
    nn <- nnetar(res, repeats = nn_repeats)
    as.numeric(forecast(nn, h = h)$mean)
  })
  nn_fc <- matrix(nn_fc, nrow = h)
  t2 <- elapsed()
  set.seed(seeds[1])
  nn1 <- nnetar(res, repeats = nn_repeats)
  spec_linear <- if (first_stage == "ARIMA") {
    paste0("ARIMA(", paste(arimaorder(fit1), collapse = ","), ")")
  } else {
    sprintf("ARFIMA(%d,%.3f,%d)", length(fit1$ar), fit1$d, length(fit1$ma))
  }
  list(forecast = linear_fc + rowMeans(nn_fc),
       linear = linear_fc, nn = rowMeans(nn_fc),
       config = sprintf("%s + NNAR(%d,%d) on residuals; %d nets x %d seeds",
                        spec_linear, nn1$p, nn1$size, nn_repeats, length(seeds)),
       n_params = length(nn1$model[[1]]$wts) * nn_repeats * length(seeds) +
         length(coef(fit1)),
       t_linear = t1 - t0, t_nn = t2 - t1)
}

## ------------------------------------------------------------------------------------- EWNet
## Verbatim algorithm of WaveletFitting(), WaveletFittingnar() and ewnet_val() from
## github.com/mad-stat/Epicasting/blob/main/Models/EWNet.R (the source file cannot be source()d as is,
## because of a stray "@export" line). Only bookkeeping (seed, timing, returned fields) is added.
WaveletFitting <- function(ts, Wvlevels, bndry, FFlag) {
  mraout <- wavelets::modwt(ts, filter = "haar", n.levels = Wvlevels, boundary = bndry, fast = FFlag)
  WaveletSeries <- cbind(do.call(cbind, mraout@W), mraout@V[[Wvlevels]])
  list(WaveletSeries = WaveletSeries, WVSeries = mraout)
}

WaveletFittingnar <- function(ts, Waveletlevels, MaxARParam, boundary, FastFlag, NForecast) {
  WS <- WaveletFitting(ts = ts, Wvlevels = Waveletlevels, bndry = boundary, FFlag = FastFlag)$WaveletSeries
  AllWaveletForecast <- NULL
  n_weights <- 0
  for (WVLevel in 1:ncol(WS)) {
    WaveletNARFit <- forecast::nnetar(y = as.ts(WS[, WVLevel]), p = MaxARParam, repeats = 500)
    WaveletNARForecast <- forecast::forecast(WaveletNARFit, h = NForecast)
    AllWaveletForecast <- cbind(AllWaveletForecast, as.matrix(WaveletNARForecast$mean))
    n_weights <- n_weights + length(WaveletNARFit$model[[1]]$wts) * 500
  }
  list(Finalforecast = rowSums(AllWaveletForecast, na.rm = TRUE), n_components = ncol(WS), n_weights = n_weights)
}

fit_ewnet <- function(train, h, max_p = 5, seed = seeds[1]) {
  t0 <- elapsed()
  n_val <- max(h, 12)
  train_val <- subset(ts(train), end = length(train) - n_val)
  val <- subset(ts(train), start = length(train) - n_val + 1)
  val_mase <- sapply(1:max_p, function(p) {
    set.seed(seed)
    f <- WaveletFittingnar(ts(train_val), Waveletlevels = floor(log(length(train_val))), boundary = "periodic",
                           FastFlag = TRUE, MaxARParam = p, NForecast = n_val)$Finalforecast
    Metrics::mase(as.numeric(val), f)                         # selection criterion of ewnet_val()
  })
  p_best <- which.min(val_mase)
  t1 <- elapsed()
  set.seed(seed)
  fit <- WaveletFittingnar(ts(train), Waveletlevels = floor(log(length(train))), boundary = "periodic",
                           FastFlag = TRUE, MaxARParam = p_best, NForecast = h)
  t2 <- elapsed()
  list(forecast = as.numeric(fit$Finalforecast),
       config = sprintf("EWNet: Haar MODWT, %d levels (+1 smooth), periodic; NNAR(p=%d) x 500 repeats per component; p chosen from 1..%d by validation MASE (%d months)",
                        floor(log(length(train))), p_best, max_p, n_val),
       n_params = fit$n_weights, t_select = t1 - t0, t_final = t2 - t1)
}

if (!evaluate_only && any(c("ARIMA-ANN", "ARFIMA-ANN") %in% r_models)) {
  timing <- list()
  for (country in countries) {
    cd <- load_country(country)
    dir.create(file.path(out_dir, "forecasts", country), showWarnings = FALSE)
    for (h in horizons) {
      train <- subset(cd$y, end = length(cd$y) - h)           # as in Table_06-09
      test <- subset(cd$y, start = length(cd$y) - h + 1)
      test_dates <- tail(cd$date, h)
      for (fs in sub("-ANN$", "", intersect(c("ARIMA-ANN", "ARFIMA-ANN"), r_models))) {
        name <- paste0(fs, "-ANN")
        r <- fit_hybrid(train, h, fs)
        write.csv(data.frame(Date = format(test_dates), Actual = as.numeric(test), Forecast = r$forecast,
                             Linear_component = r$linear, ANN_component = r$nn),
                  file.path(out_dir, "forecasts", country, paste0(name, "_", h, ".csv")), row.names = FALSE)
        timing[[length(timing) + 1]] <- data.frame(
          Country = country, Horizon = h, Model = name, Config = r$config,
          Seconds_linear_stage = round(r$t_linear, 3), Seconds_ANN_stage = round(r$t_nn, 3),
          Seconds_total = round(r$t_linear + r$t_nn, 3), N_params = r$n_params,
          Test_RMSE = rmse(as.numeric(test), r$forecast))
        cat(sprintf("%-7s h=%2d %-10s RMSE=%.4f  %s  (%.1fs)\n", country, h, name,
                    rmse(as.numeric(test), r$forecast), r$config, r$t_linear + r$t_nn))
      }
    }
  }
  write.csv(do.call(rbind, timing), file.path(out_dir, "logs", "run_R_hybrids.csv"), row.names = FALSE)
  writeLines(capture.output(sessionInfo()), file.path(out_dir, "R_sessionInfo.txt"))
}

if (!evaluate_only && "EWNet" %in% r_models) {
  run_country <- function(country) {
    cd <- load_country(country)
    dir.create(file.path(out_dir, "forecasts", country), showWarnings = FALSE)
    rows <- list()
    for (h in horizons) {
      train <- subset(cd$y, end = length(cd$y) - h)
      test <- subset(cd$y, start = length(cd$y) - h + 1)
      r <- fit_ewnet(train, h)
      write.csv(data.frame(Date = format(tail(cd$date, h)), Actual = as.numeric(test), Forecast = r$forecast),
                file.path(out_dir, "forecasts", country, paste0("EWNet_", h, ".csv")), row.names = FALSE)
      rows[[length(rows) + 1]] <- data.frame(
        Country = country, Horizon = h, Model = "EWNet", Config = r$config,
        Seconds_linear_stage = NA, Seconds_ANN_stage = round(r$t_select + r$t_final, 3),
        Seconds_total = round(r$t_select + r$t_final, 3), N_params = r$n_params,
        Test_RMSE = rmse(as.numeric(test), r$forecast))
      cat(sprintf("%-7s h=%2d EWNet RMSE=%.4f  %s  (%.1fs)\n", country, h,
                  rmse(as.numeric(test), r$forecast), r$config, r$t_select + r$t_final))
    }
    do.call(rbind, rows)
  }
  ew <- parallel::mclapply(countries, run_country, mc.cores = length(countries), mc.preschedule = FALSE)
  bad <- vapply(ew, inherits, logical(1), what = "try-error")
  if (any(bad)) stop(paste(sapply(ew[bad], as.character), collapse = "\n"))
  write.csv(do.call(rbind, ew), file.path(out_dir, "logs", "run_R_EWNet.csv"), row.names = FALSE)
  writeLines(capture.output(sessionInfo()), file.path(out_dir, "R_sessionInfo.txt"))
}

## ------------------------------------------------------------------------- ARFIMAx-ANN (ablation)
if (!evaluate_only && "ARFIMAx-ANN" %in% r_models) {
  rows <- list()
  for (country in countries) {
    cd <- load_country(country)
    dir.create(file.path(out_dir, "forecasts", country), showWarnings = FALSE)
    for (h in horizons) {
      train <- subset(cd$y, end = length(cd$y) - h); test <- subset(cd$y, start = length(cd$y) - h + 1)
      xtr <- cd$reg[1:length(train), ]; xte <- cd$reg[1:h, , drop = FALSE]; colnames(xte) <- colnames(xtr)
      t0 <- elapsed()
      fit1 <- arfima(train, xreg = xtr)
      lin <- as.numeric(forecast(fit1, h = h, xreg = xte)$mean)
      res <- residuals(fit1); res[is.na(res)] <- 0
      nn <- sapply(seeds, function(s) { set.seed(s); as.numeric(forecast(nnetar(res, repeats = nn_repeats), h = h)$mean) })
      nn <- rowMeans(matrix(nn, nrow = h))
      t1 <- elapsed()
      set.seed(seeds[1]); nn1 <- nnetar(res, repeats = nn_repeats)
      cfg <- sprintf("ARFIMAx(%d,%.3f,%d) + NNAR(%d,%d) on residuals; %d nets x %d seeds",
                     length(fit1$ar), fit1$d, length(fit1$ma), nn1$p, nn1$size, nn_repeats, length(seeds))
      write.csv(data.frame(Date = format(tail(cd$date, h)), Actual = as.numeric(test), Forecast = lin + nn,
                           Linear_component = lin, ANN_component = nn),
                file.path(out_dir, "forecasts", country, paste0("ARFIMAx-ANN_", h, ".csv")), row.names = FALSE)
      rows[[length(rows) + 1]] <- data.frame(Country = country, Horizon = h, Model = "ARFIMAx-ANN", Config = cfg,
        Seconds_linear_stage = NA, Seconds_ANN_stage = NA, Seconds_total = round(t1 - t0, 3),
        N_params = length(nn1$model[[1]]$wts) * nn_repeats * length(seeds) + length(coef(fit1)) + ncol(xtr),
        Test_RMSE = rmse(as.numeric(test), lin + nn))
      cat(sprintf("%-7s h=%2d ARFIMAx-ANN RMSE=%.4f (ARFIMAx alone %.4f)  %s\n", country, h,
                  rmse(as.numeric(test), lin + nn), rmse(as.numeric(test), lin), cfg))
    }
  }
  write.csv(do.call(rbind, rows), file.path(out_dir, "logs", "run_R_ARFIMAx-ANN.csv"), row.names = FALSE)
}

## ------------------------------------------------------------------- evaluation of all models
metrics <- list()
missing <- character(0)
for (country in countries) {
  cd <- load_country(country)
  for (h in horizons) {
    test <- as.numeric(subset(cd$y, start = length(cd$y) - h + 1))
    for (m in all_models) {
      f <- file.path(out_dir, "forecasts", country, paste0(m, "_", h, ".csv"))
      if (!file.exists(f)) { missing <- c(missing, f); next }
      fc <- read.csv(f)
      stopifnot(nrow(fc) == h, max(abs(fc$Actual - test)) < 1e-8)  # forecast file aligned with the test window
      metrics[[length(metrics) + 1]] <- cbind(Country = country, Horizon = h, evaluate(test, fc$Forecast, m))
    }
  }
}
metrics <- do.call(rbind, metrics)
write.csv(metrics, file.path(out_dir, "metrics_additional_baselines.csv"), row.names = FALSE)
cat(sprintf("\nEvaluated %d model-country-horizon cells; %d forecast files missing.\n", nrow(metrics), length(missing)))
if (length(missing)) cat(head(missing, 10), sep = "\n")
