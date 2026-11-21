################################################################################################
## MCB and MCS with the additional baselines (reviewer Comment 1)
##
##  * MCB (Figure 1 of the paper): tsutils::nemenyi(plottype = "vmcb") on the 24 country-horizon
##    cells, for RMSE, MAE, SMAPE and MAPE, exactly as Figure_01_MCB_BRIC.R but with the new models added.
##  * MCS (Table 13 of the paper): MCS::MCSprocedure on squared errors stacked over the six horizons,
##    statistic "TR", B = 5000, seed = 100, as Table_13_MCS_BRIC.R, at alpha = 0.05, 0.10, 0.15.
##    Before adding the new models, the published Table 13 is reproduced from the original forecasts.
##
##  Usage (from Supplementary_Analysis/):
##      Rscript significance_tests_extended.R                         # all additional baselines
##      Rscript significance_tests_extended.R --models=CatBoostx,TiDEx  # a chosen subset
##  Needs: tsutils, MCS, readxl. Input: results/additional_baselines/ (run the baseline scripts first).
################################################################################################
suppressPackageStartupMessages({ library(readxl); library(tsutils); library(MCS) })
cmd <- commandArgs(trailingOnly = FALSE)
here <- dirname(normalizePath(sub("^--file=", "", cmd[grep("^--file=", cmd)])))
repo <- dirname(here)
res_dir <- file.path(here, "results", "additional_baselines")
out <- file.path(here, "results", "significance_tests")
dir.create(out, recursive = TRUE, showWarnings = FALSE)

original <- c("Naïve", "AR", "ARIMAx", "ARNNx", "ARFIMAx", "ETS", "SETAR", "TBATS", "GARCH", "BSTSx", "DeepAR",
              "NBEATSx", "NHiTSx", "DLinearx", "NLinearx", "TSMixerx", "NARFIMA")
new_files <- c(CatBoostx = "CatBoost", LightGBMx = "LightGBM", TiDEx = "TiDE", GRUx = "GRU", LSTMx = "LSTM",
               `ARIMA-ANN` = "ARIMA-ANN", `ARFIMA-ANN` = "ARFIMA-ANN", EWNet = "EWNet")
sel <- grep("^--models=", cmd, value = TRUE)
new_models <- if (length(sel)) strsplit(sub("^--models=", "", sel), ",")[[1]] else names(new_files)
stopifnot(all(new_models %in% names(new_files)))
countries <- c("Brazil", "Russia", "India", "China")
horizons <- c(1, 3, 6, 12, 24, 48)
tag <- if (length(sel)) paste0("_subset_", length(new_models)) else "_all"

## ---------------------------------------------------------------------------------- MCB
long <- read.csv(file.path(res_dir, "tables", "all_models_metrics_long.csv"), check.names = FALSE, encoding = "UTF-8")
models <- c(setdiff(original, "NARFIMA"), new_models, "NARFIMA")
mcb_mat <- function(metric) {
  d <- long[long$Metric == metric & long$Model %in% models, ]
  w <- reshape(d[, c("Country", "Horizon", "Model", "Value")], idvar = c("Country", "Horizon"),
               timevar = "Model", direction = "wide")
  names(w) <- sub("^Value\\.", "", names(w))
  stopifnot(nrow(w) == 24)
  as.matrix(w[, models])
}
png(file.path(out, paste0("MCB_extended", tag, ".png")), width = 2600, height = 2600, res = 200)
par(mfrow = c(2, 2))
mcb_ranks <- list()
for (k in c("RMSE", "MAE", "SMAPE", "MAPE")) {
  nm <- tsutils::nemenyi(mcb_mat(k), plottype = "vmcb", main = paste("MCB:", k), conf.level = 0.95)
  mcb_ranks[[k]] <- data.frame(Metric = k, Model = names(nm$means), Mean_rank = unname(nm$means),
                               Lower = nm$intervals[1, names(nm$means)], Upper = nm$intervals[2, names(nm$means)])
}
invisible(dev.off())
mcb_ranks <- do.call(rbind, mcb_ranks)
write.csv(mcb_ranks, file.path(out, paste0("MCB_mean_ranks", tag, ".csv")), row.names = FALSE)

## ---------------------------------------------------------------------------------- MCS
sq_errors <- function(country, with_new) {
  data <- read_excel(file.path(repo, "Dataset", "Dataset_Selected_Exogenous", paste0(country, "_Data.xlsx")))
  y <- data[[2]]
  rows <- lapply(horizons, function(h) {
    f <- read.csv(file.path(repo, "Dataset", "Dataset_Model_Forecasts", country, paste0(country, " Forecast ", h, ".csv")),
                  check.names = FALSE)
    stopifnot(ncol(f) == 18)
    f <- f[, -1]; names(f) <- original                      # columns are in the published order (see header)
    if (with_new) for (m in new_models) {
      nf <- read.csv(file.path(res_dir, "forecasts", country, paste0(new_files[[m]], "_", h, ".csv")))
      f[[m]] <- nf$Forecast
    }
    test <- tail(y, h)
    (as.matrix(f) - test)^2
  })
  do.call(rbind, rows)
}
mcs_run <- function(L) {
  sapply(c(0.05, 0.10, 0.15), function(a) {
    r <- suppressWarnings(capture.output(m <- MCSprocedure(L, alpha = a, B = 5000, statistic = "TR", seed = 100)))
    paste(m@Info$included, collapse = ";")          # models kept in the superior set
  })
}
mcs <- list()
for (country in countries) {
  for (with_new in c(FALSE, TRUE)) {
    L <- sq_errors(country, with_new)
    keep <- mcs_run(L)
    mcs[[length(mcs) + 1]] <- data.frame(Country = country, Model_set = ifelse(with_new, "original + additional", "original (published)"),
                                         N_models = ncol(L), MCS_alpha_0.05 = keep[1], MCS_alpha_0.10 = keep[2], MCS_alpha_0.15 = keep[3])
    cat(sprintf("%-7s %-22s alpha=0.05: %s\n", country, ifelse(with_new, "original + additional", "original (published)"), keep[1]))
  }
}
mcs <- do.call(rbind, mcs)
write.csv(mcs, file.path(out, paste0("MCS_extended", tag, ".csv")), row.names = FALSE)

cat("\nMCB mean ranks (RMSE):\n")
print(head(mcb_ranks[mcb_ranks$Metric == "RMSE", ][order(mcb_ranks$Mean_rank[mcb_ranks$Metric == "RMSE"]), ], 10), row.names = FALSE)

## ------------------------------------------------------------------ LaTeX table of the MCS sets
tex_names <- function(x) {
  x <- strsplit(x, ";")[[1]]
  x <- gsub("ARIMA-ANN", "ARIMA--ANN", gsub("ARFIMA-ANN", "ARFIMA--ANN", x))
  x[x == "NARFIMA"] <- "\\textbf{NARFIMA}"
  paste(x, collapse = ", ")
}
ext <- mcs[mcs$Model_set == "original + additional", ]
tex <- c("\\begin{table}[!htbp]", "\\fontsize{7}{8}\\selectfont",
         paste0("\\caption{\\rev{Model confidence sets with the additional baselines (", ext$N_models[1],
                " models: 16 original baselines, ", ext$N_models[1] - 17, " additional baselines and NARFIMA). ",
                "Models remaining in the superior set $\\widehat{\\mathcal{A}}^*_{1-\\alpha}$ (squared loss, range statistic $T_R$, ",
                "$B = 5000$ bootstrap replications), as in Table~\\ref{tab:MCS_transposed_final}.}}"),
         "\\centering", "\\begin{tabular}{@{}l>{\\raggedright\\arraybackslash}p{4.5cm}>{\\raggedright\\arraybackslash}p{4.5cm}>{\\raggedright\\arraybackslash}p{4.5cm}@{}}", "\\hline",
         "Country & $\\alpha = 0.05$ & $\\alpha = 0.10$ & $\\alpha = 0.15$ \\\\ \\hline")
for (i in seq_len(nrow(ext))) tex <- c(tex, paste0(ext$Country[i], " & ", tex_names(ext$MCS_alpha_0.05[i]), " & ",
                                                    tex_names(ext$MCS_alpha_0.10[i]), " & ", tex_names(ext$MCS_alpha_0.15[i]), " \\\\"))
tex <- c(tex, "\\hline", "\\end{tabular}", "\\label{tab:MCS_extended}", "\\end{table}")
writeLines(tex, file.path(out, paste0("Table_MCS_extended", tag, ".tex")))
