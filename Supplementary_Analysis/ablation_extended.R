################################################################################################
## Extended ablation study (Section 6.2 / Figure 4 of the paper)
##
##  Published variants (Dataset/Dataset_Model_Evaluation/BRIC NARFIMA Variants Evaluation.xlsx, RMSE):
##    NARFIMA, NARIMA, NBSTS, NNaive (residual-feedback variants) and ARNNx (no residuals).
##  Added variant (Supplementary_Analysis/results/additional_baselines):
##    ARFIMAx-ANN  - NARFIMA's own ARFIMAx first stage + ANN forecast of its residuals, combined additively.
##    It differs from NARFIMA only in how the residuals are used, so the comparison stays like-for-like.
##    (ARIMA-ANN and ARFIMA-ANN are univariate benchmarks and are reported in the main comparison instead.)
##  Outputs (results/ablation_extended/): RMSE table, average ranks and the MCB plot (mean ranks and
##  intervals from tsutils::nemenyi, as in Figure_04_MCB_Ablation_Study), drawn in the layout of Figure 4.
##  Usage (from Supplementary_Analysis/): Rscript ablation_extended.R
################################################################################################
suppressPackageStartupMessages({ library(readxl); library(tsutils) })
cmd <- commandArgs(trailingOnly = FALSE)
here <- dirname(normalizePath(sub("^--file=", "", cmd[grep("^--file=", cmd)])))
repo <- dirname(here)
out <- file.path(here, "results", "ablation_extended")
dir.create(out, recursive = TRUE, showWarnings = FALSE)

v <- read_excel(file.path(repo, "Dataset", "Dataset_Model_Evaluation", "BRIC NARFIMA Variants Evaluation.xlsx"))
v <- as.data.frame(v[!is.na(v$Horizon), ])
for (i in seq_len(nrow(v))) if (is.na(v$Country[i])) v$Country[i] <- v$Country[i - 1]
names(v)[names(v) == "NNaïve"] <- "NNaive"
stopifnot(nrow(v) == 24)

m <- read.csv(file.path(here, "results", "additional_baselines", "metrics_additional_baselines.csv"))
add <- c("ARFIMAx-ANN")
for (a in add) {
  v[[a]] <- sapply(seq_len(nrow(v)), function(i) m$RMSE[m$Country == v$Country[i] & m$Horizon == v$Horizon[i] & m$MODEL == a])
}
## published ARFIMAx (no network) for reference
arf <- do.call(rbind, lapply(unique(v$Country), function(cn) {
  e <- read_excel(file.path(repo, "Dataset", "Dataset_Model_Evaluation", paste0(cn, " Evaluation.xlsx")))
  e <- as.data.frame(e); for (i in seq_len(nrow(e))) if (is.na(e$Horizon[i]) && i > 1) e$Horizon[i] <- e$Horizon[i - 1]
  r <- e[e$Metrics == "RMSE", c("Horizon", "ARFIMAx")]
  data.frame(Country = cn, Horizon = as.numeric(r$Horizon), ARFIMAx = as.numeric(r$ARFIMAx))
}))
v <- merge(v, arf, by = c("Country", "Horizon"), sort = FALSE)

models <- c("NARFIMA", "NARIMA", "NBSTS", "NNaive", "ARNNx", "ARFIMAx-ANN")
X <- as.matrix(v[, models])
ranks <- t(apply(X, 1, rank, ties.method = "min"))
summ <- data.frame(Model = models, Mean_rank = colMeans(ranks), Times_best = colSums(ranks == 1),
                   Median_RMSE_ratio_to_NARFIMA = apply(X, 2, function(col) median(col / X[, "NARFIMA"])))
summ <- summ[order(summ$Mean_rank), ]
write.csv(cbind(v[, c("Country", "Horizon")], X, ARFIMAx_published = v$ARFIMAx), file.path(out, "ablation_extended_rmse.csv"), row.names = FALSE)
write.csv(summ, file.path(out, "ablation_extended_ranks.csv"), row.names = FALSE)

nm <- tsutils::nemenyi(X, plottype = "none", conf.level = 0.95)
## tsutils draws MCB intervals as mean +/- CD/2 (nm$intervals holds mean +/- CD); two models differ
## significantly when their mean ranks differ by more than the critical distance CD (non-overlapping intervals)
mr <- sort(nm$means); half <- nm$cd / 2
iv <- rbind(mr - half, mr + half); colnames(iv) <- names(mr)
best_hi <- iv[2, 1]
lab <- sub("NNaive", "NNa\u00efve", names(mr))
png(file.path(out, "MCB_ablation_extended.png"), width = 3000, height = 820, res = 300)
op <- par(mar = c(3.2, 4.2, 0.6, 0.6), family = "serif", font = 2)
plot(seq_along(mr), mr, type = "n", xlim = c(0.4, length(mr) + 0.6), ylim = range(iv) + c(-0.15, 0.15),
     xaxt = "n", xlab = "", ylab = "Mean rank", las = 1, cex.lab = 1.1, font.lab = 2)
rect(par("usr")[1], iv[1, 1], par("usr")[2], best_hi, col = "grey88", border = NA)
for (j in seq_along(mr)) {
  segments(j, iv[1, j], j, iv[2, j], col = "steelblue", lwd = 2)
  points(c(j, j), iv[, j], pch = 16, cex = 0.6, col = "steelblue")
  points(j, mr[j], pch = 16, cex = 1.4, col = ifelse(mr[j] - mr[1] < nm$cd, "red", "black"))
}
mtext(sprintf("%s - %.2f", lab, mr), side = 1, at = seq_along(mr), line = 1, cex = 0.85, font = 2)   # every label shown
box(col = "grey40"); par(op); invisible(dev.off())
write.csv(data.frame(Model = names(mr), Mean_rank = unname(mr), Lower = iv[1, ], Upper = iv[2, ], CD = nm$cd,
                     Significantly_worse_than_best = mr - mr[1] >= nm$cd),
          file.path(out, "ablation_extended_mcb_intervals.csv"), row.names = FALSE)
print(summ, row.names = FALSE)
## check: the published five-model ablation (Figure 4 of the submission) is reproduced from the same file
nm5 <- tsutils::nemenyi(as.matrix(v[, c("NARFIMA", "NARIMA", "NBSTS", "NNaive", "ARNNx")]), plottype = "none")
cat("Published 5-model mean ranks reproduced:", paste(sprintf("%s %.2f", names(sort(nm5$means)), sort(nm5$means)), collapse = "; "), "\n")
cat("\nARFIMAx-ANN vs published ARFIMAx (RMSE ratio, median):", round(median(v[["ARFIMAx-ANN"]] / v$ARFIMAx), 3), "\n")
