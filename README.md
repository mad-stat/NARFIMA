# NARFIMA: Neural ARFIMA for Forecasting BRIC Exchange Rates with Long Memory

This repository holds the data, code and forecasts for the paper *Neural ARFIMA for Forecasting BRIC Exchange Rates with Long Memory*. It has been public since March 2025.

## R package `narfima` (CRAN)

The method is released as the R package **`narfima`** on CRAN:
* Version 0.1.0, published 21 September 2025.
* DOI [10.32614/CRAN.package.narfima](https://doi.org/10.32614/CRAN.package.narfima).
* Listed in the CRAN Time Series task view.

The package implements NARFIMA and the residual-feedback variants used in the ablation study:

| Function | Model |
|---|---|
| `auto_narfima()` | NARFIMA |
| `auto_narima()` | NARIMA |
| `auto_nbsts()` | NBSTS |
| `auto_nnaive()` | NNaive |
| `forecast_narfima_class()` | Point forecasts and prediction intervals |

```r
install.packages("narfima")
library(narfima)
h <- 3
data  <- EuStockMarkets[(nrow(EuStockMarkets) - 83):nrow(EuStockMarkets), 3]
train <- data[1:(length(data) - h)]
narfima_model    <- auto_narfima(train)
narfima_forecast <- forecast_narfima_class(narfima_model, h)
```

Package page: <https://CRAN.R-project.org/package=narfima>. Preprint: <https://doi.org/10.48550/arXiv.2509.06697>.

## Repository layout

| Folder or file | Content |
|---|---|
| `Dataset/` | Raw data with all candidate covariates (`Dataset_All_Exogenous`) and the five selected covariates (`Dataset_Selected_Exogenous`); forecasts of all models (`Dataset_Model_Forecasts`, `Dataset_Deep_Learning_Models_Forecasts`); evaluation summaries and MCB inputs |
| `Code/NARFIMA.R` | NARFIMA implementation used for the paper |
| `Code/Deep_Learning.ipynb` | Deep-learning baselines (Darts) |
| `Table_*.R`, `Figure_*.R`, `Ablation_Study.R` | One script per table or figure of the paper (see the list below) |
| `Supplementary_Analysis/` | Analyses added in the revision for *Studies in Nonlinear Dynamics & Econometrics* (see below) |
| `R_package_version.txt`, `Python_Package_Version.txt` | Package versions used for the original results |

## Revision for Studies in Nonlinear Dynamics & Econometrics: supplementary analyses

All code and outputs added in the revision are in [`Supplementary_Analysis/`](Supplementary_Analysis/README.md). Each analysis has its own entry script, and the README in that folder explains how to run it and maps each table and figure of the revised paper to its script and output.

1. **Additional baselines** (`additional_baselines.py`, `additional_baselines.R`):
   * CatBoost and LightGBM (gradient boosting);
   * TiDE (deep neural network);
   * GRU and LSTM (recurrent networks);
   * ARIMA–ANN, ARFIMA–ANN and EWNet (hybrids).

   They use the same splits, horizons and evaluation function as Tables 6–9 of the original submission (Tables 7–10 of the revised paper). Comparison tables are built by `make_comparison_tables.py`.
2. **MCB and MCS with the additional baselines** (`significance_tests_extended.R`).
3. **Long-memory analysis** (`long_memory_analysis.R`, `long_memory_montecarlo.R`, `R/long_memory_estimators.R`):
   * GPH, bias-reduced GPH, local Whittle, exact and feasible exact local Whittle estimates;
   * tests of d = 0, 1/2 and 1;
   * Qu's test against spurious long memory, rolling estimates and Bai–Perron breaks;
   * a Monte Carlo check of all estimators;
   * a re-run of the Whittle table of the original submission.
4. **Computational cost** (`timing_original_models.R`, `timing_original_dl.py`, `make_cost_tables.py`): wall-clock time and parameter counts for every model, and NARFIMA accuracy against ensemble size.
5. **Citation check** (`citation_check.py`): DOI and metadata verification of every new reference.

Pinned versions: `Supplementary_Analysis/requirements.txt` (Python) and the `sessionInfo` files in `Supplementary_Analysis/results/` (R).

## Data sources

Spot exchange rates and macroeconomic indicators come from FRED (<https://fred.stlouisfed.org>). The uncertainty indices come from <https://www.policyuncertainty.com>. The sample covers January 1997 to October 2023 at monthly frequency.

<!-- TODO (authors): add the FRED series identifiers and the download date of each series here. -->

---

# Reproducibility Checks: NARFIMA

The table and figure numbers in this list are those of the original submission, which the script names also follow. For the numbering of the revised paper, see the map in [`Supplementary_Analysis/README.md`](Supplementary_Analysis/README.md).

* To view the full list of packages (both R and Python) along with their respective versions, please refer to the following files [R_package_version.txt](https://github.com/mad-stat/NARFIMA/blob/main/R_package_version.txt) and [Python_Package_Version.txt](https://github.com/mad-stat/NARFIMA/blob/main/Python_Package_Version.txt).

* To view "Table 1: Time plots of the training data for exchange rates of BRIC nations, alongside the ACF plots and OLS-based CUSUM test results.", please refer to [Table_01_TS_ACF_OLS_CUSUM_BRIC.R](https://github.com/mad-stat/NARFIMA/blob/main/Table_01_TS_ACF_OLS_CUSUM_BRIC.R).

* To view "Table 2: Nonlinear Granger causality test results assessing the influence of exogenous covariates on exchange rates in BRIC countries.", please refer to [Table_02_Causality_Analysis_BRIC.R](https://github.com/mad-stat/NARFIMA/blob/main/Table_02_Causality_Analysis_BRIC.R).

* To view "Table 5: Assumptions (3) and (5) for the NARFIMA model. Reported values are median weight estimates and the associated assumption conditions, obtained from $1000$ bootstrap iterations ($100$ internal ensemble fits per iteration). Long horizons with $skip = \text{FALSE}$ are omitted, as skip connections were removed.", please refer to [Table_05_Empirical_Validation_BRIC.R](https://github.com/mad-stat/NARFIMA/blob/main/Table_05_Empirical_Validation_BRIC.R).

* To generate "Table 6: Evaluation of the proposed NARFIMA model’s performance relative to baseline forecasters across all forecast horizons for Brazil (**`best`** and ***`second-best`*** results are highlighted).", please refer to [Table_06_Evaluation_Comparison_Brazil.R](https://github.com/mad-stat/NARFIMA/blob/main/Table_06_Evaluation_Comparison_Brazil.R). To check the forecasts across various algorithms, please refer to the following folder [Brazil Forecasts](https://github.com/mad-stat/NARFIMA/tree/main/Dataset/Dataset_Model_Forecasts/Brazil). The performance evaluation summary can be found in [Brazil Evaluation](https://github.com/mad-stat/NARFIMA/blob/main/Dataset/Dataset_Model_Evaluation/Brazil%20Evaluation.xlsx).

* To generate "Table 7: Evaluation of the proposed NARFIMA model’s performance relative to baseline forecasters across all forecast horizons for Russia (**`best`** and ***`second-best`*** results are highlighted).", please refer to [Table_07_Evaluation_Comparison_Russia.R](https://github.com/mad-stat/NARFIMA/blob/main/Table_07_Evaluation_Comparison_Russia.R). To check the forecasts across various algorithms, please refer to the following folder [Russia Forecasts](https://github.com/mad-stat/NARFIMA/tree/main/Dataset/Dataset_Model_Forecasts/Russia). The performance evaluation summary can be found in [Russia Evaluation](https://github.com/mad-stat/NARFIMA/blob/main/Dataset/Dataset_Model_Evaluation/Russia%20Evaluation.xlsx).

* To generate "Table 8: Evaluation of the proposed NARFIMA model’s performance relative to baseline forecasters across all forecast horizons for India (**`best`** and ***`second-best`*** results are highlighted).", please refer to [Table_08_Evaluation_Comparison_India.R](https://github.com/mad-stat/NARFIMA/blob/main/Table_08_Evaluation_Comparison_India.R). To check the forecasts across various algorithms, please refer to the following folder [India Forecasts](https://github.com/mad-stat/NARFIMA/tree/main/Dataset/Dataset_Model_Forecasts/India). The performance evaluation summary can be found in [India Evaluation](https://github.com/mad-stat/NARFIMA/blob/main/Dataset/Dataset_Model_Evaluation/India%20Evaluation.xlsx).

* To generate "Table 9: Evaluation of the proposed NARFIMA model’s performance relative to baseline forecasters across all forecast horizons for China (**`best`** and ***`second-best`*** results are highlighted).", please refer to [Table_09_Evaluation_Comparison_China.R](https://github.com/mad-stat/NARFIMA/blob/main/Table_09_Evaluation_Comparison_China.R). To check the forecasts across various algorithms, please refer to the following folder [China Forecasts](https://github.com/mad-stat/NARFIMA/tree/main/Dataset/Dataset_Model_Forecasts/China). The performance evaluation summary can be found in [China Evaluation](https://github.com/mad-stat/NARFIMA/blob/main/Dataset/Dataset_Model_Evaluation/China%20Evaluation.xlsx).

* To view "Figure 1: Multiple comparisons with the best (MCB) plot for BRIC nations based on (a) RMSE, (b) MAE, (c) SMAPE, and (d) MAPE metrics. In the plots, ‘NARFIMA-1.58’ indicates that the average rank of the NARFIMA model is 1.58, based on the RMSE metric. A similar interpretation holds across different models and metrics.", please refer to [Figure_01_MCB_BRIC.R](https://github.com/mad-stat/NARFIMA/blob/main/Figure_01_MCB_BRIC.R).

* To view "Figure 2: Murphy diagrams of NARFIMA with baselines (ARIMAx (top) and BSTSx (bottom)) for the 48-month ahead exchange rate forecasting of (a) Brazil, (b) Russia, (c) India, and (d) China. The parameter θ represents the shape parameter as defined in Eqn. (4). Lower scores indicate better performance", please refer to [Figure_02_Murphy_Diagram_BRIC.R](https://github.com/mad-stat/NARFIMA/blob/main/Figure_02_Murphy_Diagram_BRIC.R).

* To view "Figure 3: Visualization of the actual exchange rate series (red dots) with point forecasts from NARFIMA (blue line), BSTSx (green line), and ARIMAx (violet line), along with the conformal prediction interval for NARFIMA (yellow shaded region). Forecasts are for a 48-month horizon for (a) Brazil, (b) Russia, (c) India, and (d) China.", please refer to [Figure_03_Conformal_Prediction_Interval_48.R](https://github.com/mad-stat/NARFIMA/blob/main/Figure_03_Conformal_Prediction_Interval_48.R).

* To view "Figure 4: MCB plot comparing the performance of NARFIMA and its variants based on RMSE metrics. In the plot, ‘NARFIMA - 1.38’ indicates that the average rank of NARFIMA is 1.38, similar for other models.", please refer to [Figure_04_MCB_Ablation_Study.R](https://github.com/mad-stat/NARFIMA/blob/main/Figure_04_MCB_Ablation_Study). The performance evaluation summary can be found in [BRIC NARFIMA Variants Evaluation](https://github.com/mad-stat/NARFIMA/blob/main/Dataset/Dataset_Model_Evaluation/BRIC%20NARFIMA%20Variants%20Evaluation.xlsx). To check the code, please refer to [Ablation_Study.R](https://github.com/mad-stat/NARFIMA/blob/main/Ablation_Study.R).
  
* To view "Table 10: Summary statistics of the datasets utilized in this analysis.", please refer to [Table_10_Summary_Statistics_BRIC.R](https://github.com/mad-stat/NARFIMA/blob/main/Table_10_Summary_Statistics_BRIC.R)

* To view "Table 11: Global characteristics of the economic time series under study for BRIC countries.", please refer to [Table_11_Global_Characteristics_BRIC.R](https://github.com/mad-stat/NARFIMA/blob/main/Table_11_Global_Characteristics_BRIC.R).

* To view "Table 12: Estimates of the fractional differencing parameter $d$ obtained via Whittle likelihood for BRIC exchange rates and macroeconomic variables. Standard errors, $z$-statistics, and associated $p$-values are reported. The $z$-statistics are evaluated against the $1$% critical value of $2.576$. Rejection of the null hypothesis of short memory indicates the presence of long-range dependence.", please refer to [Table_12_Whittle_Estimator.R](https://github.com/mad-stat/NARFIMA/blob/main/Table_12_Whittle_Estimator.R).

* To view "Table 13: Model Confidence Set (MCS) results for all countries evaluated across the baseline forecasters (✓ indicates the model belongs to the MCS set, while ✗ denotes elimination). Superscripts indicate the significance level at which a model is excluded: $^*$ $\alpha = 0.05$, $^\dagger$ $\alpha = 0.10$, and $^\ddagger$ $\alpha = 0.15$.", please refer to [Table_13_MCS_BRIC.R](https://github.com/mad-stat/NARFIMA/blob/main/Table_13_MCS_BRIC.R).

* To generate "Table 14: Statistical properties of ARFIMAx residuals through Teräsvirta, BDS, and Zivot-Andrews tests across all forecast horizons for BRIC nations. For the Zivot-Andrews test, statistics falling below the $1$% critical value of $-5.34$ reject the unit root null in favor of stationarity.", please refer to [Tables_14_ARFIMAx_Residuals_Tests.R](https://github.com/mad-stat/NARFIMA/blob/main/Tables_14_ARFIMAx_Residuals_Tests.R).

* To view "Figure 5: Visualization of the actual exchange rate series (red dots) with point forecasts from NARFIMA (blue line), BSTSx (green line), and ARIMAx (violet line), along with the conformal prediction interval for NARFIMA (yellow shaded region). Forecasts are for a 24-month horizon for (a) Brazil, (b) Russia, (c) India, and (d) China.", please refer to [Figure_05_Conformal_Prediction_Interval_24.R](https://github.com/mad-stat/NARFIMA/blob/main/Figure_05_Conformal_Prediction_Interval_24.R).
