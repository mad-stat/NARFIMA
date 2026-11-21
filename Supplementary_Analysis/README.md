# Supplementary Analysis for the SNDE revision

This folder holds the code and outputs of the analyses added in response to the reviewer of
*Studies in Nonlinear Dynamics & Econometrics* (manuscript DGSNDE.2026.0096). The original scripts and
results at the top level of the repository are unchanged.

## Where each result appears in the revised paper

The revised paper has no separate supplementary file: all additional tables and figures are in its
appendix, and the complete results for all 25 models are Tables 7–10 of the main text.

| Paper | Content | Script | Main output (in `results/`) |
|---|---|---|---|
| Table 3 (Section 3), Table A.3 (Appendix A.2) | Memory estimates, exchange rates and all series | `long_memory_analysis.R`, `make_long_memory_tables.py` | `long_memory/tables/Table_LM_exchange_rates.tex`, `Table_LM_all_series.tex` |
| Table A.4 (Appendix A.4) | Re-run of the Whittle estimates of the original submission | `long_memory_analysis.R` (step S02) | `long_memory/S02_whittle_table_check.csv` |
| Table A.5 (Appendix A.5) | Monte Carlo check of the estimators | `long_memory_montecarlo.R` | `long_memory/S04_montecarlo_summary.csv` |
| Table A.6 (Appendix A.6) | Bandwidth sensitivity | `long_memory_analysis.R` (S03) | `long_memory/S03_long_memory_estimates_all_bandwidths.csv` |
| Table A.7 (Appendix A.6) | ADF, PP, KPSS, modified R/S and Qu tests | `long_memory_analysis.R` (S03, S05) | `long_memory/S03_unit_root_stationarity_RS_tests.csv`, `S05_qu_test.csv` |
| Figure A.1 (Appendix A.7) | Rolling-window FELW estimates | `long_memory_analysis.R` (S05) | `long_memory/S05_rolling_felw_d.png` |
| Tables 7–10 (Section 5.5) | All five metrics for all 25 models, by country | `additional_baselines.py`, `additional_baselines.R`, `make_comparison_tables.py` | `additional_baselines/tables/Table_Extended_<Country>.tex`, `Additional_Baselines_Comparison.xlsx` |
| Table B.1 (Appendix B.2) | Selected configurations of the additional baselines | `additional_baselines.py`, `additional_baselines.R` | `additional_baselines/tables/timing_and_configurations.csv` |
| Figure 1 (Section 5.6) | MCB with all 25 models | `significance_tests_extended.R` | `significance_tests/MCB_extended_all.png`, `MCB_mean_ranks_all.csv` |
| Table B.2 and Table B.4 (Appendix B.3) | Model confidence sets with all 25 models, and re-run for the 17 original models | `significance_tests_extended.R` | `significance_tests/MCS_extended_all.csv` |
| Table 11 (Section 5.7) | Computational cost | `timing_original_models.R`, `timing_original_dl.py`, `make_cost_tables.py` | `computational_cost/tables/Table_Computational_Cost.tex` |
| Table B.6, Figure B.1 (Appendix B.5) | Run time and accuracy by ensemble size | `timing_original_models.R`, `make_cost_tables.py` | `computational_cost/tables/narfima_ensemble_size_summary.csv`, `Fig_NARFIMA_ensemble_size.png` |
| Figure 4 (Section 6.2), Table B.7 (Appendix B.6) | Ablation study with ARFIMAx–ANN | `ablation_extended.R` | `ablation_extended/MCB_ablation_extended.png`, `ablation_extended_rmse.csv` |

The script names `Table_06_...R` to `Table_13_...R` at the top level of the repository keep the table numbers of the
original submission.

## 1. Additional baselines (reviewer Comment 1)

| Model | Family | Script | Covariates |
|---|---|---|---|
| CatBoostx | Gradient-boosted trees | `additional_baselines.py` | 5 selected drivers, past values only |
| LightGBMx | Gradient-boosted trees | `additional_baselines.py` | same |
| TiDEx | Deep neural network (MLP encoder–decoder) | `additional_baselines.py` | same |
| GRUx | Recurrent neural network | `additional_baselines.py` | same |
| LSTMx | Recurrent neural network | `additional_baselines.py` | same |
| ARIMA–ANN | Hybrid (Zhang, 2003) | `additional_baselines.R` | univariate, as in the original formulation |
| ARFIMA–ANN | Hybrid | `additional_baselines.R` | univariate |
| EWNet | Ensemble wavelet neural network (Panja et al., 2023); code from `mad-stat/Epicasting/Models/EWNet.R` | `additional_baselines.R` | univariate, as published |

**Design (common to all models).**
* The countries, horizons (1, 3, 6, 12, 24, 48 months) and training/test splits are the same as in Tables 6–9.
* Covariates are used only up to the forecast origin, and forecasts are direct multi-step.
* The tree models forecast monthly changes, which are cumulated back to levels.
* The neural networks forecast min–max scaled levels. The scalers are fitted on the training data only.
* Hyperparameters come from a small grid. They are selected on the last max(h, 12) training months, scored by rolling-origin RMSE.
* The networks use early stopping, are refitted for the best number of epochs, and are averaged over 5 seeds.
* All metrics are computed with the same `evaluate()` function as the original Table 6–9 scripts.

**Run (from this folder).**

```bash
python -m venv .venv && .venv/bin/pip install -r requirements.txt
.venv/bin/python additional_baselines.py            # CatBoost, LightGBM, TiDE, GRU, LSTM
Rscript additional_baselines.R                      # ARIMA-ANN, ARFIMA-ANN, EWNet + evaluation of all new models
.venv/bin/python make_comparison_tables.py          # tables (Excel + LaTeX)
```

R packages: `forecast`, `nnet`, `Metrics`, `readxl` (see `results/additional_baselines/R_sessionInfo.txt`).
Python versions: `requirements.txt` and `results/additional_baselines/python_environment.json`.

**Outputs** (`results/additional_baselines/`)
* `forecasts/<Country>/<Model>_<h>.csv`: date, actual value, forecast (and per-seed forecasts for the networks).
* `metrics_additional_baselines.csv`: MAPE, SMAPE, MAE, MASE and RMSE for every model, country and horizon.
* `logs/`: selected hyperparameters, validation RMSE, wall-clock times and parameter or tree counts.
* `tables/Additional_Baselines_Comparison.xlsx`: all 25 models (16 original, 8 additional, NARFIMA) side by side for each country, the compact RMSE table, average ranks and timings.
* `tables/Table_Extended_<Country>.tex`: LaTeX source of Tables 7–10 of the paper (all metrics, all 25 models).
* `tables/Table_Additional_Baselines_RMSE.tex`: the RMSE-only table of the first revision, no longer used in the paper.

## 2. Long-memory analysis (reviewer Comment 2a)

| Step | What it does | Script |
|---|---|---|
| S02 | Re-runs the published Whittle table and shows why it reports d = 0.49 (estimator bound at H = 0.99; z-statistic computed as H/SE) | `long_memory_analysis.R` |
| S03 | Memory parameter d for exchange rates (levels and first differences) and all candidate covariates; tests of d = 0, 1/2, 1; ADF/PP/KPSS; modified R/S | `long_memory_analysis.R` |
| S04 | Monte Carlo check of all estimators (ARFIMA(0,d,0), n = 274, d from 0 to 1.2, 1,000 replications) | `long_memory_montecarlo.R` |
| S05 | Qu (2011) test against spurious long memory, rolling-window FELW estimates, Bai–Perron breaks | `long_memory_analysis.R` |

The estimators live in `R/long_memory_estimators.R`. They were written from the original papers:
* GPH and the bias-reduced GPH of Andrews and Guggenberger.
* Local Whittle.
* Exact local Whittle and feasible exact local Whittle (Shimotsu and Phillips; Shimotsu).
* Lo's modified R/S and Qu's test.

The GPH estimate matches `fracdiff::fdGPH` exactly. The parametric step uses the `arfima` package (exact ML).

**Run (from this folder).**

```bash
Rscript long_memory_analysis.R
Rscript long_memory_montecarlo.R 1000
.venv/bin/python make_long_memory_tables.py
```

R packages: `readxl`, `fracdiff`, `longmemo`, `arfima`, `tseries`, `strucchange`. Outputs are in `results/long_memory/`. The LaTeX and Excel tables are in `results/long_memory/tables/`.

## 3. MCB and MCS with the additional baselines (reviewer Comment 1)

`significance_tests_extended.R` re-runs the paper's MCB plot (Figure 1) and MCS procedure (Table 13) with the additional baselines. It uses the same `tsutils::nemenyi` and `MCS::MCSprocedure` settings as `Figure_01_MCB_BRIC.R` and `Table_13_MCS_BRIC.R` (statistic TR, B = 5000, seed 100), at α = 0.05, 0.10 and 0.15.

Pass `--models=...` to restrict the run to a chosen subset of the new models. Outputs are in `results/significance_tests/`.

## 4. Computational cost (reviewer Comment 1)

| Script | What it times |
|---|---|
| `timing_original_models.R` | The published R models (Naïve, AR, ARIMAx, ARNNx, ARFIMAx, ETS, SETAR, TBATS, GARCH, BSTSx, NARFIMA). It also measures NARFIMA accuracy and cost for ensembles of R = 1 to 1000 networks. |
| `timing_original_dl.py` | The published Darts models, using the configurations in `Code/Deep_Learning.ipynb`. |
| `make_cost_tables.py` | Builds the summary table and the ensemble-size figure, combining the timings above with those recorded for the additional baselines. |

The published models are re-run only to measure time. Their forecasts and accuracy are not changed. Outputs are in `results/computational_cost/`, including the hardware record.

## 5. Citation check (reviewer Comment 3)

`citation_check.py` checks each new reference against Crossref and the doi.org handle API:
* The match must be the journal or proceedings version, not a working paper.
* Title similarity must be at least 0.90.
* The first author and year must agree.

The script then builds the BibTeX from the registered metadata and writes `new_references.bib`. Papers without a DOI are checked against the proceedings or arXiv page. Each result is recorded in `citation_check.csv`.

`normalise_bibtex.py` cleans the Crossref entries (accents, dashes, print-issue year) into `refs_additions_SNDE_revision.bib`, which is appended to the manuscript's `refs.bib`.

## 6. Extended ablation (Section 6.2, Figure 4)

`additional_baselines.R --models=ARFIMAx-ANN` fits the ablation variant ARFIMAx–ANN. It uses NARFIMA's own ARFIMAx first stage, with the same covariates as the published scripts, and adds a network forecast of the ARFIMAx residuals to the ARFIMAx forecast.

`ablation_extended.R` compares it with NARFIMA and the published variants (NARIMA, NBSTS, NNaïve, ARNNx). It produces the RMSE table, the mean ranks and the MCB plot used as Figure 4, all in `results/ablation_extended/`.

The script reads the variants from `Dataset/Dataset_Model_Evaluation/BRIC NARFIMA Variants Evaluation.xlsx`. In `Dataset/MCB/BRIC Error MCB RMSE.xlsx`, the input of the original Figure 4, the NNaïve values of Russia and China are interchanged.
