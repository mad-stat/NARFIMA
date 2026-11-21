"""
Computational-cost tables and the NARFIMA ensemble-size figure (reviewer Comment 1).

Inputs: results/computational_cost/ (timing_original_models.R, timing_original_dl.py) and
        results/additional_baselines/tables/timing_and_configurations.csv (additional_baselines.py / .R).
Outputs (results/computational_cost/tables/): Computational_Cost.xlsx, Table_Computational_Cost.tex,
        Fig_NARFIMA_ensemble_size.png
Times are wall-clock seconds per country-horizon cell on the machine recorded in R_sessionInfo_and_hardware.txt.
Usage: python make_cost_tables.py
"""
import glob
from pathlib import Path

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

HERE = Path(__file__).resolve().parent
RES = HERE / "results" / "computational_cost"
OUT = RES / "tables"
OUT.mkdir(parents=True, exist_ok=True)

FAMILY = {"Naïve": "Statistical", "AR": "Statistical", "ARIMAx": "Statistical", "ARFIMAx": "Statistical",
          "ETS": "Statistical", "SETAR": "Statistical", "TBATS": "Statistical", "GARCH": "Statistical",
          "BSTSx": "Statistical (Bayesian, MCMC)", "ARNNx": "Neural network (R)",
          "DeepAR": "Deep learning (Darts)", "NBEATSx": "Deep learning (Darts)", "NHiTSx": "Deep learning (Darts)",
          "DLinearx": "Deep learning (Darts)", "NLinearx": "Deep learning (Darts)", "TSMixerx": "Deep learning (Darts)",
          "CatBoostx": "Gradient boosting", "LightGBMx": "Gradient boosting", "TiDEx": "Deep learning (Darts)",
          "GRUx": "Recurrent network (Darts)", "LSTMx": "Recurrent network (Darts)", "ARIMA-ANN": "Hybrid",
          "ARFIMA-ANN": "Hybrid", "EWNet": "Hybrid (wavelet + ANN)", "NARFIMA": "Proposed"}
ORDER = list(FAMILY)

orig_r = pd.read_csv(RES / "timing_original_R_models.csv")
orig_dl = pd.concat([pd.read_json(f, lines=True) for f in sorted(glob.glob(str(RES / "timing_original_dl_*.jsonl")))])
orig = pd.concat([orig_r, orig_dl], ignore_index=True)
orig["Includes_tuning"] = False
orig["Seconds_fit_forecast"] = orig["Seconds_total"]

new = pd.read_csv(HERE / "results" / "additional_baselines" / "tables" / "timing_and_configurations.csv")
new = new.rename(columns={"N_params_or_trees": "N_params"})
new["Seconds_fit_forecast"] = np.where(new["Seconds_select"].notna(),
                                       new["Seconds_final_fit"] + new["Seconds_predict"].fillna(0), new["Seconds_total"])
new["Includes_tuning"] = new["Seconds_select"].notna()
cells = pd.concat([orig[["Country", "Horizon", "Model", "Seconds_fit_forecast", "Seconds_total", "N_params", "Includes_tuning"]],
                   new[["Country", "Horizon", "Model", "Seconds_fit_forecast", "Seconds_total", "N_params", "Includes_tuning"]]],
                  ignore_index=True)
cells.to_csv(OUT / "timing_all_models_by_cell.csv", index=False)

summ = cells.groupby("Model").agg(
    Fit_forecast_median=("Seconds_fit_forecast", "median"), Fit_forecast_max=("Seconds_fit_forecast", "max"),
    Total_incl_tuning_median=("Seconds_total", "median"), Params_median=("N_params", "median"),
    Cells=("Seconds_total", "size")).reindex(ORDER)
summ.insert(0, "Family", [FAMILY[m] for m in summ.index])
summ.loc[~summ.index.isin(new["Model"].unique()), "Total_incl_tuning_median"] = np.nan
summ.to_csv(OUT / "timing_summary_all_models.csv")

ens = pd.read_csv(RES / "narfima_ensemble_size.csv")
ens = ens.replace([np.inf, -np.inf], np.nan)
ens_s = ens.groupby("R").agg(Median_seconds=("Seconds_total", "median"),
                             Rel_RMSE_median=("RMSE_relative_to_R1000", "median"),
                             Rel_RMSE_q25=("RMSE_relative_to_R1000", lambda x: x.quantile(0.25)),
                             Rel_RMSE_q75=("RMSE_relative_to_R1000", lambda x: x.quantile(0.75)),
                             Share_within_5pct=("RMSE_relative_to_R1000", lambda x: np.mean(np.abs(x - 1) <= 0.05)))
ens_s.to_csv(OUT / "narfima_ensemble_size_summary.csv")

# ----------------------------------------------------------------------------------- figure
fig, ax1 = plt.subplots(figsize=(7, 4.2), dpi=200)
R = ens_s.index.values
ax1.fill_between(R, ens_s.Rel_RMSE_q25, ens_s.Rel_RMSE_q75, alpha=0.25, color="tab:blue", label="Interquartile range (24 cells)")
ax1.plot(R, ens_s.Rel_RMSE_median, "o-", color="tab:blue", label="Median RMSE relative to R = 1000")
ax1.axhline(1, color="grey", lw=0.8, ls="--")
ax1.set_xscale("log"); ax1.set_xlabel("Number of networks in the NARFIMA ensemble (R)")
ax1.set_ylabel("RMSE relative to R = 1000")
ax2 = ax1.twinx()
ax2.plot(R, ens_s.Median_seconds, "s--", color="tab:red", label="Median wall-clock seconds")
ax2.set_ylabel("Seconds per country-horizon (fit + forecast)", color="tab:red")
h1, l1 = ax1.get_legend_handles_labels(); h2, l2 = ax2.get_legend_handles_labels()
ax1.legend(h1 + h2, l1 + l2, fontsize=7, loc="upper right")
fig.tight_layout(); fig.savefig(OUT / "Fig_NARFIMA_ensemble_size.png"); plt.close(fig)

# ----------------------------------------------------------------------------------- LaTeX
def fmt(v, d=2):
    if pd.isna(v):
        return "--"
    return f"{v:,.0f}" if v >= 1000 else f"{v:.{d}f}"


lines = [r"\begin{table}[!htbp]", r"\fontsize{7}{8}\selectfont", r"\centering", r"\begin{tabular}{llcccc}", r"\hline",
         r" & & \multicolumn{2}{c}{Fit + forecast (s)} & Incl.\ tuning & Parameters \\",
         r"Model & Family & median & max & (median, s) & (median) \\ \hline"]
for m, r in summ.iterrows():
    nm = r"{\color{blue}" + m + "}" if m in new["Model"].unique() or m == "NARFIMA" else m
    lines.append(f"{nm.replace('ARIMA-ANN', 'ARIMA--ANN').replace('ARFIMA-ANN', 'ARFIMA--ANN')} & {r.Family} & "
                 f"{fmt(r.Fit_forecast_median)} & {fmt(r.Fit_forecast_max)} & {fmt(r.Total_incl_tuning_median)} & "
                 f"{fmt(r.Params_median, 0)} \\\\")
lines += [r"\hline", r"\end{tabular}", r"\label{tab:computational_cost}", r"\end{table}"]

hw = (RES / "R_sessionInfo_and_hardware.txt").read_text().splitlines()
cpu = next((l.split(": ", 1)[1] for l in hw if l.startswith("CPU:")), "")
cores = next((l.split(": ", 1)[1] for l in hw if l.startswith("Cores:")), "")
ram = next((l.split(": ", 1)[1] for l in hw if l.startswith("RAM")), "")
HARDWARE = f"{cpu}, {cores} cores, {ram} GB RAM, CPU only"
caption = (r"\caption{\rev{Computational cost per country--horizon cell (24 cells): wall-clock seconds to fit the model and "
           r"produce the forecast (median and maximum), median time including hyperparameter selection and five-seed averaging "
           r"(additional baselines only), and median number of estimated parameters or trees. Published baselines and NARFIMA "
           r"are timed with their published hyperparameters (NARFIMA: ARFIMAx stage plus an ensemble of 1000 networks). "
           r"Hardware: " + HARDWARE + r".}}")
lines.insert(2, caption)
(OUT / "Table_Computational_Cost.tex").write_text("\n".join(lines) + "\n")

with pd.ExcelWriter(OUT / "Computational_Cost.xlsx") as xw:
    pd.DataFrame({"Note": [
        "Computational cost for the SNDE revision (reviewer Comment 1).",
        "Hardware: " + HARDWARE + ". R " + next((l for l in hw if l.startswith("R version")), "") + ".",
        "Published models re-run only for timing (published hyperparameters); additional baselines timed during their runs.",
        "Fit+forecast excludes tuning; 'incl. tuning' adds hyperparameter selection and the five seeds (networks).",
        "NARFIMA ensemble size: RMSE relative to R = 1000 in the same run (24 cells)."]}).to_excel(xw, "README", index=False)
    summ.round(3).to_excel(xw, "Summary_by_model")
    ens_s.round(4).to_excel(xw, "NARFIMA_ensemble_size")
    cells.to_excel(xw, "By_cell", index=False)
    ens.to_excel(xw, "Ensemble_by_cell", index=False)

print(summ.round(2).to_string()); print(); print(ens_s.round(3).to_string()); print("hardware:", HARDWARE)
