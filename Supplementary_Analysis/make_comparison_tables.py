"""
Build the comparison tables for the additional baselines (SNDE reviewer, Comment 1).

Inputs
  * Published results of the 17 original models: Dataset/Dataset_Model_Evaluation/<Country> Evaluation.xlsx
  * New results: results/additional_baselines/metrics_additional_baselines.csv  (from additional_baselines.R)
  * Run logs:    results/additional_baselines/logs/run_<Country>.jsonl, logs/run_R_hybrids.csv
Outputs (results/additional_baselines/tables/)
  * Additional_Baselines_Comparison.xlsx  - one sheet per country (all 24 models, best/second-best marked),
                                            average RMSE ranks, compact RMSE table, timings, configurations
  * Table_Extended_<Country>.tex          - Tables 6-9 in the manuscript layout with the new models added (blue)
  * Table_Additional_Baselines_RMSE.tex   - compact RMSE table: 7 new baselines vs NARFIMA, all countries/horizons
Usage: python make_comparison_tables.py
"""
from __future__ import annotations

import json
from pathlib import Path

import numpy as np
import pandas as pd
from openpyxl import Workbook
from openpyxl.styles import Alignment, Border, Font, PatternFill, Side
from openpyxl.utils import get_column_letter

HERE = Path(__file__).resolve().parent
REPO = HERE.parent
RES = HERE / "results" / "additional_baselines"
OUT = RES / "tables"
OUT.mkdir(parents=True, exist_ok=True)

COUNTRIES = ["Brazil", "Russia", "India", "China"]
HORIZONS = [1, 3, 6, 12, 24, 48]
METRICS = ["MAPE", "SMAPE", "MAE", "MASE", "RMSE"]
ORIGINAL = ["Naïve", "AR", "ARIMAx", "ARNNx", "ARFIMAx", "ETS", "SETAR", "TBATS", "GARCH", "BSTSx", "DeepAR",
            "NBEATSx", "NHiTSx", "DLinearx", "NLinearx", "TSMixerx"]
NEW_RAW = ["CatBoost", "LightGBM", "TiDE", "GRU", "LSTM", "ARIMA-ANN", "ARFIMA-ANN", "EWNet"]
# Display names: an "x" suffix marks models that use the exogenous covariates, as in the manuscript.
NEW = {"CatBoost": "CatBoostx", "LightGBM": "LightGBMx", "TiDE": "TiDEx", "GRU": "GRUx", "LSTM": "LSTMx",
       "ARIMA-ANN": "ARIMA-ANN", "ARFIMA-ANN": "ARFIMA-ANN", "EWNet": "EWNet"}
NEW_DISPLAY = [NEW[m] for m in NEW_RAW]
ALL_MODELS = ORIGINAL + NEW_DISPLAY + ["NARFIMA"]
TEX_NAME = {"NBEATSx": "NBeatsx", "ARIMA-ANN": "ARIMA--ANN", "ARFIMA-ANN": "ARFIMA--ANN"}


# ------------------------------------------------------------------------------------------- load
def load_original(country: str) -> pd.DataFrame:
    raw = pd.read_excel(REPO / "Dataset" / "Dataset_Model_Evaluation" / f"{country} Evaluation.xlsx")
    raw = raw.loc[:, [c for c in raw.columns if not str(c).startswith("Unnamed")]]
    raw["Horizon"] = raw["Horizon"].ffill()
    raw = raw.dropna(subset=["Metrics"])
    raw = raw[raw["Metrics"].isin(METRICS)]
    long = raw.melt(id_vars=["Horizon", "Metrics"], var_name="Model", value_name="Value")
    long["Model"] = long["Model"].replace({"NBEATSx": "NBEATSx"})
    long["Value"] = pd.to_numeric(long["Value"], errors="coerce")       # 'NA' (MASE at h = 1) -> NaN
    long["Horizon"] = long["Horizon"].astype(int)
    long["Country"] = country
    return long.rename(columns={"Metrics": "Metric"})


def load_new() -> pd.DataFrame:
    m = pd.read_csv(RES / "metrics_additional_baselines.csv")
    m = m[m["MODEL"].isin(NEW_RAW)]          # ARFIMAx-ANN is an ablation variant (ablation_extended.R), not a benchmark
    long = m.melt(id_vars=["Country", "Horizon", "MODEL"], value_vars=METRICS, var_name="Metric", value_name="Value")
    long = long.rename(columns={"MODEL": "Model"})
    long["Model"] = long["Model"].map(NEW)
    long["Value"] = pd.to_numeric(long["Value"], errors="coerce")
    return long


orig = pd.concat([load_original(c) for c in COUNTRIES], ignore_index=True)
missing_cols = sorted(set(orig["Model"]) - set(ORIGINAL + ["NARFIMA"]))
assert not missing_cols, f"unexpected model columns in evaluation files: {missing_cols}"
new = load_new()
expected_new = len(COUNTRIES) * len(HORIZONS) * len(NEW_RAW) * len(METRICS)
assert len(new) == expected_new, f"new results incomplete: {len(new)} of {expected_new} rows"
allm = pd.concat([orig, new], ignore_index=True)
# Display convention of the manuscript: SMAPE reported in percent
allm.loc[allm["Metric"] == "SMAPE", "Value"] *= 100
allm.to_csv(OUT / "all_models_metrics_long.csv", index=False)


def wide(country: str) -> pd.DataFrame:
    d = allm[allm["Country"] == country].pivot_table(index=["Horizon", "Metric"], columns="Model", values="Value",
                                                       aggfunc="first")
    d = d.reindex(columns=ALL_MODELS)
    idx = pd.MultiIndex.from_product([HORIZONS, METRICS], names=["Horizon", "Metric"])
    return d.reindex(idx)


def best_two(row: pd.Series) -> tuple[set, set]:
    v = row.dropna()
    if v.empty:
        return set(), set()
    u = np.sort(v.unique())
    best = set(v[v == u[0]].index)
    second = set(v[v == u[1]].index) if len(u) > 1 else set()
    return best, second


# ---------------------------------------------------------------------------- ranks and summary
rmse = allm[allm["Metric"] == "RMSE"].pivot_table(index=["Country", "Horizon"], columns="Model", values="Value")
rmse = rmse.reindex(columns=ALL_MODELS)
ranks = rmse.rank(axis=1, method="min")
avg_rank = pd.DataFrame({
    **{c: ranks.xs(c, level="Country").mean() for c in COUNTRIES},
    "All (24 cells)": ranks.mean(),
    "Times best": (ranks == 1).sum(),
}).sort_values("All (24 cells)")
avg_rank.index.name = "Model"
avg_rank.to_csv(OUT / "average_rmse_rank.csv")

compact = rmse[NEW_DISPLAY + ["NARFIMA"]].copy()
best_orig = rmse[ORIGINAL].idxmin(axis=1)
compact["Best original baseline"] = [f"{m} ({rmse.loc[i, m]:.3f})" for i, m in best_orig.items()]
compact[f"NARFIMA rank (of {len(ALL_MODELS)})"] = ranks["NARFIMA"].astype(int)
compact.to_csv(OUT / "compact_rmse_new_vs_narfima.csv")

# ----------------------------------------------------------------------------------- timings
tim = []
for c in COUNTRIES:
    f = RES / "logs" / f"run_{c}.jsonl"
    if f.exists():
        for line in f.read_text().splitlines():
            r = json.loads(line)
            tim.append(dict(Country=r["Country"], Horizon=r["Horizon"], Model=NEW[r["Model"]], Config=r["Config"],
                            Seconds_select=r["Seconds_select"], Seconds_final_fit=r["Seconds_final_fit"],
                            Seconds_predict=r["Seconds_predict"], Seconds_total=r["Seconds_total"],
                            N_params_or_trees=r["N_params_or_trees"], N_seeds=r["N_seeds"]))
for rh in sorted((RES / "logs").glob("run_R_*.csv")):
    for _, r in pd.read_csv(rh).iterrows():
        if r["Model"] not in NEW:            # ablation variant, not a benchmark
            continue
        tim.append(dict(Country=r["Country"], Horizon=int(r["Horizon"]), Model=NEW[r["Model"]], Config=r["Config"],
                        Seconds_select=np.nan, Seconds_final_fit=r["Seconds_total"], Seconds_predict=np.nan,
                        Seconds_total=r["Seconds_total"], N_params_or_trees=r["N_params"], N_seeds=5))
tim = pd.DataFrame(tim).drop_duplicates(subset=["Country", "Horizon", "Model"], keep="last")
tim.to_csv(OUT / "timing_and_configurations.csv", index=False)
tim_summary = tim.groupby("Model").agg(Median_seconds_total=("Seconds_total", "median"),
                                       Max_seconds_total=("Seconds_total", "max"),
                                       Median_params_or_trees=("N_params_or_trees", "median")).reindex(NEW_DISPLAY)
tim_summary.to_csv(OUT / "timing_summary.csv")

# ------------------------------------------------------------------------------------- Excel
HDR = PatternFill("solid", fgColor="1F4E78")
NEWHDR = PatternFill("solid", fgColor="2E75B6")
BEST = PatternFill("solid", fgColor="C6EFCE")
SECOND = PatternFill("solid", fgColor="FFEB9C")
thin = Side(style="thin", color="BFBFBF")
BORDER = Border(left=thin, right=thin, top=thin, bottom=thin)
wb = Workbook()
ws0 = wb.active
ws0.title = "README"
notes = [
    ["Additional baselines for the SNDE revision (reviewer Comment 1)"],
    ["Original 16 baselines and NARFIMA: published values from Dataset/Dataset_Model_Evaluation (unchanged)."],
    ["New baselines (blue headers): CatBoostx, LightGBMx, TiDEx, GRUx, LSTMx (additional_baselines.py) and "
     "ARIMA-ANN, ARFIMA-ANN, EWNet (additional_baselines.R); metrics from the same evaluate() function as Tables 6-9."],
    ["SMAPE is shown in percent, as in the manuscript. MASE is not defined for h = 1 (as in the manuscript)."],
    [f"Green = best, yellow = second-best among all {len(ALL_MODELS)} models in each row."],
    ["Sheets: one per country; Compact_RMSE; Average_RMSE_rank; Timing_summary; Timing_and_configs."],
]
for n in notes:
    ws0.append(n)
ws0["A1"].font = Font(bold=True, size=13)
ws0.column_dimensions["A"].width = 150

for c in COUNTRIES:
    ws = wb.create_sheet(c)
    d = wide(c)
    ws.append(["Horizon", "Metric"] + ALL_MODELS)
    for j, cell in enumerate(ws[1]):
        cell.font = Font(bold=True, color="FFFFFF")
        cell.fill = NEWHDR if (j >= 2 and ALL_MODELS[j - 2] in NEW_DISPLAY) else HDR
        cell.alignment = Alignment(horizontal="center", vertical="center", wrap_text=True)
    for (h, met), row in d.iterrows():
        if h == 1 and met == "MASE":
            continue
        b, s = best_two(row)
        ws.append([h, met] + [None if pd.isna(v) else round(float(v), 4) for v in row.values])
        r = ws.max_row
        for j, mdl in enumerate(ALL_MODELS, start=3):
            cell = ws.cell(row=r, column=j)
            cell.number_format = "0.000"
            if mdl in b:
                cell.fill, cell.font = BEST, Font(bold=True, underline="single")
            elif mdl in s:
                cell.fill, cell.font = SECOND, Font(bold=True, italic=True)
    for row in ws.iter_rows():
        for cell in row:
            cell.border = BORDER
    ws.column_dimensions["A"].width = 8
    ws.column_dimensions["B"].width = 8
    for j in range(3, 3 + len(ALL_MODELS)):
        ws.column_dimensions[get_column_letter(j)].width = 11
    ws.freeze_panes = "C2"


def df_sheet(name, df, index=True, widths=None):
    ws = wb.create_sheet(name)
    df2 = df.reset_index() if index else df
    ws.append([str(c) for c in df2.columns])
    for cell in ws[1]:
        cell.font = Font(bold=True, color="FFFFFF")
        cell.fill = HDR
        cell.alignment = Alignment(wrap_text=True)
    for row in df2.itertuples(index=False):
        ws.append([(round(v, 4) if isinstance(v, (float, np.floating)) and not pd.isna(v) else (None if (isinstance(v, float) and pd.isna(v)) else v)) for v in row])
    for j in range(1, len(df2.columns) + 1):
        ws.column_dimensions[get_column_letter(j)].width = (widths or {}).get(j, 16)
    ws.freeze_panes = "B2"


df_sheet("Compact_RMSE", compact.round(4))
df_sheet("Average_RMSE_rank", avg_rank.round(2))
df_sheet("Timing_summary", tim_summary.round(3))
df_sheet("Timing_and_configs", tim, index=False, widths={4: 70})
wb.save(OUT / "Additional_Baselines_Comparison.xlsx")


# ------------------------------------------------------------------------------------- LaTeX
def tex_name(m: str) -> str:
    n = TEX_NAME.get(m, m)
    if m in NEW_DISPLAY:
        return r"{\color{blue}" + n + "}"
    if m == "NARFIMA":
        return r"{\color{blue}NARFIMA}"
    return n


def num(v: float) -> str:
    """3 decimals as in the manuscript; 4 decimals for values below 0.01 so that tiny errors stay distinguishable."""
    return f"{v:.3f}" if abs(v) >= 0.01 else f"{v:.4f}"


def fmt(v, mdl, b, s):
    if pd.isna(v):
        return "--"
    t = num(v)
    if mdl in b:
        t = r"\textbf{\underline{" + t + "}}"
    elif mdl in s:
        t = r"\textbf{\textit{" + t + "}}"
    if mdl in NEW_DISPLAY:
        t = r"{\color{blue}" + t + "}"
    return t


for c in COUNTRIES:
    d = wide(c)
    lines = [r"\begin{table*}[!htbp]",
             r"\caption{\rev{Evaluation of the proposed NARFIMA model relative to the original and the additional "
             r"baselines (blue columns) across all forecast horizons for " + c +
             r" (\textbf{\underline{best}} and \textbf{\textit{second-best}} results among all models are highlighted). "
             r"The MASE metric is not defined for a forecast horizon of length one.}}",
             r"\centering", r"\begin{adjustbox}{width=1\textwidth}",
             r"\begin{tabular}{" + "c" * (2 + len(ALL_MODELS)) + "}", r"\hline",
             "Horizon & Metrics & " + " & ".join(tex_name(m) for m in ALL_MODELS) + r" \\\hline"]
    for h in HORIZONS:
        first = True
        for met in METRICS:
            if h == 1 and met == "MASE":
                continue
            row = d.loc[(h, met)]
            b, s = best_two(row)
            cells = [fmt(row[m], m, b, s) for m in ALL_MODELS]
            lines.append((f"{h} & " if first else "& ") + f"{met} & " + " & ".join(cells) + r" \\")
            first = False
        lines.append(r"\hline")
    lines += [r"\end{tabular}", r"\end{adjustbox}", r"\label{tab:performance_metrics_" + c.lower()[:4] + "_ext}",
              r"\end{table*}"]
    (OUT / f"Table_Extended_{c}.tex").write_text("\n".join(lines) + "\n")

cols8 = NEW_DISPLAY + ["NARFIMA"]
lines = [r"\begin{table}[!htbp]",
         r"\caption{\rev{RMSE of the additional baselines and NARFIMA for all BRIC countries and forecast horizons "
         rf"(\textbf{{\underline{{best}}}} and \textbf{{\textit{{second-best}}}} among these {len(cols8)} models are highlighted). "
         rf"The last column gives the rank of NARFIMA among all {len(ALL_MODELS)} models ({len(ORIGINAL)} original baselines, "
         rf"{len(NEW_DISPLAY)} additional baselines and NARFIMA).}}}}",
         r"\centering", r"\begin{adjustbox}{width=\textwidth}",
         r"\begin{tabular}{ll" + "c" * len(cols8) + "c}", r"\hline",
         "Country & $h$ & " + " & ".join(TEX_NAME.get(m, m) for m in cols8) + r" & Rank of NARFIMA \\\hline"]
for c in COUNTRIES:
    for k, h in enumerate(HORIZONS):
        row = rmse.loc[(c, h), cols8]
        b, s = best_two(row)
        cells = []
        for m in cols8:
            t = num(row[m])
            if m in b:
                t = r"\textbf{\underline{" + t + "}}"
            elif m in s:
                t = r"\textbf{\textit{" + t + "}}"
            cells.append(t)
        lines.append((rf"\multirow{{6}}{{*}}{{{c}}}" if k == 0 else "") + f" & {h} & " + " & ".join(cells) +
                     f" & {int(ranks.loc[(c, h), 'NARFIMA'])} \\\\")
    lines.append(r"\hline")
lines += [r"\end{tabular}", r"\end{adjustbox}", r"\label{tab:additional_baselines_rmse}", r"\end{table}"]
(OUT / "Table_Additional_Baselines_RMSE.tex").write_text("\n".join(lines) + "\n")

print("Average RMSE rank (all 24 cells):")
print(avg_rank[["All (24 cells)", "Times best"]].round(2).to_string())
print("\nCompact RMSE table:")
print(compact.round(3).to_string())
print("\nTiming summary (new models):")
print(tim_summary.round(2).to_string())
print("\nWritten to", OUT)
