"""
Tables for the long-memory analysis (reviewer Comment 2a), from the CSV files written by
long_memory_analysis.R and long_memory_montecarlo.R.

Outputs (results/long_memory/tables/)
  Long_Memory_Results.xlsx                - every result table, one sheet each
  Table_LM_exchange_rates.tex             - exchange rates: d estimates (levels / first differences), ML ARFIMA, Qu, Bai-Perron
  Table_LM_all_series.tex                 - replaces the published Whittle table: FELW, LW, GPH for all 30 series + tests
  Table_LM_whittle_check.tex              - why the published values are 0.49 (Supplement)
  Table_LM_montecarlo.tex                 - Monte Carlo bias / RMSE / coverage (Supplement)
Usage: python make_long_memory_tables.py
"""
from pathlib import Path

import pandas as pd
from openpyxl import Workbook
from openpyxl.styles import Alignment, Font, PatternFill
from openpyxl.utils import get_column_letter

HERE = Path(__file__).resolve().parent
RES = HERE / "results" / "long_memory"
OUT = RES / "tables"
OUT.mkdir(parents=True, exist_ok=True)

s02 = pd.read_csv(RES / "S02_whittle_table_check.csv")
s03 = pd.read_csv(RES / "S03_long_memory_estimates_all_bandwidths.csv")
ml = pd.read_csv(RES / "S03_arfima_ml_first_differences.csv")
ur = pd.read_csv(RES / "S03_unit_root_stationarity_RS_tests.csv")
s04 = pd.read_csv(RES / "S04_montecarlo_summary.csv")
qu = pd.read_csv(RES / "S05_qu_test.csv")
bp = pd.read_csv(RES / "S05_bai_perron_first_differences.csv")
roll = pd.read_csv(RES / "S05_rolling_felw_d.csv")
used = pd.read_csv(RES / "series_used.csv")

MAIN_BW = "n^0.6"
EST_ORDER = ["FELW", "ELW", "LW", "GPH", "BR-GPH"]
COUNTRY_ORDER = ["Brazil", "Russia", "India", "China", "Global"]


def stars(p):
    return "^{***}" if p < 0.01 else "^{**}" if p < 0.05 else "^{*}" if p < 0.10 else ""


def cell(d, se):
    return f"{d:.3f} ({se:.3f})"


# ------------------------------------------------------------ Table: exchange rates
er = s03[s03.Series.str.startswith("Exchange") & (s03.Bandwidth == MAIN_BW)]
rows = []
for c in COUNTRY_ORDER[:4]:
    lev = er[(er.Country == c) & (er.Series == "Exchange Rate")].set_index("Estimator")
    dif = er[(er.Country == c) & (er.Series == "Exchange Rate (first difference)")].set_index("Estimator")
    r = ml[ml.Country == c].iloc[0]
    q = qu[(qu.Country == c) & (qu.Series == "Exchange Rate (first difference)")].iloc[0]
    b = bp[bp.Country == c].iloc[0]
    rows.append(dict(
        Country=c,
        **{f"{k} (level)": cell(lev.loc[k, "d_hat"], lev.loc[k, "SE"]) for k in EST_ORDER},
        **{f"{k} (first diff.)": cell(dif.loc[k, "d_hat"], dif.loc[k, "SE"]) for k in ["FELW", "LW", "GPH"]},
        **{"ML ARFIMA 1 + d (first diff.)": f"{r.d_level:.3f} ({r.SE:.3f})", "ML model": r.Model.replace(" on first differences", ""),
           "FELW p(d=0)": lev.loc["FELW", "p_H0_d0"], "FELW p(d=0.5)": lev.loc["FELW", "p_H0_d0.5"],
           "FELW p(d=1)": lev.loc["FELW", "p_H0_d1"], "ML p(d=1)": r.p_H0_d_level_1,
           "Qu W (first diff.)": q.Qu_W, "Qu rejects at 5%": bool(q.Reject_true_long_memory_5pct),
           "Bai-Perron breaks (first diff.)": int(b.Breaks_BIC),
           "Break dates": b.Break_dates if isinstance(b.Break_dates, str) else ""}))
t_er = pd.DataFrame(rows)

lines = [r"\begin{table*}[!htbp]",
         r"\caption{\rev{Long-memory evidence for the BRIC exchange rates (training sample 1997-01 to 2019-10, $n = 274$). "
         r"Semiparametric estimates of the memory parameter $d$ with bandwidth $m = \lfloor n^{0.6} \rfloor = 29$ "
         r"(standard errors in parentheses): feasible exact local Whittle (FELW; Shimotsu, 2010), exact local Whittle "
         r"(ELW; Shimotsu and Phillips, 2005), local Whittle (LW; Robinson, 1995a), GPH (Geweke and Porter-Hudak, 1983) and "
         r"bias-reduced GPH (Andrews and Guggenberger, 2003). ML: exact maximum likelihood ARFIMA$(p,d,q)$ fitted to the first "
         r"differences (BIC over $p,q \le 2$), reported as the implied order of the level, $1+\hat{d}$. Qu: test of "
         r"\citet{qu2011test} of true against spurious long memory on the first differences (5\% critical value 1.155). "
         r"BP: number of breaks in the mean of the first differences (Bai--Perron, BIC). $p$-values refer to FELW (levels).}}",
         r"\centering", r"\begin{adjustbox}{width=\textwidth}",
         r"\begin{tabular}{lccccccccccc}", r"\hline",
         r" & \multicolumn{5}{c}{Levels} & \multicolumn{2}{c}{First differences} & & \multicolumn{3}{c}{FELW $p$-value for $H_0$} \\",
         r"Country & FELW & ELW & LW & GPH & BR-GPH & FELW & GPH & ML $1+\hat{d}$ & $d=0$ & $d=1/2$ & $d=1$ \\ \hline"]
for _, r in t_er.iterrows():
    lines.append(f"{r.Country} & {r['FELW (level)']} & {r['ELW (level)']} & {r['LW (level)']} & {r['GPH (level)']} & "
                 f"{r['BR-GPH (level)']} & {r['FELW (first diff.)']} & {r['GPH (first diff.)']} & "
                 f"{r['ML ARFIMA 1 + d (first diff.)']} & {r['FELW p(d=0)']:.3f} & {r['FELW p(d=0.5)']:.3f} & {r['FELW p(d=1)']:.3f} \\\\")
lines += [r"\hline", r" & \multicolumn{11}{l}{Qu statistic (first differences): " +
          "; ".join(f"{r.Country} {r['Qu W (first diff.)']:.3f}" for _, r in t_er.iterrows()) +
          r". BP breaks: " + "; ".join(f"{r.Country} {r['Bai-Perron breaks (first diff.)']}" +
                                        (f" ({r['Break dates']})" if r['Break dates'] else "") for _, r in t_er.iterrows()) + r".} \\",
          r"\hline", r"\end{tabular}", r"\end{adjustbox}", r"\label{tab:long_memory_er}", r"\end{table*}"]
(OUT / "Table_LM_exchange_rates.tex").write_text("\n".join(lines) + "\n")

# ------------------------------------------------------------ Table: all series (replaces Whittle table)
lv = s03[(s03.Bandwidth == MAIN_BW) & ~s03.Series.str.contains("first difference")]
allrows = []
for (c, s), g in lv.groupby(["Country", "Series"], sort=False):
    g = g.set_index("Estimator")
    f = g.loc["FELW"]
    allrows.append(dict(Country=c, Series=s, m=int(f.m), FELW=f.d_hat, FELW_SE=f.SE, CI_low=f.CI95_low, CI_high=f.CI95_high,
                        LW=g.loc["LW", "d_hat"], LW_SE=g.loc["LW", "SE"], GPH=g.loc["GPH", "d_hat"], GPH_SE=g.loc["GPH", "SE"],
                        p_d0=f.p_H0_d0, p_d05=f["p_H0_d0.5"], p_d1=f.p_H0_d1,
                        n=int(used[(used.Country == c) & (used.Series == s)].n.iloc[0]),
                        note=str(used[(used.Country == c) & (used.Series == s)].Note.iloc[0]) if not pd.isna(used[(used.Country == c) & (used.Series == s)].Note.iloc[0]) else ""))
t_all = pd.DataFrame(allrows)
t_all["Country"] = pd.Categorical(t_all["Country"], COUNTRY_ORDER, ordered=True)
t_all = t_all.sort_values("Country", kind="stable")

lines = [r"\begin{table*}[!htbp]", r"\fontsize{6}{7}\selectfont",
         r"\caption{\rev{Estimates of the memory parameter $d$ for the exchange rates and all candidate covariates "
         r"(levels; training sample 1997-01 to 2019-10). FELW: feasible exact local Whittle estimator \citep{shimotsu2010exact}, "
         r"valid for stationary and nonstationary $d$; LW: local Whittle \citep{robinson1995gaussian}; GPH: log-periodogram "
         r"regression \citep{geweke1983estimation}. Bandwidth $m = \lfloor n^{0.6} \rfloor$; standard errors in parentheses. "
         r"The last three columns give FELW-based $p$-values of $H_0: d = 0$ (short memory), $H_0: d = 1/2$ (boundary of "
         r"stationarity) and $H_0: d = 1$ (unit root). This table replaces the earlier Whittle estimates, which were bounded "
         r"at $d = 0.49$ (see Appendix~\ref{app:whittle}).}}",
         r"\centering", r"\begin{adjustbox}{width=\textwidth}", r"\begin{tabular}{llcccccccc}", r"\hline",
         r"Country & Series & FELW $\hat{d}$ & 95\% CI & LW $\hat{d}$ & GPH $\hat{d}$ & $m$ & $p$($d=0$) & $p$($d=1/2$) & $p$($d=1$) \\ \hline"]
prev = None
for _, r in t_all.iterrows():
    cname = r.Country if r.Country != prev else ""
    if prev is not None and r.Country != prev:
        lines.append(r"\hline")
    prev = r.Country
    nm = r.Series + (r"$^{\dagger}$" if r.note else "")
    lines.append(f"{cname} & {nm} & {cell(r.FELW, r.FELW_SE)} & [{r.CI_low:.2f}, {r.CI_high:.2f}] & {cell(r.LW, r.LW_SE)} & "
                 f"{cell(r.GPH, r.GPH_SE)} & {r.m} & {r.p_d0:.3f} & {r.p_d05:.3f} & {r.p_d1:.3f} \\\\")
notes = t_all[t_all.note != ""]
lines += [r"\hline", r"\multicolumn{10}{l}{$^{\dagger}$ " + "; ".join(f"{r.Country} {r.Series.lower()}: {r.note}" for _, r in notes.iterrows()) + r".} \\"
          if len(notes) else "", r"\end{tabular}", r"\end{adjustbox}", r"\label{Table_Whittle_Estimator}", r"\end{table*}"]
(OUT / "Table_LM_all_series.tex").write_text("\n".join(l for l in lines if l) + "\n")

# ------------------------------------------------------------ Supplement: Whittle check, Monte Carlo
lines = [r"\begin{table}[!htbp]", r"\fontsize{6}{7}\selectfont",
         r"\caption{\rev{Re-run of the Whittle estimates reported in the original submission (\texttt{longmemo::WhittleEst}, "
         r"fractional ARIMA$(0,d,0)$). The routine estimates $H = d + 1/2$ on $(0, 1)$ and stops at $H = 0.99$, i.e. $d = 0.49$; "
         r"the originally reported $z$-statistic is $\hat{H}/\mathrm{SE}$, which tests $H = 0$ ($d = -1/2$) rather than $d = 0$. "
         r"A simulated random walk ($d = 1$) also returns $d = 0.49$.}}",
         r"\centering", r"\begin{tabular}{llccccc}", r"\hline",
         r"Country & Series & $\hat{H}$ & $\hat{d} = \hat{H} - 1/2$ & SE & $z$ reported ($\hat{H}$/SE) & $z$ for $d=0$ \\ \hline"]
for _, r in s02.iterrows():
    lines.append(f"{r.Country} & {r.Series} & {r.H_hat:.3f} & {r.d_hat:.3f} & {r.SE:.3f} & {r.z_published_H_over_SE:.3f} & {r.z_for_H0_d_eq_0:.3f} \\\\")
lines += [r"\hline", r"\end{tabular}", r"\label{tab:whittle_check}", r"\end{table}"]
(OUT / "Table_LM_whittle_check.tex").write_text("\n".join(lines) + "\n")

mc_est = ["Whittle (published setup)", "GPH", "BR-GPH", "LW", "LW on differences + 1", "ELW", "FELW"]
ds = sorted(s04.d_true.unique())
lines = [r"\begin{table}[!htbp]", r"\fontsize{7}{8}\selectfont",
         r"\caption{\rev{Monte Carlo performance of the memory estimators: ARFIMA$(0,d,0)$, $n = 274$, $m = 29$, "
         r"1{,}000 replications. Mean of $\hat{d}$, RMSE and coverage of the nominal 95\% interval $\hat{d} \pm 1.96\,\mathrm{SE}$.}}",
         r"\centering", r"\begin{tabular}{l" + "c" * len(ds) + "}", r"\hline",
         r"Estimator & " + " & ".join(f"$d={d:g}$" for d in ds) + r" \\ \hline"]
for k, lab in [("Mean_d_hat", "Mean of $\\hat{d}$"), ("RMSE", "RMSE"), ("Coverage_95", "Coverage (95\\%)")]:
    lines.append(r"\multicolumn{" + str(len(ds) + 1) + r"}{l}{\textit{" + lab + r"}} \\")
    for e in mc_est:
        v = s04[s04.Estimator == e].set_index("d_true")[k]
        lines.append(e + " & " + " & ".join(f"{v[d]:.3f}" for d in ds) + r" \\")
lines += [r"\hline", r"\end{tabular}", r"\label{tab:lm_montecarlo}", r"\end{table}"]
(OUT / "Table_LM_montecarlo.tex").write_text("\n".join(lines) + "\n")

# ------------------------------------------------------------ Excel
wb = Workbook()
ws = wb.active
ws.title = "README"
for line in ["Long-memory analysis for the SNDE revision (reviewer Comment 2a)",
             "Training sample 1997-01 to 2019-10 (n = 274), as in the paper. Main bandwidth m = floor(n^0.6) = 29.",
             "Sheets: Exchange_rates (main table), All_series (replaces the Whittle table), Whittle_check, Monte_Carlo,",
             "All_bandwidths (n^0.5, n^0.6, n^0.7 for all estimators), ML_ARFIMA, Unit_root_RS, Qu_test, Bai_Perron, Rolling_FELW.",
             "Code: long_memory_analysis.R, long_memory_montecarlo.R, R/long_memory_estimators.R."]:
    ws.append([line])
ws["A1"].font = Font(bold=True, size=13)
ws.column_dimensions["A"].width = 140
HDR = PatternFill("solid", fgColor="1F4E78")
for name, df in [("Exchange_rates", t_er), ("All_series", t_all), ("Whittle_check", s02), ("Monte_Carlo", s04),
                 ("All_bandwidths", s03), ("ML_ARFIMA", ml), ("Unit_root_RS", ur), ("Qu_test", qu), ("Bai_Perron", bp),
                 ("Rolling_FELW", roll)]:
    w = wb.create_sheet(name)
    w.append(list(df.columns))
    for c in w[1]:
        c.font = Font(bold=True, color="FFFFFF"); c.fill = HDR; c.alignment = Alignment(wrap_text=True)
    for row in df.itertuples(index=False):
        w.append([round(v, 4) if isinstance(v, float) else (str(v) if hasattr(v, "categories") else v) for v in row])
    for j in range(1, len(df.columns) + 1):
        w.column_dimensions[get_column_letter(j)].width = 18
    w.freeze_panes = "C2"
wb.save(OUT / "Long_Memory_Results.xlsx")
print("written:", sorted(p.name for p in OUT.iterdir()))
