"""
Additional baselines requested by the SNDE reviewer (Comment 1):
    CatBoost, LightGBM (gradient-boosted trees), TiDE, GRU, LSTM (deep neural networks).
The two hybrid baselines (ARIMA-ANN, ARFIMA-ANN) are in additional_baselines.R.

Design (identical for every model; documented in the Supplement)
----------------------------------------------------------------
* Data: Dataset/Dataset_Selected_Exogenous/<Country>_Data.xlsx (1997-01 to 2023-10, 322 months).
* Splits: same as Tables 6-9. For horizon h, training = first 322 - h months, test = last h months.
* Covariates: the five selected drivers (short-term IRD, oil-price growth, GEPU, US EMV, US MPU), used as
  PAST covariates, i.e. only values observed up to the forecast origin (as for the paper's NBEATSx, NHiTSx, ...).
  No test-period covariate value is used.
* Multi-step strategy: direct, output_chunk_length = h (one forecast path of length h from the origin).
* Trees (CatBoost, LightGBM) model the monthly change of the exchange rate and are cumulated back to the
  level, because regression trees cannot extrapolate beyond the training range of the level.
* Neural networks (TiDE, GRU, LSTM) model the min-max scaled level; scalers are fitted on training data only.
* Hyperparameters: small grid chosen on a validation window = last max(h, 12) months of the training sample,
  scored by RMSE of h-step forecasts from every origin inside that window (rolling origin, no refit).
  Networks use early stopping on the same window; the selected configuration is refitted on the full
  training sample for the best number of epochs and averaged over 5 seeds.
* Timing: wall-clock seconds for selection, final fit and prediction; number of trainable parameters.

Usage
-----
    python additional_baselines.py                       # all countries, horizons and models
    python additional_baselines.py --countries Brazil --models LightGBM CatBoost --horizons 1 3
Outputs go to Supplementary_Analysis/results/additional_baselines/.
"""
from __future__ import annotations

import argparse
import itertools
import json
import logging
import os
import platform
import random
import time
import warnings
from pathlib import Path

import numpy as np
import pandas as pd
import torch
from darts import TimeSeries
from darts.dataprocessing.transformers import Scaler
from darts.models import BlockRNNModel, CatBoostModel, LightGBMModel, TiDEModel
from pytorch_lightning.callbacks import Callback, EarlyStopping

warnings.filterwarnings("ignore")
logging.getLogger("pytorch_lightning").setLevel(logging.ERROR)
logging.getLogger("lightning.pytorch").setLevel(logging.ERROR)
logging.getLogger("darts").setLevel(logging.ERROR)

HERE = Path(__file__).resolve().parent
REPO = HERE.parent
DATA_DIR = REPO / "Dataset" / "Dataset_Selected_Exogenous"
OUT_DIR = HERE / "results" / "additional_baselines"

COUNTRIES = ["Brazil", "Russia", "India", "China"]
HORIZONS = [1, 3, 6, 12, 24, 48]
MODELS = ["CatBoost", "LightGBM", "TiDE", "GRU", "LSTM"]
SEEDS = [100, 101, 102, 103, 104]          # final-fit seeds for the neural networks
BASE_SEED = 100
MAX_EPOCHS = 300
PATIENCE = 20


# ----------------------------------------------------------------------------------------- data
def load_country(country: str) -> tuple[TimeSeries, TimeSeries]:
    """Target series and the five selected covariates (by column position: col 1 = exchange rate, cols 2-6 = covariates)."""
    df = pd.read_excel(DATA_DIR / f"{country}_Data.xlsx")
    dates = pd.to_datetime(df.iloc[:, 0])
    expected = pd.date_range(dates.iloc[0], periods=len(df), freq="MS")
    assert (dates.values == expected.values).all(), f"{country}: dates are not consecutive months"
    y = TimeSeries.from_times_and_values(expected, df.iloc[:, 1].astype(float).values, columns=["exchange_rate"])
    cov_names = ["IRD", "Oil_growth", "GEPU", "US_EMV", "US_MPU"]   # columns 2..6 of the selected-exogenous files
    X = TimeSeries.from_times_and_values(expected, df.iloc[:, 2:7].astype(float).values, columns=cov_names)
    return y, X


def rmse(a, b) -> float:
    a, b = np.asarray(a, float), np.asarray(b, float)
    return float(np.sqrt(np.mean((a - b) ** 2)))


def set_all_seeds(seed: int) -> None:
    random.seed(seed)
    np.random.seed(seed)
    torch.manual_seed(seed)


# ------------------------------------------------------------------------------- tree models
TREE_GRID = [dict(lags=l, lags_past_covariates=lp) for l, lp in itertools.product([6, 12], [1, 6])]


def make_tree(name: str, cfg: dict, h: int):
    common = dict(lags=cfg["lags"], lags_past_covariates=cfg["lags_past_covariates"],
                  output_chunk_length=h, multi_models=True, random_state=BASE_SEED)
    if name == "LightGBM":
        return LightGBMModel(**common, n_estimators=300, learning_rate=0.05, num_leaves=15,
                             min_child_samples=10, subsample=0.9, subsample_freq=1, colsample_bytree=0.9,
                             verbose=-1, n_jobs=1)   # n_jobs=1: multithreaded LightGBM segfaults on macOS when torch (libomp) is loaded
    return CatBoostModel(**common, iterations=300, learning_rate=0.05, depth=4, verbose=0,
                         thread_count=4)


def tree_forecast_levels(model, y_level: TimeSeries, X: TimeSeries, h: int) -> np.ndarray:
    """Predict h monthly changes from the end of y_level and cumulate them onto the last observed level."""
    dy = y_level.diff()
    d_hat = model.predict(n=h, series=dy, past_covariates=X).values().ravel()
    return y_level.values()[-1, 0] + np.cumsum(d_hat)


def tree_n_params(model, h: int) -> int:
    """Number of fitted trees (one sub-model per forecast step)."""
    try:
        est = model.model.estimators_ if hasattr(model.model, "estimators_") else [model.model]
        n = 0
        for e in est:
            if hasattr(e, "booster_"):
                n += e.booster_.num_trees()
            elif hasattr(e, "tree_count_"):
                n += e.tree_count_
        return int(n)
    except Exception:
        return -1


def run_tree(name: str, y_tr: TimeSeries, X_tr: TimeSeries, h: int) -> dict:
    val_len = max(h, 12)
    t0 = time.perf_counter()
    scores = []
    for cfg in TREE_GRID:
        y_in = y_tr[:-val_len]
        m = make_tree(name, cfg, h)
        m.fit(y_in.diff(), past_covariates=X_tr)
        errs = []
        for origin in range(len(y_tr) - val_len, len(y_tr) - h + 1):     # rolling origins inside the window
            y_hist = y_tr[:origin]
            pred = tree_forecast_levels(m, y_hist, X_tr, h)
            errs.append(rmse(y_tr.values()[origin:origin + h, 0], pred))
        scores.append(float(np.mean(errs)))
    best = TREE_GRID[int(np.argmin(scores))]
    t_sel = time.perf_counter() - t0

    t1 = time.perf_counter()
    model = make_tree(name, best, h)
    model.fit(y_tr.diff(), past_covariates=X_tr)
    t_fit = time.perf_counter() - t1
    t2 = time.perf_counter()
    fc = tree_forecast_levels(model, y_tr, X_tr, h)
    t_pred = time.perf_counter() - t2
    return dict(forecast=fc, config=best, val_rmse=min(scores), t_select=t_sel, t_fit=t_fit,
                t_predict=t_pred, n_params=tree_n_params(model, h), n_seeds=1)


# ---------------------------------------------------------------------------- neural networks
NN_GRID = {
    "TiDE": [dict(input_chunk_length=L, hidden_size=hs) for L, hs in itertools.product([12, 24], [32, 64])],
    "GRU": [dict(input_chunk_length=L, hidden_dim=hd) for L, hd in itertools.product([12, 24], [16, 32])],
    "LSTM": [dict(input_chunk_length=L, hidden_dim=hd) for L, hd in itertools.product([12, 24], [16, 32])],
}


class ValLossTracker(Callback):
    def __init__(self):
        self.losses = []

    def on_validation_epoch_end(self, trainer, pl_module):
        v = trainer.callback_metrics.get("val_loss")
        if v is not None and not trainer.sanity_checking:
            self.losses.append(float(v))


def trainer_kwargs(callbacks=None, max_epochs=MAX_EPOCHS):
    return dict(accelerator="cpu", enable_progress_bar=False, enable_model_summary=False,
                logger=False, enable_checkpointing=False,
                callbacks=callbacks or [])


def make_nn(name: str, cfg: dict, h: int, seed: int, callbacks=None, max_epochs=MAX_EPOCHS):
    common = dict(input_chunk_length=cfg["input_chunk_length"], output_chunk_length=h, batch_size=32,
                  n_epochs=max_epochs, random_state=seed, optimizer_kwargs={"lr": 1e-3},
                  pl_trainer_kwargs=trainer_kwargs(callbacks, max_epochs), force_reset=True,
                  save_checkpoints=False)
    if name == "TiDE":
        return TiDEModel(**common, hidden_size=cfg["hidden_size"], num_encoder_layers=1, num_decoder_layers=1,
                         decoder_output_dim=8, temporal_decoder_hidden=16, dropout=0.1, use_layer_norm=True)
    return BlockRNNModel(**common, model=name, hidden_dim=cfg["hidden_dim"], n_rnn_layers=1, dropout=0.0)


def n_trainable(model) -> int:
    try:
        return int(sum(p.numel() for p in model.model.parameters() if p.requires_grad))
    except Exception:
        return -1


def run_nn(name: str, y_tr: TimeSeries, X_tr: TimeSeries, h: int) -> dict:
    val_len = max(h, 12)
    t0 = time.perf_counter()
    results = []
    for cfg in NN_GRID[name]:
        L = cfg["input_chunk_length"]
        y_in = y_tr[:-val_len]
        sy, sx = Scaler(), Scaler()
        y_in_s = sy.fit_transform(y_in)
        X_s = sx.fit(X_tr[: len(y_in)]).transform(X_tr)
        y_tr_s = sy.transform(y_tr)
        y_val_s = y_tr_s[-(L + val_len):]                      # inputs + validation targets
        tracker = ValLossTracker()
        es = EarlyStopping(monitor="val_loss", patience=PATIENCE, min_delta=1e-6, mode="min")
        set_all_seeds(BASE_SEED)
        m = make_nn(name, cfg, h, BASE_SEED, callbacks=[es, tracker])
        m.fit(y_in_s, past_covariates=X_s, val_series=y_val_s, val_past_covariates=X_s, verbose=False)
        best_epoch = int(np.argmin(tracker.losses)) + 1 if tracker.losses else MAX_EPOCHS
        errs = []
        for origin in range(len(y_tr) - val_len, len(y_tr) - h + 1):
            pred_s = m.predict(n=h, series=y_tr_s[:origin], past_covariates=X_s, verbose=False)
            pred = sy.inverse_transform(pred_s).values().ravel()
            errs.append(rmse(y_tr.values()[origin:origin + h, 0], pred))
        results.append((float(np.mean(errs)), cfg, max(best_epoch, 5)))
    val_score, best, best_epoch = min(results, key=lambda r: r[0])
    t_sel = time.perf_counter() - t0

    sy, sx = Scaler(), Scaler()
    y_s = sy.fit_transform(y_tr)
    X_s = sx.fit_transform(X_tr)
    preds, t_fit, t_pred, npar = [], 0.0, 0.0, -1
    for seed in SEEDS:
        set_all_seeds(seed)
        m = make_nn(name, best, h, seed, max_epochs=best_epoch)
        t1 = time.perf_counter()
        m.fit(y_s, past_covariates=X_s, verbose=False)
        t_fit += time.perf_counter() - t1
        t2 = time.perf_counter()
        p = m.predict(n=h, series=y_s, past_covariates=X_s, verbose=False)
        t_pred += time.perf_counter() - t2
        preds.append(sy.inverse_transform(p).values().ravel())
        npar = n_trainable(m)
    preds = np.vstack(preds)
    return dict(forecast=preds.mean(axis=0), seed_forecasts=preds, config={**best, "epochs": best_epoch},
                val_rmse=val_score, t_select=t_sel, t_fit=t_fit, t_predict=t_pred, n_params=npar,
                n_seeds=len(SEEDS))


# ------------------------------------------------------------------------------------- driver
def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--countries", nargs="+", default=COUNTRIES)
    ap.add_argument("--horizons", nargs="+", type=int, default=HORIZONS)
    ap.add_argument("--models", nargs="+", default=MODELS)
    ap.add_argument("--threads", type=int, default=4)
    args = ap.parse_args()
    torch.set_num_threads(args.threads)

    fc_dir = OUT_DIR / "forecasts"
    log_dir = OUT_DIR / "logs"
    fc_dir.mkdir(parents=True, exist_ok=True)
    log_dir.mkdir(parents=True, exist_ok=True)

    for country in args.countries:
        y, X = load_country(country)
        for h in args.horizons:
            y_tr, y_te = y[:-h], y[-h:]
            X_tr = X[:-h]                                   # covariates observed up to the origin only
            for name in args.models:
                t_start = time.perf_counter()
                res = run_tree(name, y_tr, X_tr, h) if name in ("CatBoost", "LightGBM") else run_nn(name, y_tr, X_tr, h)
                total = time.perf_counter() - t_start
                out = pd.DataFrame({"Date": y_te.time_index.strftime("%Y-%m-%d"),
                                    "Actual": y_te.values().ravel(), "Forecast": res["forecast"]})
                if "seed_forecasts" in res:
                    for i, s in enumerate(SEEDS):
                        out[f"Forecast_seed{s}"] = res["seed_forecasts"][i]
                (fc_dir / country).mkdir(exist_ok=True)
                out.to_csv(fc_dir / country / f"{name}_{h}.csv", index=False)
                rec = dict(Country=country, Horizon=h, Model=name, Config=json.dumps(res["config"]),
                           Val_RMSE=res["val_rmse"], Test_RMSE=rmse(y_te.values().ravel(), res["forecast"]),
                           Seconds_select=round(res["t_select"], 3), Seconds_final_fit=round(res["t_fit"], 3),
                           Seconds_predict=round(res["t_predict"], 4), Seconds_total=round(total, 3),
                           N_seeds=res["n_seeds"], N_params_or_trees=res["n_params"])
                with open(log_dir / f"run_{country}.jsonl", "a") as f:
                    f.write(json.dumps(rec) + "\n")
                print(f"{country:7s} h={h:2d} {name:9s} test RMSE={rec['Test_RMSE']:.4f} "
                      f"val RMSE={rec['Val_RMSE']:.4f} cfg={rec['Config']} total={total:.1f}s", flush=True)

    env = dict(python=platform.python_version(), platform=platform.platform(), processor=platform.processor(),
               torch=torch.__version__, darts=__import__("darts").__version__,
               lightgbm=__import__("lightgbm").__version__, catboost=__import__("catboost").__version__,
               threads=args.threads)
    (OUT_DIR / "python_environment.json").write_text(json.dumps(env, indent=2))


if __name__ == "__main__":
    main()
