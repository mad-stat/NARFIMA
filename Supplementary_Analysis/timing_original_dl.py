"""
Computational cost of the published deep-learning baselines (reviewer Comment 1).

Re-runs the configurations of Code/Deep_Learning.ipynb (Darts; 100 epochs; input_chunk_length 10 for
the RNN labelled DeepAR and 30 for the others; output_chunk_length 13 for h <= 12, 25 for h = 24 and 50 for
h = 48; past covariates for NBEATSx, NHiTSx, DLinearx, NLinearx, TSMixerx) only to record wall-clock time
(fit + predict) and the number of trainable parameters, on the CPU of the machine stated in the output.
The published forecasts and accuracy figures are not touched.

Usage: python timing_original_dl.py [--countries Brazil Russia ...] [--threads 4]
"""
import argparse
import json
import logging
import platform
import time
import warnings
from pathlib import Path

import pandas as pd
import torch
from darts import TimeSeries
from darts.models import DLinearModel, NBEATSModel, NHiTSModel, NLinearModel, RNNModel, TSMixerModel

warnings.filterwarnings("ignore")
for name in ("pytorch_lightning", "lightning.pytorch", "darts"):
    logging.getLogger(name).setLevel(logging.ERROR)

HERE = Path(__file__).resolve().parent
REPO = HERE.parent
OUT = HERE / "results" / "computational_cost"
HORIZONS = [1, 3, 6, 12, 24, 48]
OCL = {1: 13, 3: 13, 6: 13, 12: 13, 24: 25, 48: 50}
TRAINER = dict(accelerator="cpu", enable_progress_bar=False, enable_model_summary=False, logger=False,
               enable_checkpointing=False)


def load(country):
    df = pd.read_excel(REPO / "Dataset" / "Dataset_Selected_Exogenous" / f"{country}_Data.xlsx")
    idx = pd.date_range(pd.to_datetime(df.iloc[0, 0]), periods=len(df), freq="MS")
    y = TimeSeries.from_times_and_values(idx, df.iloc[:, 1].astype(float).values)
    X = TimeSeries.from_times_and_values(idx, df.iloc[:, 2:7].astype(float).values)
    return y, X


def models(h):
    o = OCL[h]
    return {
        "DeepAR": (RNNModel(input_chunk_length=10, output_chunk_length=o, model="RNN", random_state=100,
                            pl_trainer_kwargs=TRAINER), False),
        "NBEATSx": (NBEATSModel(input_chunk_length=30, output_chunk_length=o, random_state=100, pl_trainer_kwargs=TRAINER), True),
        "NHiTSx": (NHiTSModel(input_chunk_length=30, output_chunk_length=o, random_state=100, pl_trainer_kwargs=TRAINER), True),
        "DLinearx": (DLinearModel(input_chunk_length=30, output_chunk_length=o, random_state=100, pl_trainer_kwargs=TRAINER), True),
        "NLinearx": (NLinearModel(input_chunk_length=30, output_chunk_length=o, random_state=100, pl_trainer_kwargs=TRAINER), True),
        "TSMixerx": (TSMixerModel(input_chunk_length=30, output_chunk_length=o, random_state=100, pl_trainer_kwargs=TRAINER), True),
    }


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--countries", nargs="+", default=["Brazil", "Russia", "India", "China"])
    ap.add_argument("--threads", type=int, default=4)
    a = ap.parse_args()
    torch.set_num_threads(a.threads)
    OUT.mkdir(parents=True, exist_ok=True)
    for c in a.countries:
        y, X = load(c)
        for h in HORIZONS:
            tr, Xtr = y[:-h], X[:-h]
            for name, (m, cov) in models(h).items():
                t0 = time.perf_counter()
                if cov:
                    m.fit(tr, past_covariates=Xtr, epochs=100, verbose=False)
                    m.predict(n=h, series=tr, past_covariates=Xtr, verbose=False)
                else:
                    m.fit(tr, epochs=100, verbose=False)
                    m.predict(n=h, series=tr, verbose=False)
                sec = time.perf_counter() - t0
                npar = sum(p.numel() for p in m.model.parameters() if p.requires_grad)
                rec = dict(Country=c, Horizon=h, Model=name, Seconds_total=round(sec, 3), N_params=int(npar))
                with open(OUT / f"timing_original_dl_{c}.jsonl", "a") as f:
                    f.write(json.dumps(rec) + "\n")
                print(rec, flush=True)
    (OUT / "python_environment_dl_timing.json").write_text(json.dumps(dict(
        python=platform.python_version(), platform=platform.platform(), torch=torch.__version__,
        darts=__import__("darts").__version__, threads=a.threads), indent=2))


if __name__ == "__main__":
    main()
