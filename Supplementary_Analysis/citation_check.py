"""
Integrity check of every new reference added in the SNDE revision.

For each candidate (key, expected title, first-author surname, year) the script queries the Crossref API,
takes the best match, and records the DOI, the registered title, authors, journal, year, volume, issue and
pages, a title-similarity score, and whether title, author and year agree. A reference is marked VERIFIED
only if the DOI is registered (doi.org handle API) and title similarity >= 0.90, first author and year match.
The BibTeX entry is then taken from doi.org content negotiation, so the bibliography is built from the
registered metadata rather than typed by hand. Papers without a DOI (e.g. conference proceedings) are
checked against a stable URL and marked as such.

Usage: python citation_check.py      -> citation_check.csv, new_references.bib
"""
import csv
import difflib
import json
import re
import time
import urllib.parse
import urllib.request
import urllib.error
from pathlib import Path

HERE = Path(__file__).resolve().parent
UA = {"User-Agent": "citation-check/1.0 (SNDE revision; research use)"}

CANDIDATES = [
    # key, title, first-author surname, year
    ("geweke1983estimation", "The estimation and application of long memory time series models", "Geweke", 1983),
    ("robinson1995gaussian", "Gaussian semiparametric estimation of long range dependence", "Robinson", 1995),
    ("robinson1995log", "Log-periodogram regression of time series with long range dependence", "Robinson", 1995),
    ("shimotsu2005exact", "Exact local Whittle estimation of fractional integration", "Shimotsu", 2005),
    ("shimotsu2010exact", "Exact local Whittle estimation of fractional integration with unknown mean and time trend", "Shimotsu", 2010),
    ("andrews2003bias", "A bias-reduced log-periodogram regression estimator for the long-memory parameter", "Andrews", 2003),
    ("velasco1999nonstationary", "Non-stationary log-periodogram regression", "Velasco", 1999),
    ("velasco1999gaussian", "Gaussian semiparametric estimation of non-stationary time series", "Velasco", 1999),
    ("lo1991long", "Long-term memory in stock market prices", "Lo", 1991),
    ("qu2011test", "A test against spurious long memory", "Qu", 2011),
    ("dahlhaus1997fitting", "Fitting time series models to nonstationary processes", "Dahlhaus", 1997),
    ("diebold2001long", "Long memory and regime switching", "Diebold", 2001),
    ("fox1986large", "Large-sample properties of parameter estimates for strongly dependent stationary Gaussian time series", "Fox", 1986),
    ("sowell1992maximum", "Maximum likelihood estimation of stationary univariate fractionally integrated time series models", "Sowell", 1992),
    ("jensen2014fast", "A fast fractional difference algorithm", "Jensen", 2014),
    ("bai2003computation", "Computation and analysis of multiple structural change models", "Bai", 2003),
    ("cheung1993long", "Long memory in foreign-exchange rates", "Cheung", 1993),
    ("baillie1994cointegration", "Cointegration, fractional cointegration, and exchange rate dynamics", "Baillie", 1994),
    ("diebold1991real", "Real exchange rates under the gold standard", "Diebold", 1991),
    ("johansen2008representation", "A representation theory for a class of vector autoregressive models for fractional processes", "Johansen", 2008),
    ("johansen2012likelihood", "Likelihood inference for a fractionally cointegrated vector autoregressive model", "Johansen", 2012),
    ("dolatabadi2016fractionally", "A fractionally cointegrated VAR model with deterministic trends and application to commodity futures markets", "Dolatabadi", 2016),
    ("ravishanker1997bayesian", "Bayesian analysis of vector ARFIMA processes", "Ravishanker", 1997),
    ("sela2009computationally", "Computationally efficient methods for two multivariate fractionally integrated models", "Sela", 2009),
    ("tsay2010maximum", "Maximum likelihood estimation of stationary multivariate ARFIMA processes", "Tsay", 2010),
    ("chiriac2011modelling", "Modelling and forecasting multivariate realized volatility", "Chiriac", 2011),
    ("zhang2003time", "Time series forecasting using a hybrid ARIMA and neural network model", "Zhang", 2003),
    ("khashei2011novel", "A novel hybridization of artificial neural networks and ARIMA models for time series forecasting", "Khashei", 2011),
    ("bukhari2020fractional", "Fractional neuro-sequential ARFIMA-LSTM for financial market forecasting", "Bukhari", 2020),
    ("panja2023epicasting", "Epicasting: An Ensemble Wavelet Neural Network for forecasting epidemics", "Panja", 2023),  # existing key; EWNet
    ("chen2016xgboost", "XGBoost: A Scalable Tree Boosting System", "Chen", 2016),
    ("cho2014learning", "Learning Phrase Representations using RNN Encoder-Decoder for Statistical Machine Translation", "Cho", 2014),
    ("medeiros2021forecasting", "Forecasting inflation in a data-rich environment: the benefits of machine learning methods", "Medeiros", 2021),
    ("goulet2022machine", "How is machine learning useful for macroeconomic forecasting?", "Goulet Coulombe", 2022),
    ("tashman2000out", "Out-of-sample tests of forecasting accuracy: an analysis and review", "Tashman", 2000),
    ("bergmeir2012use", "On the use of cross-validation for time series predictor evaluation", "Bergmeir", 2012),
]

# Searches that return a different paper first: the expected DOI is looked up directly (and still checked)
DOI_OVERRIDE = {"chen2016xgboost": "10.1145/2939672.2939785"}

# Conference papers without a DOI: verified against the proceedings page instead
NO_DOI = [
    ("ke2017lightgbm", "LightGBM: A Highly Efficient Gradient Boosting Decision Tree", "Ke", 2017,
     "https://papers.nips.cc/paper_files/paper/2017/hash/6449f44a102fde848669bdd9eb6b76fa-Abstract.html"),
    ("prokhorenkova2018catboost", "CatBoost: unbiased boosting with categorical features", "Prokhorenkova", 2018,
     "https://papers.nips.cc/paper_files/paper/2018/hash/14491b756b3a51daac41c24863285549-Abstract.html"),
    ("das2023long", "Long-term Forecasting with TiDE: Time-series Dense Encoder", "Das", 2023,
     "https://export.arxiv.org/abs/2304.08424"),   # cited as arXiv preprint
]


def get(url, accept=None, timeout=30, tries=5):
    h = dict(UA)
    if accept:
        h["Accept"] = accept
    for k in range(tries):
        try:
            req = urllib.request.Request(url, headers=h)
            with urllib.request.urlopen(req, timeout=timeout) as r:
                return r.status, r.read().decode("utf-8", errors="replace"), r.geturl()
        except urllib.error.HTTPError as e:
            if e.code == 429 and k < tries - 1:
                time.sleep(5 * (k + 1))
                continue
            raise


def norm(s):
    return re.sub(r"[^a-z0-9 ]", "", re.sub(r"<[^>]+>", "", s.lower())).strip()


rows, bib = [], []
for key, title, author, year in CANDIDATES:
    q = urllib.parse.urlencode({"query.bibliographic": f"{title} {author}", "rows": 5,
                                "select": "DOI,title,subtitle,author,container-title,issued,published-print,volume,issue,page,type"})
    try:
        _, body, _ = get(f"https://api.crossref.org/works?{q}")
        items = json.loads(body)["message"]["items"]
    except Exception as e:
        rows.append(dict(key=key, status=f"QUERY FAILED: {e}"))
        continue
    if key in DOI_OVERRIDE:                                  # look the expected DOI up directly
        _, body, _ = get(f"https://api.crossref.org/works/{DOI_OVERRIDE[key]}")
        items = [json.loads(body)["message"]] + items
    best, best_sim, best_score = None, 0.0, -1.0
    for it in items:
        t = (it.get("title") or [""])[0] + ((": " + it["subtitle"][0]) if it.get("subtitle") else "")
        sim = difflib.SequenceMatcher(None, norm(t), norm(title)).ratio()
        venue = (it.get("container-title") or [""])[0]
        published = it.get("type") in ("journal-article", "proceedings-article") and "SSRN" not in venue  # not WPs/preprints
        score = sim + (0.5 if published else 0.0)
        if score > best_score:
            best, best_sim, best_score = it, sim, score
    it = best or {}
    authors = it.get("author", [])
    fam = authors[0].get("family", "") if authors else ""
    yr = (it.get("issued", {}).get("date-parts") or [[None]])[0][0]
    yr_print = (it.get("published-print", {}).get("date-parts") or [[None]])[0][0]
    doi = it.get("DOI", "")
    # DOI existence via the DOI handle API (publisher pages often block scripted requests)
    try:
        _, hb, _ = get(f"https://doi.org/api/handles/{urllib.parse.quote(doi)}") if doi else (None, "{}", "")
        hj = json.loads(hb)
        code = 200 if hj.get("responseCode") == 1 else f"handle responseCode {hj.get('responseCode')}"
        final = next((v["data"]["value"] for v in hj.get("values", []) if v.get("type") == "URL"), "")
    except Exception as e:
        code, final = f"ERR {e}", ""
    author_ok = norm(author.split()[0]) in norm(fam) or norm(fam) in norm(author)
    year_ok = any(v is not None and abs(v - year) <= 1 for v in (yr, yr_print))   # online-first vs print year
    ok = bool(doi) and best_sim >= 0.90 and author_ok and year_ok and code in (200, 302) \
        and it.get("type") in ("journal-article", "proceedings-article") \
        and "SSRN" not in ((it.get("container-title") or [""])[0])
    rows.append(dict(key=key, status=("VERIFIED" + ("" if year in (yr, yr_print) else " (Crossref year differs by 1)")) if ok else "CHECK MANUALLY", doi=doi, title_registered=(it.get("title") or [""])[0],
                     title_similarity=round(best_sim, 3), first_author=fam, year=yr, expected_year=year,
                     journal=(it.get("container-title") or [""])[0], volume=it.get("volume", ""), issue=it.get("issue", ""),
                     pages=it.get("page", ""), n_authors=len(authors), doi_resolves=code, resolved_to=final))
    if ok:
        try:
            _, b, _ = get(f"https://doi.org/{doi}", accept="application/x-bibtex")
            b = re.sub(r"^@(\w+)\{[^,]*,", lambda m: "@" + m.group(1) + "{" + key + ",", b.strip(), count=1)
            bib.append(b)
        except Exception as e:
            rows[-1]["status"] = f"VERIFIED (bibtex fetch failed: {e})"
    time.sleep(1.5)

for key, title, author, year, url in NO_DOI:
    try:
        code, body, final = get(url)
        txt = norm(body)
        ok = code == 200 and norm(title) in txt and norm(author) in txt
    except Exception as e:
        code, ok, final = f"ERR {e}", False, ""
    rows.append(dict(key=key, status="VERIFIED (no DOI; proceedings page)" if ok else "CHECK MANUALLY", doi="",
                     title_registered=title if ok else "", title_similarity="", first_author=author if ok else "",
                     year=year, expected_year=year, journal="", volume="", issue="", pages="", n_authors="",
                     doi_resolves=code, resolved_to=final or url))

fields = ["key", "status", "doi", "title_registered", "title_similarity", "first_author", "year", "expected_year",
          "journal", "volume", "issue", "pages", "n_authors", "doi_resolves", "resolved_to"]
with open(HERE / "citation_check.csv", "w", newline="") as f:
    w = csv.DictWriter(f, fieldnames=fields)
    w.writeheader()
    for r in rows:
        w.writerow({k: r.get(k, "") for k in fields})
(HERE / "new_references.bib").write_text("\n\n".join(bib) + "\n")
for r in rows:
    print(f"{r['key']:30s} {r['status']:38s} {r.get('doi',''):40s} sim={r.get('title_similarity','')} {r.get('journal','')[:40]}")
