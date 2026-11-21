"""
Turn the Crossref BibTeX (new_references.bib) into clean entries for the manuscript bibliography.

* keeps: author, title, journal/booktitle, volume, number, pages, year, doi
* converts HTML entities and typographic characters (&amp;, en dash, U+2010 hyphen, curly quotes)
* uses the print-issue year recorded as expected_year in citation_check.csv (Crossref sometimes gives the online year)
* fixes all-capital titles to title case
* skips keys that already exist in the manuscript's refs.bib
Usage: python normalise_bibtex.py <path to refs.bib> <output .bib>
"""
import csv
import re
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
refs_bib, out_path = Path(sys.argv[1]), Path(sys.argv[2])
existing = set(re.findall(r"@\w+\{([^,]+),", refs_bib.read_text(encoding="utf-8")))
rows = list(csv.DictReader(open(HERE / "citation_check.csv")))
expected_year = {r["key"]: r["expected_year"] for r in rows}
registered_title = {r["key"]: r["title_registered"] for r in rows}
# Crossref records without authors (the DOI content negotiation then returns no author and no title):
# authors taken from the publisher's page, title from the Crossref API (title_registered in citation_check.csv)
MISSING_AUTHORS = {
    # econometricsociety.org/publications/econometrica/2012/11/01/likelihood-inference-fractionally-cointegrated-vector
    "johansen2012likelihood": "Johansen, S{\\o}ren and Nielsen, Morten {\\O}rregaard",
}

KEEP = ["author", "title", "journal", "booktitle", "volume", "number", "pages", "year", "doi"]


def fields(entry):
    out = {}
    for m in re.finditer(r"(\w+)=\{((?:[^{}]|\{[^{}]*\})*)\}|(\w+)=(\w+)", entry):
        k = (m.group(1) or m.group(3)).lower()
        v = m.group(2) if m.group(1) else m.group(4)
        out[k] = v
    return out


ACCENTS = {"Ø": r"{\O}", "ø": r"{\o}", "Á": r"{\'A}", "á": r"{\'a}", "é": r"{\'e}", "É": r"{\'E}", "í": r"{\'i}",
           "ó": r"{\'o}", "ú": r"{\'u}", "ü": r'{\"u}', "ö": r'{\"o}', "ä": r'{\"a}', "ç": r"{\c c}", "ñ": r"{\~n}",
           "è": r"{\`e}", "ł": r"{\l}", "ş": r"{\c s}", "ğ": r"{\u g}"}


def clean(v):
    v = v.replace("&amp;", r"\&").replace("–", "--").replace("—", "---").replace("‐", "-").replace("’", "'")
    for k, t in ACCENTS.items():          # BibTeX is 8-bit: write accents as LaTeX macros
        v = v.replace(k, t)
    return re.sub(r"\s+", " ", v).strip()


def title_case(t):
    if t.upper() != t:
        return t
    small = {"a", "an", "and", "of", "the", "in", "for", "on", "with", "to"}
    words = t.lower().split()
    return " ".join(w if (i and w in small) else w.capitalize() for i, w in enumerate(words))


entries = re.findall(r"@(\w+)\{([^,]+),(.*?)\}\s*(?=@|\Z)", (HERE / "new_references.bib").read_text(encoding="utf-8"), flags=re.S)
blocks, skipped = [], []
for typ, key, body in entries:
    key = key.strip()
    if key in existing:
        skipped.append(key)
        continue
    f = {k: clean(v) for k, v in fields(body).items()}
    if f.get("author") and all(a == a.upper() for a in f["author"].split(" and ")):   # Crossref sometimes registers names in capitals
        f["author"] = " and ".join(a.strip().title() for a in f["author"].split(" and "))
    if not f.get("author"):
        f["author"] = MISSING_AUTHORS[key]          # KeyError = an entry without authors that has not been checked
    if not f.get("title"):
        f["title"] = clean(registered_title[key])
    f["title"] = "{" + title_case(f.get("title", "")) + "}"   # protect capitalisation of the registered title
    if key in expected_year and expected_year[key]:
        f["year"] = str(int(float(expected_year[key])))
    lines = [f"@{typ.lower()}{{{key},"] + [f"  {k:<9} = {{{f[k]}}}," for k in KEEP if k in f and f[k]]
    blocks.append("\n".join(lines).rstrip(",") + "\n}")
manual = (HERE / "new_references_manual.bib").read_text(encoding="utf-8").strip()
manual_keys = set(re.findall(r"@\w+\{([^,]+),", manual))
out_path.write_text("% ---- References added in the SNDE revision; verified by Supplementary_Analysis/citation_check.py ----\n\n"
                    + "\n\n".join(blocks) + "\n\n" + (manual if not (manual_keys & existing) else "") + "\n", encoding="utf-8")
print(f"{len(blocks)} Crossref entries + {len(manual_keys)} manual entries written; skipped existing keys: {skipped}")
