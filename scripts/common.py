"""Shared helpers for the experiment scripts: cached signatures, baselines, bootstrap."""
import json, os, sys

import numpy as np
from scipy.stats import rankdata

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, ".."))
import sgw
from corpora import LOADERS, NAMES  # noqa: F401

RESULTS = os.path.join(HERE, "..", "results")
CACHE = os.path.join(HERE, "..", "data", "cache")
os.makedirs(RESULTS, exist_ok=True)
os.makedirs(CACHE, exist_ok=True)
A_GRID, K_GRID = (0, 0.5, 1, 2, 4, 8, 16, 32), (0, 0.05, 0.1, 0.2, 0.5, 1, 2)
SPLITS = ("train", "dev", "test")


def cached(name, fn):
    """Compute once, keep as .npy under data/cache."""
    path = os.path.join(CACHE, name + ".npy")
    if not os.path.exists(path):
        np.save(path, fn())
    return np.load(path)


def signatures(corpora, tag=""):
    return {c: {s: cached(f"sig{tag}_{c}_{s}", lambda: sgw.sketch(list(corpora[c][s].text))) for s in SPLITS} for c in corpora}


def pearson(a, b):
    a, b = a - a.mean(), b - b.mean()
    d = np.sqrt((a * a).sum() * (b * b).sum())
    return float((a * b).sum() / d) if d > 0 else 0.0


def spearman(a, b):
    return pearson(rankdata(a), rankdata(b))


def resamples(groups, n=2000, seed=0):
    """Index arrays for a cluster bootstrap: whole groups are drawn with replacement."""
    rng = np.random.default_rng(seed)
    _, inv = np.unique(groups, return_inverse=True)
    order = np.argsort(inv, kind="stable")
    bounds = np.concatenate(([0], np.cumsum(np.bincount(inv))))
    g = len(bounds) - 1
    for _ in range(n):
        pick = rng.integers(0, g, g)
        yield np.concatenate([order[bounds[i]:bounds[i + 1]] for i in pick]) if g < len(groups) else order[pick]


def boot(stat, groups, n=2000):
    """Point estimate and 95% percentile interval of stat(index array)."""
    full = stat(np.arange(len(groups)))
    draws = np.array([stat(i) for i in resamples(groups, n)])
    lo, hi = np.percentile(draws, [2.5, 97.5], axis=0)
    return full, lo, hi


def jack(stat, groups):
    """Delete-one-cluster jackknife with a t reference: estimate and 95% interval. For results that rest on a
    handful of clusters, where a percentile bootstrap over clusters is too narrow."""
    from scipy.stats import t as tdist
    ids = np.unique(groups)
    full = float(stat(np.arange(len(groups))))
    drop = np.array([float(stat(np.flatnonzero(groups != g))) for g in ids])
    n = len(ids)
    se = np.sqrt((n - 1) / n * np.sum((drop - drop.mean()) ** 2))
    h = tdist.ppf(0.975, n - 1) * se
    return [full, full - h, full + h]


def fmt(x):
    return [float(v) for v in np.atleast_1d(x)] if np.ndim(x) else float(x)


def save(name, obj):
    json.dump(obj, open(os.path.join(RESULTS, name + ".json"), "w"), indent=1)


# ---- baselines ----

def vader_scores(texts):
    from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer
    v = SentimentIntensityAnalyzer()
    return np.array([v.polarity_scores(t)["compound"] for t in texts])


def textblob_scores(texts):
    from textblob import TextBlob
    return np.array([TextBlob(t).sentiment.polarity for t in texts])


_LABMT = None


def hedonometer_scores(texts):
    """Dodds et al. (2011): mean labMT happiness over words outside the neutral band 4 < h < 6."""
    global _LABMT
    if _LABMT is None:
        _LABMT = {}
        for line in open(os.path.join(HERE, "..", "data", "lex", "labMT_English.tsv"), encoding="utf-8"):
            w, h = line.split("\t")[:2]
            if abs(float(h) - 5) >= 1:
                _LABMT[w] = float(h)
    out = []
    for t in texts:
        h = [_LABMT[w] for w in sgw.tokens(t) if w in _LABMT]
        out.append(np.mean(h) if h else 5.0)
    return np.array(out)
