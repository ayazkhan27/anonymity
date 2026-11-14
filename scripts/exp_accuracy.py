"""Agreement with human continuous ratings (primary outcome), leave-one-corpus-out.

For each target corpus the two exposure scalars (a, kappa) and the ridge coefficients are
fitted on the training and development portions of the other corpora only, then scored
on the target's test portion. Writes results/accuracy.json and per-item test scores.
"""
import sys
import numpy as np
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.linear_model import Ridge

import re

from common import *

VARIANTS = {  # name -> keyword overrides for sgw.features, plus fixed a / kappa where an ablation pins them
    "full": {}, "no position": {"position": False}, "no shifters": {"shifters": False},
    "one channel": {"two_channel": False}, "no dilution": {"kappa": 0}, "no shrinkage": {"a": 0},
}


def zscore(y, ref):
    return (y - ref.mean()) / ref.std()


def design(sig, corpora, names, splits, a, k, kw):
    """Stacked features, standardised ratings and equal-per-corpus weights."""
    X, y, w = [], [], []
    for c in names:
        ref = corpora[c]["train"].y.values
        for s in splits:
            X.append(sgw.features(sig[c][s], a, k, **kw))
            y.append(zscore(corpora[c][s].y.values, ref))
            w.append(np.full(len(y[-1]), 1.0 / sum(len(corpora[c][t]) for t in splits)))
    return np.vstack(X), np.concatenate(y), np.concatenate(w)


def fit(sig, corpora, pool, variant):
    """Choose (a, kappa) on the pool's development data, then refit on train + dev."""
    kw = {k: v for k, v in VARIANTS[variant].items() if k not in ("a", "kappa")}
    grid = [(a, k) for a in ([VARIANTS[variant]["a"]] if "a" in VARIANTS[variant] else A_GRID)
            for k in ([VARIANTS[variant]["kappa"]] if "kappa" in VARIANTS[variant] else K_GRID)]
    best, best_level = (-9,), (-9,)
    for a, k in grid:
        if a == 0 and k == 0:
            continue  # exposure would be zero for texts with no charged word
        coef = sgw.ridge(*design(sig, corpora, pool, ("train",), a, k, kw))
        r = np.mean([pearson(sgw.predict(coef, sgw.features(sig[c]["dev"], a, k, **kw)), corpora[c]["dev"].y.values) for c in pool])
        lvl = np.mean([pearson(sgw.level(sig[c]["dev"], a, k), corpora[c]["dev"].y.values) for c in pool])
        best = max(best, (r, a, k))
        best_level = max(best_level, (lvl, a, k))
    _, a, k = best
    coef = sgw.ridge(*design(sig, corpora, pool, ("train", "dev"), a, k, kw))
    return dict(a=a, kappa=k, coef=coef, kw=kw, level=best_level[1:])


def main():
    corpora = {c: load() for c, load in LOADERS.items()}
    sig = signatures(corpora)
    out, scores = {}, {}
    for target in corpora:
        test, y = corpora[target]["test"], corpora[target]["test"].y.values
        texts = list(test.text)
        pool = [c for c in corpora if c != target]
        s = {}
        for variant in VARIANTS:
            m = fit(sig, corpora, pool, variant)
            s["SGW" if variant == "full" else f"SGW, {variant}"] = sgw.predict(m["coef"], sgw.features(sig[target]["test"], m["a"], m["kappa"], **m["kw"]))
            if variant == "full":
                loco = m
                s["SGW level"] = sgw.level(sig[target]["test"], *m["level"])
        own = fit(sig, corpora, [target], "full")                # constants fitted on the target's own training data
        s["SGW, in-corpus constants"] = sgw.predict(own["coef"], sgw.features(sig[target]["test"], own["a"], own["kappa"]))
        s["VADER"] = cached(f"vader_{target}_test", lambda: vader_scores(texts))
        s["VADER, mean of sentences"] = cached(f"vadersent_{target}_test", lambda: vader_sentence_means(texts))
        s["Hedonometer"] = cached(f"hedo_{target}_test", lambda: hedonometer_scores(texts))
        s["TextBlob"] = cached(f"textblob_{target}_test", lambda: textblob_scores(texts))

        def tfidf():
            tr = corpora[target]["train"]
            vec = TfidfVectorizer(ngram_range=(1, 2), min_df=2, sublinear_tf=True)
            return Ridge(alpha=1.0).fit(vec.fit_transform(tr.text), tr.y.values).predict(vec.transform(texts))
        s["TF-IDF ridge (in-corpus)"] = cached(f"tfidf_{target}_test", tfidf)
        for name, label in (("refined", "SGW, refined charges"), ("bowpool", "Bag-of-words ridge (other corpora)"),
                            ("distilbert", "DistilBERT (SST-2)"), ("roberta", "Twitter-RoBERTa")):
            path = os.path.join(CACHE, f"{name}_{target}_test.npy")   # written by exp_refine.py and exp_transformers.py
            if os.path.exists(path):
                s[label] = np.load(path)
        keep = ~np.isnan(next(iter(v for k, v in s.items() if "BERT" in k), np.zeros(len(y))))
        groups = test.group.values[keep]
        res = {"n": int(keep.sum()), "loco": {"a": loco["a"], "kappa": loco["kappa"], "level_a": loco["level"][0], "level_kappa": loco["level"][1],
                                              "coef": fmt(loco["coef"])},
               "own": {"a": own["a"], "kappa": own["kappa"], "coef": fmt(own["coef"])}, "methods": {}}
        for name, z in s.items():
            res["methods"][name] = agreement(z[keep], y[keep], s["SGW"][keep], groups)
        out[target] = res
        scores[target] = {k: fmt(v) for k, v in s.items()}
        print(f"\n{NAMES[target]} (n={res['n']}; LOCO a={loco['a']}, kappa={loco['kappa']})")
        for name, m in sorted(res["methods"].items(), key=lambda kv: -kv[1]["r"]):
            print(f"  {name:28s} r={m['r']:.3f} [{m['r_ci'][0]:.3f}, {m['r_ci'][1]:.3f}]  rho={m['rho']:.3f}   SGW-this={m['sgw_minus']:+.3f} [{m['sgw_minus_ci'][0]:+.3f}, {m['sgw_minus_ci'][1]:+.3f}]")
    methods = list(next(iter(out.values()))["methods"])
    out["mean_r"] = {m: float(np.mean([out[c]["methods"][m]["r"] for c in corpora if m in out[c]["methods"]])) for m in methods}
    out["mean_r_primary"] = {m: float(np.mean([out[c]["methods"][m]["r"] for c in corpora if c != "yelp" and m in out[c]["methods"]])) for m in methods}
    out["feature_names"] = sgw.FEATURES
    final = fit(sig, corpora, list(corpora), "full")            # shipped constants: every corpus, train + dev
    json.dump({"a": final["a"], "kappa": final["kappa"], "level_a": final["level"][0], "level_kappa": final["level"][1],
               "coef": fmt(final["coef"]), "features": ["intercept"] + sgw.FEATURES,
               "note": "Calibrated readout predicts a human valence rating in standard-deviation units, pooled over six corpora."},
              open(os.path.join(HERE, "..", "sgw", "constants.json"), "w"), indent=1)
    out["shipped"] = {"a": final["a"], "kappa": final["kappa"], "coef": fmt(final["coef"])}
    save("accuracy", out)
    np.savez_compressed(os.path.join(CACHE, "test_scores.npz"), **{f"{c}|{k}": np.array(v) for c in scores for k, v in scores[c].items()})
    print("\nmean r over the five human-rated corpora:")
    for m, v in sorted(out["mean_r_primary"].items(), key=lambda kv: -kv[1]):
        print(f"  {m:28s} {v:.3f}")


def agreement(z, yy, ref, groups):
    """Pearson r and Spearman rho of a score with the ratings, and the signature's r minus this method's, with bootstrap intervals."""
    r = boot(lambda i: pearson(z[i], yy[i]), groups)
    rho = boot(lambda i: spearman(z[i], yy[i]), groups, 500)
    d = boot(lambda i: pearson(ref[i], yy[i]) - pearson(z[i], yy[i]), groups)
    return {"r": fmt(r[0]), "r_ci": [fmt(r[1]), fmt(r[2])], "rho": fmt(rho[0]), "rho_ci": [fmt(rho[1]), fmt(rho[2])],
            "sgw_minus": fmt(d[0]), "sgw_minus_ci": [fmt(d[1]), fmt(d[2])]}


SENT = re.compile(r"(?<=[.!?])\s+")


def vader_sentence_means(texts):
    """VADER's compound score for each sentence, averaged over the text: the use its authors advise for long texts."""
    sents = [[s for s in SENT.split(t.replace("\n", " ")) if s.strip()] or [t] for t in texts]
    flat = vader_scores([s for ss in sents for s in ss])
    ends = np.cumsum([len(ss) for ss in sents])
    return np.array([flat[a:b].mean() for a, b in zip(np.r_[0, ends[:-1]], ends)])


def add_vader_sentences():
    """Add the sentence-averaged VADER comparator to an existing accuracy.json without refitting anything (added after review)."""
    corpora = {c: load() for c, load in LOADERS.items()}
    out = json.load(open(os.path.join(RESULTS, "accuracy.json")))
    scores = np.load(os.path.join(CACHE, "test_scores.npz"))
    name = "VADER, mean of sentences"
    for c in corpora:
        test = corpora[c]["test"]
        z = cached(f"vadersent_{c}_test", lambda: vader_sentence_means(list(test.text)))
        keep = ~np.isnan(scores[f"{c}|DistilBERT (SST-2)"])
        assert keep.sum() == out[c]["n"]
        out[c]["methods"][name] = agreement(z[keep], test.y.values[keep], scores[f"{c}|SGW"][keep], test.group.values[keep])
        m = out[c]["methods"][name]
        print(f"{NAMES[c]:22s} r={m['r']:.3f} [{m['r_ci'][0]:.3f}, {m['r_ci'][1]:.3f}] rho={m['rho']:.3f}  SGW-this={m['sgw_minus']:+.3f} [{m['sgw_minus_ci'][0]:+.3f}, {m['sgw_minus_ci'][1]:+.3f}]  (whole-text VADER r={out[c]['methods']['VADER']['r']:.3f})", flush=True)
    out["mean_r"][name] = float(np.mean([out[c]["methods"][name]["r"] for c in corpora]))
    out["mean_r_primary"][name] = float(np.mean([out[c]["methods"][name]["r"] for c in corpora if c != "yelp"]))
    save("accuracy", out)
    print("mean over the five primary corpora:", round(out["mean_r_primary"][name], 3), "| six:", round(out["mean_r"][name], 3))


if __name__ == "__main__":
    add_vader_sentences() if sys.argv[1:] == ["vader_sentences"] else main()
