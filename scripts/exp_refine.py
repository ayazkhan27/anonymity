"""Refined word charges, leave-one-corpus-out, and a bag-of-words ridge trained on the same pool.

For each target corpus: fit the readout with norm charges on the other corpora, solve one ridge
problem for the charges (penalty chosen on the pool's development data), refit the readout, score
the target's test portion. With no argument, also fits the shipped refined lexicon on all corpora.
"""
from collections import Counter

import numpy as np
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.linear_model import Ridge

from common import *
from sgw import refine as R

A_REF, LAMBDAS = (2, 4, 8, 16), (0.03, 0.1, 0.3)


def stack(corpora, pool, splits):
    texts, z, w, where = [], [], [], []
    for c in pool:
        ref = corpora[c]["train"].y.values
        n = sum(len(corpora[c][s]) for s in splits)
        for s in splits:
            texts += list(corpora[c][s].text)
            z.append((corpora[c][s].y.values - ref.mean()) / ref.std())
            w.append(np.full(len(z[-1]), 1.0 / n))
            where += [(c, s)] * len(z[-1])
    return texts, np.concatenate(z), np.concatenate(w), np.array([f"{c}|{s}" for c, s in where])


def readout(S, a, **kw):
    return sgw.features(S, a, 1.0, two_channel=False, **kw)       # exposure a + words


def fit(corpora, pool, verbose=""):
    texts, z, w, where = stack(corpora, pool, ("train", "dev"))
    tr = np.array([k.endswith("train") for k in where])
    df = Counter(tok for t in np.array(texts, dtype=object)[tr] for tok in set(sgw.tokens(t)))
    extra = [k for k, n in df.items() if n >= 25 and k.replace("'", "").isalpha()]
    sgw.load_lexicon(extra=extra)
    norms = sgw.CHARGE.copy()
    t = sgw.scan(texts)
    S0 = sgw.aggregate(t)
    sub = lambda mask: {k: (v[mask[t["doc"]]] if k != "words" else v[mask]) for k, v in t.items()} | {"doc": np.cumsum(mask)[t["doc"][mask[t["doc"]]]] - 1}

    def dev_r(S, coef, a):
        return float(np.mean([pearson(sgw.predict(coef, readout(S[where == f"{c}|dev"], a)), z[where == f"{c}|dev"]) for c in pool]))
    a = max(A_REF, key=lambda a: dev_r(S0, sgw.ridge(readout(S0[tr], a), z[tr], w[tr]), a))
    t_tr = sub(tr)
    best = (-1,)
    for lam in LAMBDAS:
        sgw.core.CHARGE = norms.copy()
        c = R.refine(t_tr, z[tr], w[tr], a, sgw.ridge(readout(S0[tr], a, position=False), z[tr], w[tr]), lam)
        R.install(c)
        S = sgw.aggregate(R.recharge(t))
        r = dev_r(S, sgw.ridge(readout(S[tr], a), z[tr], w[tr]), a)
        if verbose:
            print(f"  {verbose} a={a} lambda={lam}: mean dev r {r:.4f} (norm charges {dev_r(S0, sgw.ridge(readout(S0[tr], a), z[tr], w[tr]), a):.4f})", flush=True)
        if r > best[0]:
            best = (r, lam)
    lam = best[1]
    sgw.core.CHARGE = norms.copy()                                 # refit on train + dev with the chosen penalty
    c = R.refine(t, z, w, a, sgw.ridge(readout(S0, a, position=False), z, w), lam)
    R.install(c)
    coef = sgw.ridge(readout(sgw.aggregate(R.recharge(t)), a), z, w)
    return dict(a=a, lam=lam, coef=coef, n_words=int((c != 0).sum()), n_new=int(((c != 0) & (norms == 0)).sum()))


def main():
    corpora = {c: load() for c, load in LOADERS.items()}
    out = {}
    for target in corpora:
        done = os.path.join(CACHE, f"refine_{target}.json")       # finished folds are kept
        if not os.path.exists(done):
            pool = [c for c in corpora if c != target]
            test = list(corpora[target]["test"].text)
            m = fit(corpora, pool, verbose=target)
            np.save(os.path.join(CACHE, f"refined_{target}_test.npy"), sgw.predict(m["coef"], readout(sgw.sketch(test), m["a"])))
            texts, z, w, _ = stack(corpora, pool, ("train", "dev"))
            vec = TfidfVectorizer(ngram_range=(1, 2), min_df=3, sublinear_tf=True)
            bow = Ridge(alpha=1.0).fit(vec.fit_transform(texts), z, sample_weight=w / w.mean())
            np.save(os.path.join(CACHE, f"bowpool_{target}_test.npy"), bow.predict(vec.transform(test)))
            y = corpora[target]["test"].y.values
            json.dump(dict(a=m["a"], lam=m["lam"], n_words=m["n_words"], n_new=m["n_new"],
                           r_refined=pearson(np.load(os.path.join(CACHE, f"refined_{target}_test.npy")), y),
                           r_bow_pool=pearson(np.load(os.path.join(CACHE, f"bowpool_{target}_test.npy")), y)), open(done, "w"))
        out[target] = json.load(open(done))
        print(target, out[target], flush=True)
    m = fit(corpora, list(corpora), verbose="all")                 # shipped refined lexicon: every corpus, train + dev
    R.write(os.path.join(HERE, "..", "sgw", "lexicon_refined.tsv"))
    out["shipped"] = dict(a=m["a"], lam=m["lam"], coef=fmt(m["coef"]), n_words=m["n_words"], n_new=m["n_new"])
    out["lambdas"] = list(LAMBDAS)
    save("refine", out)
    sgw.load_lexicon()


if __name__ == "__main__":
    main()
