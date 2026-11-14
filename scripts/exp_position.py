"""Tests of the two 2024 hypotheses: front-loading (H1) and dilution (H2).

  where    H1a: where charge sits along a text, by corpus
  kernel   H1b: position kernel fitted against human ratings
  emobank  H1c: human-rated intensity by sentence position inside documents
  dilute   H2: rated intensity against buffer words at fixed charge; fit quality along kappa
  tree     human phrase ratings only (Stanford Sentiment Treebank): how two parts combine,
           and what neutral words do to the rating of the phrase they join
"""
import glob, json, re, sys

import numpy as np
import pandas as pd
from scipy.stats import rankdata

from common import *
from exp_books import BOOKS, chapters

CONST = json.load(open(os.path.join(HERE, "..", "sgw", "constants.json")))
A0, K0 = CONST["a"], CONST["kappa"]
T = np.linspace(-1, 1, 601)
LEG = np.stack([T, (3 * T ** 2 - 1) / 2, (5 * T ** 3 - 3 * T) / 2])
FIRST, LAST = T < -1 / 3, T > 1 / 3


def all_texts(corpora, c):
    """All portions of a corpus. Resampling unit: the source document for EmoBank, the item elsewhere."""
    df = pd.concat([corpora[c][s] for s in SPLITS])
    groups = df.group.values if c == "emobank" else np.arange(len(df))
    return list(df.text), df.y.values, groups, df


def extra_units(corpora):
    """Units without ratings, for the descriptive question only."""
    out = {}
    out["Film review paragraphs"] = [p for t in corpora["scale"]["test"].text for p in t.split("\n") if len(p.split()) >= 10]
    ch = []
    for path in sorted(glob.glob(os.path.join(BOOKS, "*.txt")))[:150]:
        ch += [" ".join(c) for c in chapters(path)]
    out["Novel chapters"] = ch
    return out


def where(corpora):
    units = {NAMES[c]: (all_texts(corpora, c)[0], all_texts(corpora, c)[2]) for c in corpora}
    units.update({k: (v, np.arange(len(v))) for k, v in extra_units(corpora).items()})
    out = {}
    for name, (texts, groups) in units.items():
        t = sgw.scan(texts)
        n = t["words"]
        ok = n[t["doc"]] >= 10                                    # positions are too coarse in very short texts
        d, x = t["doc"][ok], (t["pos"][ok] + 0.5) / n[t["doc"]][ok]
        g = np.abs(t["c"][ok])
        nd = len(n)
        res = {"n_texts": int((n >= 10).sum())}
        for key, wgt in (("gross", g), ("hedons", np.maximum(t["c"][ok], 0)), ("dolors", np.maximum(-t["c"][ok], 0)), ("charged words", (g > 0) * 1.0)):
            m1 = np.bincount(d, weights=wgt * (2 * x - 1), minlength=nd)   # first Legendre moment per text
            m0 = np.bincount(d, weights=wgt, minlength=nd)
            est = boot(lambda i: m1[i].sum() / m0[i].sum(), groups, 1000)
            res[key] = [fmt(v) for v in est]
        bins = np.minimum((x * 10).astype(int), 9)
        dens = np.bincount(bins, weights=g, minlength=10) / np.bincount(bins, minlength=10)
        res["decile_density"] = (dens / dens.mean()).tolist()
        out[name] = res
        print(f"{name:24s} n={res['n_texts']:6d}  charge-weighted mean position {res['gross'][0]:+.4f} [{res['gross'][1]:+.4f}, {res['gross'][2]:+.4f}]"
              f"   hd {res['hedons'][0]:+.3f}  dl {res['dolors'][0]:+.3f}", flush=True)
    return out


def kernel(corpora):
    out = {}
    for c in corpora:
        texts, y, groups, _ = all_texts(corpora, c)
        S = cached(f"sigall_{c}", lambda: sgw.sketch(texts))
        ok = S[:, sgw.L] >= 10
        X = sgw.features(S[ok], A0, K0, two_channel=False)       # net, net x P1..P3, shifters
        z = (y[ok] - y[ok].mean()) / y[ok].std()
        g = groups[ok]

        def stat(i):
            b = sgw.ridge(X[i], z[i])[1:5]
            k = 1 + (b[1:4] / b[0]) @ LEG
            return np.concatenate(([k[FIRST].mean() - k[LAST].mean()], k[::20]))
        est, lo, hi = boot(stat, g, 1000)
        out[NAMES[c]] = dict(n=int(ok.sum()), contrast=[fmt(est[0]), fmt(lo[0]), fmt(hi[0])], kernel=est[1:].tolist(),
                             lo=lo[1:].tolist(), hi=hi[1:].tolist(), x=((T[::20] + 1) / 2).tolist())
        print(f"{NAMES[c]:22s} n={ok.sum():6d}  first third minus last third: {est[0]:+.3f} [{lo[0]:+.3f}, {hi[0]:+.3f}]", flush=True)
    return out


def emobank(corpora):
    df = pd.concat([corpora["emobank"][s] for s in SPLITS]).sort_values(["group", "start"])
    df = df[df.category != "SemEval"]                             # headlines are one-sentence documents
    df["n"] = df.groupby("group").text.transform("size")
    df = df[df.n >= 8].copy()
    df["u"] = (df.groupby("group").cumcount() + 0.5) / df.n
    df["intensity"] = (df.y - 3).abs()
    S = sgw.sketch(list(df.text))
    df["sgw_gross"] = 1000 * (S[:, sgw.P0] + S[:, sgw.N0]) / np.maximum(S[:, sgw.L], 1)

    def slope(d, col):
        u, v = d.u - d.groupby("group").u.transform("mean"), d[col] - d.groupby("group")[col].transform("mean")
        return float((u * v).sum() / (u * u).sum())
    out = {}
    for cat in ["all"] + sorted(df.category.unique()):
        d = df if cat == "all" else df[df.category == cat]
        ids = d.group.unique()
        by = {g: x for g, x in d.groupby("group")}
        rng = np.random.default_rng(0)
        res = {}
        for col in ("intensity", "y", "sgw_gross"):
            draws = [slope(pd.concat([by[g] for g in rng.choice(ids, len(ids))], keys=range(len(ids))).reset_index(level=0).assign(group=lambda t: t.level_0), col) for _ in range(500)]
            res[col] = [slope(d, col)] + [float(v) for v in np.percentile(draws, [2.5, 97.5])]
        first = d[d.groupby("group").cumcount() == 0].intensity.mean()
        out[cat] = dict(n_docs=int(len(ids)), n_sentences=int(len(d)), first_sentence_intensity=float(first), other_intensity=float(d[d.groupby("group").cumcount() > 0].intensity.mean()), **res)
        print(f"{cat:14s} docs={len(ids):4d} sents={len(d):5d}  slope of |V-3| on position {res['intensity'][0]:+.3f} [{res['intensity'][1]:+.3f}, {res['intensity'][2]:+.3f}]", flush=True)
    return out


def dilute(corpora):
    out = {}
    for c in corpora:
        texts, y, groups, df = all_texts(corpora, c)
        S = cached(f"sigall_{c}", lambda: sgw.sketch(texts))
        inten = np.abs(y - np.median(y))
        G, B = S[:, sgw.P0] + S[:, sgw.N0], S[:, sgw.L] - S[:, sgw.E]
        ok = S[:, sgw.E] >= 1

        def partial(i):                                           # rank correlation of intensity with buffer count, charge held fixed
            ri, rb, rg = (rankdata(v[i]) for v in (inten[ok], B[ok], G[ok]))
            res = lambda a: a - np.polyval(np.polyfit(rg, a, 1), rg)
            return pearson(res(ri), res(rb))
        est = boot(partial, groups[ok], 500 if len(y) < 50000 else 100)
        # fit quality along kappa (in-corpus constants; a fixed at the shipped value)
        tr = np.concatenate([np.zeros(len(corpora[c][s]), bool) if s == "test" else np.ones(len(corpora[c][s]), bool) for s in SPLITS])
        z = (y - y[tr].mean()) / y[tr].std()
        curve = []
        for k in (0, 0.05, 0.1, 0.2, 0.5, 1, 2, 4):
            X = sgw.features(S, A0, k)
            curve.append(pearson(sgw.predict(sgw.ridge(X[tr], z[tr]), X[~tr]), y[~tr]))
        out[NAMES[c]] = dict(partial=[fmt(v) for v in est], kappa=[0, 0.05, 0.1, 0.2, 0.5, 1, 2, 4], r_by_kappa=curve)
        print(f"{NAMES[c]:22s} partial rank corr(intensity, buffer words | charge) {est[0]:+.3f} [{est[1]:+.3f}, {est[2]:+.3f}]   r by kappa " + " ".join(f"{v:.3f}" for v in curve), flush=True)
    return out


def tree_nodes(parts):
    """Binary nodes of treebank trees with human ratings for the phrase, its earlier part and its later part."""
    lab = dict(zip(*[pd.read_parquet(os.path.join(HERE, "..", "data", "sst", "dictionary_dictionary.parquet"))[k] for k in ("phrase", "label")]))
    fix = {"-LRB-": "(", "-RRB-": ")"}
    rows, seen, total, sid = [], 0, 0, 0
    for part in parts:
        for t in pd.read_parquet(os.path.join(HERE, "..", "data", "sst", f"ptb_{part}.parquet")).ptb_tree:
            sid += 1
            stack = []
            for tok in re.findall(r"\(|\)|[^\s()]+", t):
                if tok == "(":
                    stack.append([])
                elif tok == ")":
                    node = stack.pop()                            # [label, child, child] or [label, word]
                    kids = node[1:]
                    words = [w for k in kids for w in (k if isinstance(k, list) else [k])]
                    if len(kids) == 2:
                        total += 1
                        l, r = (k if isinstance(k, list) else [k] for k in kids)
                        key = [" ".join(fix.get(w, w).replace("\\/", "/") for w in x) for x in (words, l, r)]
                        if all(k in lab for k in key):
                            seen += 1
                            rows.append((sid, lab[key[0]] - 0.5, lab[key[1]] - 0.5, lab[key[2]] - 0.5, len(l), len(r)))
                    if stack:
                        stack[-1].append(words)
                else:
                    stack[-1].append(tok)
    return pd.DataFrame(rows, columns=["sid", "p", "l", "r", "nl", "nr"]), seen / total


def neutral_pairs(d, neutral=0.05, strong=0.15):
    """A rated part joined to a neutral part: signed phrase rating, absolute part rating, neutral words, side."""
    rec = []
    for side, (c, o, n) in (("neutral words after", ("l", "r", "nr")), ("neutral words before", ("r", "l", "nl"))):
        x = d[(d[c].abs() >= strong) & (d[o].abs() <= neutral)]
        rec.append(pd.DataFrame({"sid": x.sid, "ratio_num": x.p * np.sign(x[c]), "ratio_den": x[c].abs(), "m": x[n], "side": side}))
    return pd.concat(rec)


def tree():
    """Training trees: how two parts combine, and what neutral words do (exploratory)."""
    d, match = tree_nodes(["train"])
    g = d.sid.values

    def ols(i, sub=None):
        x = d.iloc[i] if sub is None else d.iloc[i][sub[i]]
        X = np.column_stack([np.ones(len(x)), x.l, x.r])
        b = np.linalg.lstsq(X, x.p.values, rcond=None)[0]
        return np.array([b[1], b[2], b[2] - b[1]])
    out = {"n_nodes": int(len(d)), "match_rate": match}
    for name, sub in (("all nodes", None), ("both parts two or more words", ((d.nl >= 2) & (d.nr >= 2)).values), ("top node of each sentence", (d.groupby("sid").cumcount(ascending=False) == 0).values)):
        est, lo, hi = boot(lambda i: ols(i, sub), g, 500)
        out[name] = dict(left=[est[0], lo[0], hi[0]], right=[est[1], lo[1], hi[1]], right_minus_left=[est[2], lo[2], hi[2]])
        print(f"{name:32s} left {est[0]:.3f}  right {est[1]:.3f}  right-left {est[2]:+.3f} [{lo[2]:+.3f}, {hi[2]:+.3f}]", flush=True)
    x = neutral_pairs(d)                                          # dilution from human ratings alone
    bins = [(1, 1), (2, 2), (3, 4), (5, 8), (9, 99)]
    out["dilution"] = {}
    for side in ("neutral words after", "neutral words before", "either"):
        xs = x if side == "either" else x[x.side == side]
        cur = []
        for lo_m, hi_m in bins:
            b = xs[(xs.m >= lo_m) & (xs.m <= hi_m)]
            est = boot(lambda i: b.ratio_num.values[i].sum() / b.ratio_den.values[i].sum(), b.sid.values, 500)
            cur.append(dict(words=f"{lo_m}" if lo_m == hi_m else (f"{lo_m}-{hi_m}" if hi_m < 99 else f"{lo_m}+"), n=int(len(b)), kept=[fmt(v) for v in est]))
        out["dilution"][side] = cur
        print(f"{side:22s} share of rating kept, by number of neutral words: " + "  ".join(f"{c['words']}: {c['kept'][0]:.2f} (n={c['n']})" for c in cur), flush=True)
    return out


def confirm():
    """Addendum 2: the recency hypothesis on data that did not suggest it."""
    from sklearn.metrics import roc_auc_score
    d, match = tree_nodes(["validation", "test"])
    two = d[(d.nl >= 2) & (d.nr >= 2)].reset_index(drop=True)

    def weights(i):
        x = two.iloc[i]
        b = np.linalg.lstsq(np.column_stack([np.ones(len(x)), x.l, x.r]), x.p.values, rcond=None)[0]
        return np.array([b[1], b[2], b[2] - b[1]])
    w = boot(weights, two.sid.values, 2000)
    x = neutral_pairs(d)
    x = x[x.m >= 2].reset_index(drop=True)
    after, before = (x.side == "neutral words after").values, (x.side == "neutral words before").values

    def kept(i):
        num, den, a, b = x.ratio_num.values[i], x.ratio_den.values[i], after[i], before[i]
        ka, kb = num[a].sum() / den[a].sum(), num[b].sum() / den[b].sum()
        return np.array([ka, kb, kb - ka])
    k = boot(kept, x.sid.values, 2000)
    df = pd.read_parquet(os.path.join(HERE, "..", "data", "imdb", "test.parquet"))
    t = sgw.scan(list(df.text))
    n = t["words"]
    xr = (t["pos"] + 0.5) / n[t["doc"]]
    dens = {}
    for name, m in (("first", xr < 0.2), ("last", xr >= 0.8)):
        dens[name] = np.bincount(t["doc"][m], weights=t["c"][m], minlength=len(n)) / np.maximum(np.bincount(t["doc"][m], minlength=len(n)), 1)
    ok = n >= 100
    y, f, l = df.label.values[ok], dens["first"][ok], dens["last"][ok]
    a = boot(lambda i: np.array([roc_auc_score(y[i], f[i]), roc_auc_score(y[i], l[i]), roc_auc_score(y[i], l[i]) - roc_auc_score(y[i], f[i])]), np.arange(len(y)), 1000)
    verdict = lambda lo, hi: "supported" if lo > 0 else "refuted" if hi < 0 else "undecided"
    out = dict(n_nodes=int(len(two)), n_sentences=int(d.sid.nunique()), match_rate=match,
               R1=dict(earlier=fmt(w[0][0]), later=fmt(w[0][1]), diff=[fmt(w[0][2]), fmt(w[1][2]), fmt(w[2][2])], verdict=verdict(w[1][2], w[2][2])),
               R2=dict(n_after=int(after.sum()), n_before=int(before.sum()), kept_after=[fmt(k[0][0]), fmt(k[1][0]), fmt(k[2][0])], kept_before=[fmt(k[0][1]), fmt(k[1][1]), fmt(k[2][1])],
                       diff=[fmt(k[0][2]), fmt(k[1][2]), fmt(k[2][2])], verdict=verdict(k[1][2], k[2][2])),
               R3=dict(n_reviews=int(ok.sum()), auc_first=[fmt(a[0][0]), fmt(a[1][0]), fmt(a[2][0])], auc_last=[fmt(a[0][1]), fmt(a[1][1]), fmt(a[2][1])],
                       diff=[fmt(a[0][2]), fmt(a[1][2]), fmt(a[2][2])], verdict=verdict(a[1][2], a[2][2])))
    for r in ("R1", "R2", "R3"):
        print(r, {kk: (vv if not isinstance(vv, list) else [round(q, 3) for q in vv]) for kk, vv in out[r].items()})
    return out


if __name__ == "__main__":
    corpora = {c: load() for c, load in LOADERS.items()}
    todo = sys.argv[1:] or ("where", "kernel", "emobank", "dilute", "tree")
    path = os.path.join(RESULTS, "position.json")
    res = json.load(open(path)) if os.path.exists(path) else {}
    for name in todo:
        print(f"\n== {name}")
        res[name] = tree() if name == "tree" else confirm() if name == "confirm" else dict(where=where, kernel=kernel, emobank=emobank, dilute=dilute)[name](corpora)
        save("position", res)
