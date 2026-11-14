"""Development-set exploration (never touches test portions). Pooled fit on train, mean Pearson r on dev.

    python3 scripts/dev_explore.py
"""
import itertools
import numpy as np
from common import *

A, KAPPA = (0.5, 1, 2, 4, 8, 16, 32), (0, 0.05, 0.1, 0.2, 0.5, 1, 2)


def run(label, lexicon=None, delta=250, neg=3, mod=2):
    sgw.core.DELTA, sgw.core.NEG_WINDOW, sgw.core.MOD_WINDOW = delta, neg, mod
    sgw.load_lexicon(lexicon)
    sig = {c: {s: sgw.sketch(list(corpora[c][s].text)) for s in ("train", "dev")} for c in corpora}
    best = (-1,)
    for a, k in itertools.product(A, KAPPA):
        X = np.vstack([sgw.features(sig[c]["train"], a, k) for c in corpora])
        y = np.concatenate([(corpora[c]["train"].y - corpora[c]["train"].y.mean()) / corpora[c]["train"].y.std() for c in corpora])
        w = np.concatenate([np.full(len(corpora[c]["train"]), 1 / len(corpora[c]["train"])) for c in corpora])
        coef = sgw.ridge(X, y, w)
        rs = [pearson(sgw.predict(coef, sgw.features(sig[c]["dev"], a, k)), corpora[c]["dev"].y.values) for c in corpora]
        if np.mean(rs) > best[0]:
            best = (np.mean(rs), a, k, rs)
    cover = np.mean([sig[c]["dev"][:, sgw.E].sum() / sig[c]["dev"][:, sgw.L].sum() for c in corpora])
    print(f"{label:34s} mean dev r {best[0]:.4f}  a={best[1]} kappa={best[2]}  charged share {cover:.3f}  " + " ".join(f"{r:.3f}" for r in best[3]), flush=True)


if __name__ == "__main__":
    corpora = {c: load() for c, load in LOADERS.items()}
    corpora["yelp"] = {"train": corpora["yelp"]["train"].iloc[:20000], "dev": corpora["yelp"]["dev"].iloc[:5000]}
    print("columns:", " ".join(corpora))
    run("open lexicon, delta 0.25")
    run("extended lexicon, delta 0.25", os.path.join(HERE, "..", "data", "lex_restricted", "extended.tsv"))
    for d in (0, 125, 375):
        run(f"open, delta {d/1000}", delta=d)
    for n in (2, 4):
        run(f"open, negation window {n}", neg=n)
    run("open, modifier window 1", mod=1)
