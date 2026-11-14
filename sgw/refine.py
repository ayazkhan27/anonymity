"""Convex refinement of word charges against rated texts.

With the exposure and the operator multipliers held fixed, a text's predicted rating is
linear in the vector of word charges. The charges that best predict the ratings, pulled
toward the human word norms by a ridge penalty, are therefore the unique solution of one
least-squares problem. Charges stay in hedons: the norms set the unit, the data move the values.
"""
import numpy as np
import scipy.sparse as sp
from sklearn.linear_model import Ridge

from . import core


def design(t, a, theta):
    """Sparse texts x words matrix from a `scan`: each word's operator multiplier over the exposure a + words."""
    m = 1 + theta[0] * t["neg"] + theta[1] * t["boost"] + theta[2] * t["damp"] + theta[3] * t["post"]
    return sp.csr_matrix((m / (a + t["words"])[t["doc"]], (t["doc"], t["id"])), shape=(len(t["words"]), len(core.CHARGE)))


def refine(t, z, weight, a, coef, lam):
    """New charges (hedons, one per word id) from one ridge problem centred on the current charges.

    coef: intercept and the five one-channel coefficients (net, negated, intensified, diminished,
    after contrast) of a readout fitted with the current charges and exposure a + words.
    """
    b0, s0, theta = coef[0], coef[1], np.asarray(coef[2:6]) / coef[1]
    X = design(t, a, theta)
    c0 = core.CHARGE / 1000.0
    v = Ridge(alpha=lam, solver="sparse_cg", tol=1e-4, max_iter=300).fit(X, z - b0 - s0 * (X @ c0), sample_weight=weight / weight.mean()).coef_
    c = np.clip(c0 + v / s0, -1, 1)
    c[(core.KIND & core.WORD) == 0] = 0                        # punctuation carries no charge
    c[(core.KIND & (core.NEG | core.BOOST | core.DAMP | core.CONTR)) > 0] = 0   # operators act on other words
    c[0] = 0                                                   # unknown words stay neutral
    return c


def install(c):
    """Use charges c (hedons per word id) for scoring, with no neutral band."""
    core.CHARGE = np.round(1000 * np.asarray(c)).astype(np.int64)


def recharge(t):
    """A scan with its charges re-read from the installed table (no re-tokenising)."""
    return dict(t, c=core.CHARGE[t["id"]] / 1000.0)


def write(path):
    inv = {v: k for k, v in core._EMO.items()}
    with open(path, "w", encoding="utf-8") as f:
        for w, i in sorted(core.VOCAB.items()):
            if core.KIND[i] & core.WORD and core.CHARGE[i]:
                f.write(f"{inv.get(w, w)}\t{int(core.CHARGE[i])}\n")
