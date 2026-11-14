"""Do the scores come with honest error bars? (exploratory)

For each target corpus, the leave-one-corpus-out readout is applied to the other corpora's
training data; squared errors there are regressed on 1/exposure (non-negative least squares)
to give a predictive variance s0 + s1/exposure. On the target's test portion we report how
often the 90% interval covers the human rating, and the error by exposure band.
"""
import json
import numpy as np
from scipy.optimize import nnls

from common import *


def main():
    corpora = {c: load() for c, load in LOADERS.items()}
    sig = signatures(corpora)
    acc = json.load(open(os.path.join(RESULTS, "accuracy.json")))
    z = lambda c, s: (corpora[c][s].y.values - corpora[c]["train"].y.mean()) / corpora[c]["train"].y.std()
    out = {}
    for target in corpora:
        m = acc[target]["loco"]
        coef, a, k = np.array(m["coef"]), m["a"], m["kappa"]
        pool = [c for c in corpora if c != target]
        e2 = np.concatenate([(z(c, "train") - sgw.predict(coef, sgw.features(sig[c]["train"], a, k))) ** 2 for c in pool])
        D = np.concatenate([sgw.exposure(sig[c]["train"], a, k) for c in pool])
        w = np.concatenate([np.full(len(corpora[c]["train"]), 1 / len(corpora[c]["train"])) for c in pool])
        (s0, s1), _ = nnls(np.column_stack([np.ones(len(D)), 1 / D]) * np.sqrt(w)[:, None], e2 * np.sqrt(w))
        St = sig[target]["test"]
        pred, Dt, y = sgw.predict(coef, sgw.features(St, a, k)), sgw.exposure(St, a, k), z(target, "test")
        # the target's rating scale is unknown to the pool: put the prediction on it with a two-number linear map
        slope, icpt = np.polyfit(pred, y, 1)
        err, sd = y - (icpt + slope * pred), np.sqrt(s0 + s1 / Dt)
        scale = np.sqrt(np.mean(err ** 2) / np.mean(sd ** 2))     # one global variance scale per corpus
        cover = float(np.mean(np.abs(err) <= 1.645 * scale * sd))
        cuts = np.quantile(Dt, [0, 0.25, 0.5, 0.75, 1.0])
        bands = [dict(exposure=float(np.median(Dt[(Dt >= lo) & (Dt <= hi)])), rmse=float(np.sqrt(np.mean(err[(Dt >= lo) & (Dt <= hi)] ** 2))),
                      predicted=float(scale * np.sqrt(np.mean(sd[(Dt >= lo) & (Dt <= hi)] ** 2))),
                      cover=float(np.mean(np.abs(err[(Dt >= lo) & (Dt <= hi)]) <= 1.645 * scale * sd[(Dt >= lo) & (Dt <= hi)])))
                 for lo, hi in zip(cuts[:-1], cuts[1:])]
        out[target] = dict(s0=float(s0), s1=float(s1), coverage90=cover, bands=bands, corr_abs_err_sd=spearman(np.abs(err), sd))
        print(f"{NAMES[target]:22s} s0={s0:.2f} s1={s1:.2f}  90% interval covers {cover:.3f}   |error| vs predicted sd: rho={out[target]['corr_abs_err_sd']:+.3f}   "
              + "  ".join(f"D~{b['exposure']:.0f}: rmse {b['rmse']:.2f} vs {b['predicted']:.2f}" for b in bands), flush=True)
    save("uncertainty", out)


def constants_ci(n=500):
    """Bootstrap intervals for the shipped composition constants (texts resampled within each corpus)."""
    corpora = {c: load() for c, load in LOADERS.items()}
    sig = signatures(corpora)
    k = json.load(open(os.path.join(HERE, "..", "sgw", "constants.json")))
    X, y, w, idx = [], [], [], []
    for c in corpora:
        for s_ in ("train", "dev"):
            X.append(sgw.features(sig[c][s_], k["a"], k["kappa"]))
            y.append((corpora[c][s_].y.values - corpora[c]["train"].y.mean()) / corpora[c]["train"].y.std())
        m = sum(len(corpora[c][s_]) for s_ in ("train", "dev"))
        w.append(np.full(m, 1.0 / m)); idx.append(m)
    X, y, w = np.vstack(X), np.concatenate(y), np.concatenate(w)
    starts = np.concatenate(([0], np.cumsum(idx)))

    def derived(b):
        base = (b[1] - b[2]) / 2
        g = 1 + (np.array([b[3], b[4], b[5]]) / base) @ LEG          # position weight along the text
        return np.array([-b[2] / b[1], 1 + b[6] / b[1], 1 + b[7] / b[2], b[8] / base, b[9] / base, b[10] / base, g[FIRST].mean() - g[LAST].mean()])
    rng = np.random.default_rng(0)
    est = derived(sgw.ridge(X, y, w))
    draws = []
    for _ in range(n):
        i = np.concatenate([rng.integers(a, b, b - a) for a, b in zip(starts[:-1], starts[1:])])
        draws.append(derived(sgw.ridge(X[i], y[i], w[i])))
    lo, hi = np.percentile(draws, [2.5, 97.5], axis=0)
    names = ["dolor_over_hedon", "negated_hedon_multiplier", "negated_dolor_multiplier", "intensifier_gain", "diminisher_gain", "after_contrast_gain", "first_minus_last_third"]
    out = {nm: [float(e), float(a), float(b)] for nm, e, a, b in zip(names, est, lo, hi)}
    save("constants_ci", out)
    for nm, v in out.items():
        print(f"{nm:28s} {v[0]:+.2f} [{v[1]:+.2f}, {v[2]:+.2f}]")


T = np.linspace(-1, 1, 601)
LEG = np.stack([T, (3 * T ** 2 - 1) / 2, (5 * T ** 3 - 3 * T) / 2])
FIRST, LAST = T < -1 / 3, T > 1 / 3

if __name__ == "__main__":
    main()
    constants_ci()
