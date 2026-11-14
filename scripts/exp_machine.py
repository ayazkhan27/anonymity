"""Human and machine writing on the same prompts (addendum to the analysis plan).

  hc3        24k questions answered by people and by ChatGPT (Guo et al. 2023)
  essays     160 student essays (PERSUADE 2.0) against essays by Claude models on the same assignments
  reference  human ranges of the ten tone measures by kind of text -> sgw/reference.json
  jobs       write the revision jobs for the tone-check test
  revise     score the revisions (after the subagents have written them)
"""
import json, re, sys

import numpy as np
import pandas as pd
from sklearn.impute import SimpleImputer
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import roc_auc_score
from sklearn.model_selection import GroupKFold
from sklearn.pipeline import make_pipeline
from sklearn.preprocessing import StandardScaler

from common import *
from sgw import tone

DATA = os.path.join(HERE, "..", "data")
MT = os.path.join(HERE, "..", "machine_text")
M = tone.MEASURES
HYP = {"HL1": ("net", 1), "HL2": ("dolors", -1), "HL3": ("clustering", -1), "HL4": ("ending_lift", 1)}
GLUE = re.compile(r"\s+(n't|'s|'re|'ve|'ll|'d|'m)\b")           # HC3's human answers are pre-tokenised


def paired(Xh, Xm, groups, n=2000):
    """Machine minus human on the same prompt: mean difference per measure, 95% interval, and the same in human s.d. units."""
    d, sd = Xm - Xh, np.nanstd(Xh, 0)
    est, lo, hi = boot(lambda i: np.nanmean(d[i], 0), groups, n)
    return {m: dict(human=float(np.nanmean(Xh[:, j])), machine=float(np.nanmean(Xm[:, j])), diff=float(est[j]), lo=float(lo[j]), hi=float(hi[j]),
                    d=float(est[j] / sd[j]), d_lo=float(lo[j] / sd[j]), d_hi=float(hi[j] / sd[j])) for j, m in enumerate(M)}


def classifier():
    return make_pipeline(SimpleImputer(strategy="median"), StandardScaler(), LogisticRegression(max_iter=2000))


def auc(Xh, Xm, groups, folds=10):
    """Out-of-fold AUC of a logistic regression; a prompt's texts stay in one fold; interval by resampling prompts."""
    X, y, g = np.vstack([Xh, Xm]), np.r_[np.zeros(len(Xh)), np.ones(len(Xm))], np.r_[groups, groups]
    score = np.zeros(len(y))
    for tr, te in GroupKFold(n_splits=min(folds, len(np.unique(g)))).split(X, y, g):
        score[te] = classifier().fit(X[tr], y[tr]).predict_proba(X[te])[:, 1]
    est, lo, hi = boot(lambda i: roc_auc_score(y[i], score[i]), g, 1000)
    return [float(est), float(lo), float(hi)]


def decisions(res):
    out = {}
    for h, (m, sign) in HYP.items():
        lo, hi = res[m]["lo"], res[m]["hi"]
        out[h] = "supported" if sign * lo > 0 and sign * hi > 0 else "refuted" if sign * lo < 0 and sign * hi < 0 else "undecided"
    return out


def load_hc3(min_words=30):
    rows = []
    for line in open(f"{DATA}/hc3/all.jsonl", encoding="utf-8"):
        d = json.loads(line)
        if d["human_answers"] and d["chatgpt_answers"]:
            rows.append((d.get("source", ""), GLUE.sub(r"\1", d["human_answers"][0]), d["chatgpt_answers"][0]))
    df = pd.DataFrame(rows, columns=["source", "human", "machine"])
    Xh, nh = tone.profiles(df.human)
    Xm, nm = tone.profiles(df.machine)
    ok = (nh >= min_words) & (nm >= min_words)
    return df[ok].reset_index(drop=True), Xh[ok], Xm[ok], nh[ok], nm[ok]


def hc3():
    df, Xh, Xm, nh, nm = load_hc3()
    q = np.arange(len(df))
    res = paired(Xh, Xm, q, 1000)
    out = dict(n=int(len(df)), words_human=float(np.median(nh)), words_machine=float(np.median(nm)), measures=res, decisions=decisions(res),
               auc=auc(Xh, Xm, q), auc_words=auc(np.log(nh)[:, None], np.log(nm)[:, None], q), by_source={})
    for s, idx in df.groupby("source").indices.items():
        r = paired(Xh[idx], Xm[idx], np.arange(len(idx)), 500)
        out["by_source"][s] = dict(n=int(len(idx)), d={m: r[m]["d"] for m in M}, decisions=decisions(r))
    np.savez_compressed(os.path.join(CACHE, "hc3_profiles.npz"), Xh=Xh, Xm=Xm, nh=nh, nm=nm)
    print(f"HC3: {out['n']} questions; AUC {out['auc'][0]:.3f} [{out['auc'][1]:.3f}, {out['auc'][2]:.3f}]; word count alone {out['auc_words'][0]:.3f}")
    for m in M:
        print(f"  {m:15s} human {res[m]['human']:7.2f}  machine {res[m]['machine']:7.2f}  d = {res[m]['d']:+.2f} [{res[m]['d_lo']:+.2f}, {res[m]['d_hi']:+.2f}]")
    print(" ", out["decisions"], {s: v["decisions"] for s, v in out["by_source"].items()})
    return out


def load_essays():
    s = pd.read_csv(f"{MT}/sample.csv")
    corpus = pd.read_parquet(f"{DATA}/persuade/essays.parquet").set_index("essay_id_comp")
    s["human"] = [corpus.loc[e].full_text for e in s.essay_id]
    for c in "AB":
        s[c] = [open(p, encoding="utf-8").read() if os.path.exists(p) else None for p in (f"{MT}/essays/{c}/{k:03d}_{t}.txt" for k, t in zip(s.k, s.tier))]
    return s


def essays():
    s = load_essays()
    out = {"n_sampled": int(len(s))}
    prof = {}
    for c in "AB":
        d = s[s[c].notna()].reset_index(drop=True)
        Xh, nh = tone.profiles(d.human)
        Xm, nm = tone.profiles(d[c])
        prof[c] = (d, Xh, Xm)
        g = d.prompt.values
        res, res_item = paired(Xh, Xm, g), paired(Xh, Xm, np.arange(len(d)))
        out[c] = dict(n=int(len(d)), words_human=float(np.median(nh)), words_machine=float(np.median(nm)), measures=res, decisions=decisions(res),
                      decisions_resampling_essays=decisions(res_item), d_ci_resampling_essays={m: [res_item[m]["d_lo"], res_item[m]["d_hi"]] for m in M},
                      auc=auc(Xh, Xm, g, 8), auc_words=auc(np.log(nh)[:, None], np.log(nm)[:, None], g, 8),
                      by_tier={t: {m: float(np.nanmean((Xm - Xh)[idx, j]) / np.nanstd(Xh[:, j])) for j, m in enumerate(M)} for t, idx in d.groupby("tier").indices.items()})
        print(f"\nEssays, condition {c}: {len(d)} pairs; AUC {out[c]['auc'][0]:.3f} [{out[c]['auc'][1]:.3f}, {out[c]['auc'][2]:.3f}]; word count alone {out[c]['auc_words'][0]:.3f}")
        for m in M:
            print(f"  {m:15s} human {res[m]['human']:7.2f}  machine {res[m]['machine']:7.2f}  d = {res[m]['d']:+.2f} [{res[m]['d_lo']:+.2f}, {res[m]['d_hi']:+.2f}]")
        print(" ", out[c]["decisions"], "| resampling essays:", out[c]["decisions_resampling_essays"])
    # does a classifier trained on people versus ChatGPT (2023 answers) carry over to students versus Claude (2026 essays)?
    z = np.load(os.path.join(CACHE, "hc3_profiles.npz"))
    clf = classifier().fit(np.vstack([z["Xh"], z["Xm"]]), np.r_[np.zeros(len(z["Xh"])), np.ones(len(z["Xm"]))])
    for c in "AB":
        d, Xh, Xm = prof[c]
        y, sc = np.r_[np.zeros(len(Xh)), np.ones(len(Xm))], clf.predict_proba(np.vstack([Xh, Xm]))[:, 1]
        out[c]["auc_trained_on_hc3"] = float(roc_auc_score(y, sc))
        print(f"condition {c}: classifier trained on HC3 gives AUC {out[c]['auc_trained_on_hc3']:.3f} on the essays")
    return out


def stats(X):
    X = X[~np.isnan(X).any(1)] if False else X
    out = {}
    for j, m in enumerate(M):
        v = X[:, j][~np.isnan(X[:, j])]
        q = np.percentile(v, [5, 10, 25, 50, 75, 90, 95])
        out[m] = dict(mean=round(float(v.mean()), 3), sd=round(float(v.std()), 3), **{f"p{p}": round(float(x), 2) for p, x in zip((5, 10, 25, 50, 75, 90, 95), q)})
    return out


def reference():
    """Human ranges by kind of text, shipped with the package for the tone check."""
    import glob
    from exp_books import BOOKS, chapters
    ref = {}
    corpus = pd.read_parquet(f"{DATA}/persuade/essays.parquet")
    X, n = tone.profiles(corpus.full_text)
    ref["essay"] = dict(stats(X[n >= 100]), n=int((n >= 100).sum()), source="PERSUADE 2.0 student essays, grades 6 to 12")
    z = np.load(os.path.join(CACHE, "hc3_profiles.npz"))
    ref["answer"] = dict(stats(z["Xh"]), n=int(len(z["Xh"])), source="HC3 human answers")
    y = pd.read_parquet(f"{DATA}/yelp/test.parquet").text.values[:20000]
    X, n = tone.profiles(y)
    ref["review"] = dict(stats(X[n >= 50]), n=int((n >= 50).sum()), source="Yelp reviews")
    ch = [" ".join(c) for p in sorted(glob.glob(os.path.join(BOOKS, "*.txt")))[:150] for c in chapters(p)][:3000]
    X, n = tone.profiles(ch)
    ref["fiction"] = dict(stats(X), n=int(len(ch)), source="chapters of Project Gutenberg novels")
    json.dump(ref, open(os.path.join(HERE, "..", "sgw", "reference.json"), "w"), indent=1)
    for g, r in ref.items():
        print(f"{g:8s} n={r['n']:6d}  " + "  ".join(f"{m} {r[m]['p50']}" for m in M))
    return {g: r["n"] for g, r in ref.items()}


def essay_reference(exclude_prompt):
    corpus = pd.read_parquet(f"{DATA}/persuade/essays.parquet")
    X, n = tone.profiles(corpus[corpus.prompt_name != exclude_prompt].full_text)
    return stats(X[n >= 100])


def jobs(n_per_tier=15):
    """Revision jobs: each sampled condition-A essay is revised twice, with and without the tone report."""
    s = load_essays()
    s = s[s.A.notna()]
    pick = pd.concat([g.sample(n_per_tier, random_state=0) for _, g in s.groupby("tier")]).sort_values("k")
    os.makedirs(f"{MT}/revision_jobs", exist_ok=True)
    for arm in ("control", "report"):
        os.makedirs(f"{MT}/revisions/{arm}", exist_ok=True)
    refs = {p: essay_reference(p) for p in pick.prompt.unique()}
    root = os.path.abspath(MT)
    for i, r in enumerate(pick.itertuples()):
        who = f"a US grade {int(r.grade)} student" if not np.isnan(r.grade) else "a US high school student"
        base = (f"Below is a draft essay. Revise it so that it reads as if {who} wrote it in class for a timed writing test. "
                f"Keep the position it argues and keep it at about {len(r.A.split())} words. The file must contain only the revised essay.")
        rep = tone.report(r.A, "essay", refs[r.prompt])
        for arm, extra in (("control", ""), ("report", "\n\nA measurement tool compared the draft with essays that students wrote on other assignments. Use its report when you revise:\n\n" + rep)):
            json.dump({"instructions": base + extra + "\n\nDraft:\n\n" + r.A, "output_path": f"{root}/revisions/{arm}/{r.k:03d}_{r.tier}.txt"},
                      open(f"{MT}/revision_jobs/{2 * i + (arm == 'report'):03d}.json", "w"), indent=1)
    pick[["k", "essay_id", "prompt", "grade", "words", "tier"]].to_csv(f"{MT}/revision_sample.csv", index=False)
    print(len(pick), "essays,", 2 * len(pick), "revision jobs")
    return {"n": int(len(pick))}


def revise():
    """HT1: is the draft closer to the human range after a revision that saw the report than after one that did not?"""
    pick = pd.read_csv(f"{MT}/revision_sample.csv")
    s = load_essays().set_index("k").loc[pick.k].reset_index()
    texts = {"draft": list(s.A)}
    for arm in ("control", "report"):
        texts[arm] = [open(f"{MT}/revisions/{arm}/{k:03d}_{t}.txt", encoding="utf-8").read() if os.path.exists(f"{MT}/revisions/{arm}/{k:03d}_{t}.txt") else None for k, t in zip(s.k, s.tier)]
    ok = np.array([a is not None and b is not None for a, b in zip(texts["control"], texts["report"])])
    s = s[ok].reset_index(drop=True)
    refs = {p: essay_reference(p) for p in s.prompt.unique()}
    mu = np.array([[refs[p][m]["mean"] for m in M] for p in s.prompt]); sd = np.array([[refs[p][m]["sd"] for m in M] for p in s.prompt])
    Xh, _ = tone.profiles(s.human)
    z = np.load(os.path.join(CACHE, "hc3_profiles.npz"))
    corpus = pd.read_parquet(f"{DATA}/persuade/essays.parquet")
    dist, words, prof = {}, {}, {}
    for arm in ("draft", "control", "report"):
        X, n = tone.profiles([t for t, keep in zip(texts[arm], ok) if keep])
        prof[arm] = X
        dist[arm] = np.nanmean(((X - mu) / sd) ** 2, 1)
        words[arm] = float(np.median(n))
    dist["human"] = np.nanmean(((Xh - mu) / sd) ** 2, 1)
    g = s.prompt.values
    out = dict(n=int(len(s)), words=words, distance={k: float(np.mean(v)) for k, v in dist.items()})
    for name, a, b in (("report_minus_control", "report", "control"), ("control_minus_draft", "control", "draft"), ("report_minus_draft", "report", "draft")):
        e = boot(lambda i: float(np.mean(dist[a][i] - dist[b][i])), g)
        e2 = boot(lambda i: float(np.mean(dist[a][i] - dist[b][i])), np.arange(len(s)))
        out[name] = dict(diff=float(e[0]), ci_prompts=[float(e[1]), float(e[2])], ci_essays=[float(e2[1]), float(e2[2])])
    out["HT1"] = "supported" if out["report_minus_control"]["ci_prompts"][1] < 0 else "refuted" if out["report_minus_control"]["ci_prompts"][0] > 0 else "undecided"
    out["per_measure_z"] = {arm: {m: float(np.nanmean(((prof[arm] - mu) / sd)[:, j])) for j, m in enumerate(M)} for arm in prof}
    # secondary: a classifier trained on the other student essays and the other condition-A essays
    rest = load_essays()
    rest = rest[rest.A.notna() & ~rest.k.isin(s.k)]
    Xa, _ = tone.profiles(rest.human); Xb, _ = tone.profiles(rest.A)
    clf = classifier().fit(np.vstack([Xa, Xb]), np.r_[np.zeros(len(Xa)), np.ones(len(Xb))])
    out["machine_score"] = {arm: float(np.mean(clf.predict_proba(prof[arm])[:, 1])) for arm in prof}
    out["machine_score"]["human"] = float(np.mean(clf.predict_proba(Xh)[:, 1]))
    print(json.dumps({k: v for k, v in out.items() if k != "per_measure_z"}, indent=1))
    for m in M:
        print(f"  mean z  {m:15s} " + "  ".join(f"{arm} {out['per_measure_z'][arm][m]:+.2f}" for arm in ("draft", "control", "report")))
    return out


if __name__ == "__main__":
    path = os.path.join(RESULTS, "machine.json")
    res = json.load(open(path)) if os.path.exists(path) else {}
    for name in sys.argv[1:] or ("hc3", "essays", "reference"):
        res[name] = dict(hc3=hc3, essays=essays, reference=reference, jobs=jobs, revise=revise)[name]()
        save("machine", res)
