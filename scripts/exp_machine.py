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


def prepare():
    """One row per essay from the PERSUADE 2.0 training file (which has one row per discourse element)."""
    cols = ["essay_id_comp", "full_text", "task", "source_text", "prompt_name", "assignment", "grade_level", "essay_word_count", "holistic_essay_score"]
    parts = [c.drop_duplicates("essay_id_comp") for c in pd.read_csv(f"{DATA}/persuade/persuade_train.csv", usecols=cols, chunksize=40000, low_memory=False)]
    df = pd.concat(parts).drop_duplicates("essay_id_comp").reset_index(drop=True)
    df.to_parquet(f"{DATA}/persuade/essays.parquet")
    print(len(df), "essays;", df.prompt_name.nunique(), "assignments")
    return {"essays": int(len(df))}


TIERS = ["fable", "sonnet", "haiku", "opus"]


def samplejobs():
    """The sample of 160 student essays (20 per independent assignment, seed 0) and the two essay jobs for each."""
    df = pd.read_parquet(f"{DATA}/persuade/essays.parquet")
    df = df[(df.task == "Independent") & df.essay_word_count.between(150, 800)]
    s = pd.concat([g.sample(20, random_state=0) for _, g in sorted(df.groupby("prompt_name"), key=lambda kv: kv[0])]).reset_index(drop=True)
    os.makedirs(f"{MT}/jobs", exist_ok=True)
    meta = []
    for k, r in s.iterrows():
        n = int(round(r.essay_word_count, -1))
        grade = None if pd.isna(r.grade_level) else int(r.grade_level)
        tier = TIERS[k % 4]
        for cond in "AB":
            as_student = ("" if cond == "A" else f", the way a US grade {grade} student would write it in class for a timed writing test" if grade
                          else ", the way a US high school student would write it in class for a timed writing test")
            text = (f"Write an essay in response to the assignment below{as_student}. Aim for about {n} words. "
                    f"The file must contain only the essay itself: no notes, no commentary, no word count.\n\nAssignment: {r.assignment.strip()}")
            json.dump({"instructions": text, "output_path": f"machine_text/essays/{cond}/{k:03d}_{tier}.txt"}, open(f"{MT}/jobs/{2 * k + (cond == 'B'):03d}.json", "w"), indent=1)
        meta.append(dict(k=k, essay_id=r.essay_id_comp, prompt=r.prompt_name, grade=grade, words=int(r.essay_word_count), tier=tier))
    pd.DataFrame(meta).to_csv(f"{MT}/sample.csv", index=False)
    return {"essays": int(len(s))}


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
        dz = (Xm - Xh) / np.nanstd(Xh, 0)
        jk = {m: jack(lambda i, j=j: float(np.nanmean(dz[i, j])), g) for j, m in enumerate(M)}
        verdict_jk = {h: ("supported" if sgn * jk[m][1] > 0 and sgn * jk[m][2] > 0 else "refuted" if sgn * jk[m][1] < 0 and sgn * jk[m][2] < 0 else "undecided") for h, (m, sgn) in HYP.items()}
        out[c] = dict(n=int(len(d)), words_human=float(np.median(nh)), words_machine=float(np.median(nm)), measures=res, decisions=decisions(res),
                      decisions_resampling_essays=decisions(res_item), d_ci_resampling_essays={m: [res_item[m]["d_lo"], res_item[m]["d_hi"]] for m in M},
                      d_jackknife=jk, decisions_jackknife=verdict_jk,
                      auc=auc(Xh, Xm, g, 8), auc_words=auc(np.log(nh)[:, None], np.log(nm)[:, None], g, 8),
                      by_tier={t: {m: float(np.nanmean((Xm - Xh)[idx, j]) / np.nanstd(Xh[:, j])) for j, m in enumerate(M)} for t, idx in d.groupby("tier").indices.items()})
        print(f"\nEssays, condition {c}: {len(d)} pairs; AUC {out[c]['auc'][0]:.3f} [{out[c]['auc'][1]:.3f}, {out[c]['auc'][2]:.3f}]; word count alone {out[c]['auc_words'][0]:.3f}")
        for m in M:
            print(f"  {m:15s} human {res[m]['human']:7.2f}  machine {res[m]['machine']:7.2f}  d = {res[m]['d']:+.2f} [{res[m]['d_lo']:+.2f}, {res[m]['d_hi']:+.2f}]")
        print(" ", out[c]["decisions"], "| resampling essays:", out[c]["decisions_resampling_essays"], "| jackknife:", verdict_jk)
        print("  jackknife d:", {m: [round(v, 2) for v in jk[m]] for m in ("net", "dolors", "clustering", "ending_lift", "negated", "intensified")})
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
    root = "machine_text"                                       # job files carry repository-relative paths
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


SKILLS = {"humanizer": "humanizer:humanizer", "sepia": "sepia:sepia-recreate"}


def skilljobs():
    """Addendum 3: the same drafts rewritten with an anti-slop skill (control instruction plus the skill)."""
    pick = pd.read_csv(f"{MT}/revision_sample.csv")
    s = load_essays().set_index("k").loc[pick.k].reset_index()
    os.makedirs(f"{MT}/skill_jobs", exist_ok=True)
    root, tiers = "machine_text", []
    for i, r in enumerate(s.itertuples()):
        who = f"a US grade {int(r.grade)} student" if not np.isnan(r.grade) else "a US high school student"
        base = (f"Below is a draft essay. Revise it so that it reads as if {who} wrote it in class for a timed writing test. "
                f"Keep the position it argues and keep it at about {len(r.A.split())} words. The file must contain only the revised essay.")
        for j, (arm, skill) in enumerate(SKILLS.items()):
            os.makedirs(f"{MT}/revisions/{arm}", exist_ok=True)
            extra = f"\n\nDo the rewrite with the skill named `{skill}`: load it with the Skill tool and follow its instructions for rewriting text, then save only the final text."
            json.dump({"instructions": base + extra + "\n\nDraft:\n\n" + r.A, "output_path": f"{root}/revisions/{arm}/{r.k:03d}_{r.tier}.txt"},
                      open(f"{MT}/skill_jobs/{len(SKILLS) * i + j:03d}.json", "w"), indent=1)
            tiers.append(r.tier)
    print(json.dumps(tiers))
    return {"n": int(len(s)), "skills": SKILLS}


def revise():
    """HT1: is the draft closer to the human range after a revision that saw the report than after one that did not?"""
    pick = pd.read_csv(f"{MT}/revision_sample.csv")
    s = load_essays().set_index("k").loc[pick.k].reset_index()
    arms = [a for a in ("control", "report", "humanizer", "sepia") if os.path.isdir(f"{MT}/revisions/{a}")]
    texts = {"draft": list(s.A)}
    for arm in arms:
        texts[arm] = [open(f"{MT}/revisions/{arm}/{k:03d}_{t}.txt", encoding="utf-8").read() if os.path.exists(f"{MT}/revisions/{arm}/{k:03d}_{t}.txt") else None for k, t in zip(s.k, s.tier)]
    ok = np.array([all(texts[a][i] is not None for a in arms) for i in range(len(s))])
    declined = {a: int(sum(t is None for t in texts[a])) for a in arms}
    s = s[ok].reset_index(drop=True)
    refs = {p: essay_reference(p) for p in s.prompt.unique()}
    mu = np.array([[refs[p][m]["mean"] for m in M] for p in s.prompt]); sd = np.array([[refs[p][m]["sd"] for m in M] for p in s.prompt])
    Xh, _ = tone.profiles(s.human)
    z = np.load(os.path.join(CACHE, "hc3_profiles.npz"))
    corpus = pd.read_parquet(f"{DATA}/persuade/essays.parquet")
    dist, words, prof = {}, {}, {}
    for arm in ["draft"] + arms:
        X, n = tone.profiles([t for t, keep in zip(texts[arm], ok) if keep])
        prof[arm] = X
        dist[arm] = np.nanmean(((X - mu) / sd) ** 2, 1)
        words[arm] = float(np.median(n))
    dist["human"] = np.nanmean(((Xh - mu) / sd) ** 2, 1)
    g = s.prompt.values
    out = dict(n=int(len(s)), n_assignments=int(len(set(g))), arms=arms, declined=declined, words=words, distance={k: float(np.mean(v)) for k, v in dist.items()})
    pairs = [("report_minus_control", "report", "control"), ("control_minus_draft", "control", "draft"), ("report_minus_draft", "report", "draft")]
    pairs += [(f"{a}_minus_control", a, "control") for a in arms if a in SKILLS] + [(f"{a}_minus_draft", a, "draft") for a in arms if a in SKILLS]
    pairs += [(f"report_minus_{a}", "report", a) for a in arms if a in SKILLS]
    for name, a, b in pairs:
        e = boot(lambda i: float(np.mean(dist[a][i] - dist[b][i])), g)
        e2 = boot(lambda i: float(np.mean(dist[a][i] - dist[b][i])), np.arange(len(s)))
        out[name] = dict(diff=float(e[0]), ci_prompts=[float(e[1]), float(e[2])], ci_essays=[float(e2[1]), float(e2[2])],
                         ci_jackknife=jack(lambda i: float(np.mean(dist[a][i] - dist[b][i])), g)[1:])
    verdict = lambda k: "supported" if out[k]["ci_prompts"][1] < 0 else "refuted" if out[k]["ci_prompts"][0] > 0 else "undecided"
    out["HT1"] = verdict("report_minus_control")
    out["HS1"] = {a: ("closer" if out[f"{a}_minus_control"]["ci_prompts"][1] < 0 else "farther" if out[f"{a}_minus_control"]["ci_prompts"][0] > 0 else "undecided") for a in arms if a in SKILLS}
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
        print(f"  mean z  {m:15s} " + "  ".join(f"{arm} {out['per_measure_z'][arm][m]:+.2f}" for arm in ["draft"] + arms))
    return out


# ---------------------------------------------------------------- addendum 4: the dial

SHEET = [("hedons", "positive words such as good, love, fun"), ("dolors", "negative words such as bad, hate, problem"),
         ("charged", "share of all words that carry any feeling"), ("negated", "feeling words placed under not, never or n't"),
         ("intensified", "feeling words placed right after very, really or so"), ("after_contrast", "feeling words placed after but or however in the same sentence"),
         ("ending_lift", "how much more positive the last fifth is than the essay as a whole; negative means a more downbeat ending"),
         ("clustering", "above 1 means the feeling is bunched into some sentences while others are plain; below 1 means it is spread evenly")]
CONTROL = ("Below is a draft essay. Revise it so that it reads as if {who} wrote it in class for a timed writing test. "
           "Keep the position it argues and keep it at about {n} words. The file must contain only the revised essay.")


def sheet(current, target, again=False):
    head = ("A measurement tool scored your last revision. " if again else "A measurement tool scored the draft on how it carries feeling. ") + \
           "For each measure below it gives the current value and a target. Revise the essay so that its measures move to the targets; the tool will score the result again.\n"
    rows = []
    for m, hint in SHEET:
        j = M.index(m)
        name, unit = tone.LABEL[m]
        rows.append(f"- {name} ({unit}): now {current[j]:.1f}, target {target[j]:.1f}. Counts {hint}.")
    return head + "\n".join(rows)


def who(grade):
    return f"a US grade {int(grade)} student" if not np.isnan(grade) else "a US high school student"


def dial_sample():
    s = load_essays()
    used = pd.read_csv(f"{MT}/revision_sample.csv").k
    rest = s[s.A.notna() & ~s.k.isin(used)]
    pick = pd.concat([g.sample(min(15, len(g)), random_state=1) for _, g in rest.groupby("tier")]).sort_values("k").reset_index(drop=True)
    corpus = pd.read_parquet(f"{DATA}/persuade/essays.parquet")
    X, n = tone.profiles(corpus.full_text)
    ok = (n >= 100) & ~np.isnan(X).any(1)
    rng = np.random.default_rng(0)
    targets = np.array([X[rng.choice(np.flatnonzero(ok & (corpus.prompt_name != p).values))] for p in pick.prompt])
    return pick, targets


def dialjobs():
    """Round 1: dial arm (control instruction plus the sheet) and control arm, on fresh condition-A drafts."""
    pick, targets = dial_sample()
    root = "machine_text"                                       # job files carry repository-relative paths
    for d in ("dial_jobs/r1", "dial_jobs/r2", "dial/r1/dial", "dial/r1/control", "dial/r2/dial", "dial/r2/control"):
        os.makedirs(f"{MT}/{d}", exist_ok=True)
    cur, _ = tone.profiles(pick.A)
    tiers = []
    for i, r in enumerate(pick.itertuples()):
        base = CONTROL.format(who=who(r.grade), n=len(r.A.split()))
        for j, arm in enumerate(("dial", "control")):
            extra = "\n\n" + sheet(cur[i], targets[i]) if arm == "dial" else ""
            json.dump({"instructions": base + extra + "\n\nDraft:\n\n" + r.A, "output_path": f"{root}/dial/r1/{arm}/{r.k:03d}_{r.tier}.txt"},
                      open(f"{MT}/dial_jobs/r1/{2 * i + j:03d}.json", "w"), indent=1)
            tiers.append(r.tier)
    pick[["k", "essay_id", "prompt", "grade", "words", "tier"]].to_csv(f"{MT}/dial_sample.csv", index=False)
    np.save(f"{MT}/dial_targets.npy", targets)
    print(json.dumps(tiers))
    return {"n": int(len(pick))}


def dialjobs2():
    """Round 2: each arm revises its own round-1 text; the dial arm gets a fresh sheet."""
    pick, targets = pd.read_csv(f"{MT}/dial_sample.csv"), np.load(f"{MT}/dial_targets.npy")
    root, tiers, n = "machine_text", [], 0
    for i, r in enumerate(pick.itertuples()):
        for j, arm in enumerate(("dial", "control")):
            p1 = f"{MT}/dial/r1/{arm}/{r.k:03d}_{r.tier}.txt"
            path = f"{MT}/dial_jobs/r2/{2 * i + j:03d}.json"
            tiers.append(r.tier)
            if not os.path.exists(p1):
                json.dump({"instructions": "Reply with the single word: skipped. Write no file.", "output_path": ""}, open(path, "w"))
                continue
            text = open(p1, encoding="utf-8").read()
            base = CONTROL.format(who=who(r.grade), n=len(text.split())).replace("Below is a draft essay.", "Below is an essay you revised once.")
            extra = "\n\n" + sheet(tone.profiles([text])[0][0], targets[i], again=True) if arm == "dial" else ""
            json.dump({"instructions": base + extra + "\n\nEssay:\n\n" + text, "output_path": f"{root}/dial/r2/{arm}/{r.k:03d}_{r.tier}.txt"}, open(path, "w"), indent=1)
            n += 1
    print(json.dumps(tiers))
    return {"jobs": n}


def judgejobs():
    """HD3: blind pairs. The judge sees the two round-2 revisions of one draft in a seeded random order."""
    pick = pd.read_csv(f"{MT}/dial_sample.csv")
    corpus = pd.read_parquet(f"{DATA}/persuade/essays.parquet").drop_duplicates("prompt_name").set_index("prompt_name")
    os.makedirs(f"{MT}/judge_jobs", exist_ok=True)
    rng = np.random.default_rng(0)
    key, judges = [], []
    for i, r in enumerate(pick.itertuples()):
        paths = {arm: f"{MT}/dial/r2/{arm}/{r.k:03d}_{r.tier}.txt" for arm in ("dial", "control")}
        if not all(os.path.exists(p) for p in paths.values()):
            continue
        order = ["dial", "control"] if rng.random() < 0.5 else ["control", "dial"]
        texts = [open(paths[a], encoding="utf-8").read() for a in order]
        json.dump({"assignment": corpus.loc[r.prompt].assignment, "writer": who(r.grade), "essay_1": texts[0], "essay_2": texts[1]},
                  open(f"{MT}/judge_jobs/{len(key):03d}.json", "w"), indent=1)
        key.append(dict(job=len(key), k=int(r.k), first=order[0]))
        judges.append("opus" if r.tier != "opus" else "sonnet")
    pd.DataFrame(key).to_csv(f"{MT}/judge_key.csv", index=False)
    print(json.dumps(judges))
    return {"pairs": len(key)}


def dial():
    """Score the round-2 texts against the assigned targets (HD1), the classifier (HD2) and the judges (HD3)."""
    pick, targets = pd.read_csv(f"{MT}/dial_sample.csv"), np.load(f"{MT}/dial_targets.npy")
    s = load_essays().set_index("k")
    texts = {arm: [open(p, encoding="utf-8").read() if os.path.exists(p) else None for p in (f"{MT}/dial/r2/{arm}/{k:03d}_{t}.txt" for k, t in zip(pick.k, pick.tier))] for arm in ("dial", "control")}
    r1 = {arm: [open(p, encoding="utf-8").read() if os.path.exists(p) else None for p in (f"{MT}/dial/r1/{arm}/{k:03d}_{t}.txt" for k, t in zip(pick.k, pick.tier))] for arm in ("dial", "control")}
    ok = np.array([a is not None and b is not None for a, b in zip(texts["dial"], texts["control"])])
    pick, T = pick[ok].reset_index(drop=True), targets[ok]
    g = pick.prompt.values
    ref = stats(tone.profiles(pd.read_parquet(f"{DATA}/persuade/essays.parquet").full_text)[0])
    sd = np.array([ref[m]["sd"] for m in M]); mu = np.array([ref[m]["mean"] for m in M])
    P = {"draft": tone.profiles(s.loc[pick.k].A)[0], "human": tone.profiles(s.loc[pick.k].human)[0]}
    for arm in ("dial", "control"):
        P[arm] = tone.profiles([t for t, keep in zip(texts[arm], ok) if keep])[0]
        P[arm + "_r1"] = tone.profiles([t if t is not None else "" for t, keep in zip(r1[arm], ok) if keep])[0]
    corr = lambda X, i: np.array([pearson(T[i, j], np.nan_to_num(X[i, j], nan=np.nanmean(X[:, j]))) for j in range(len(M))])
    gap = lambda X: np.nanmean(((X - T) / sd) ** 2, 1)
    out = dict(n=int(len(pick)), n_assignments=int(len(set(g))), declined=int((~ok).sum()), corr={a: {m: float(c) for m, c in zip(M, corr(P[a], np.arange(len(pick))))} for a in ("dial", "control", "draft")},
               gap={a: float(gap(P[a]).mean()) for a in ("draft", "control_r1", "dial_r1", "control", "dial")},
               distance_to_centre={a: float(np.nanmean(((P[a] - mu) / sd) ** 2)) for a in ("draft", "control", "dial", "human")})
    for name, stat in (("corr_dial_minus_control", lambda i: float(np.mean(corr(P["dial"], i) - corr(P["control"], i)))),
                       ("gap_dial_minus_control", lambda i: float(np.mean(gap(P["dial"])[i] - gap(P["control"])[i])))):
        e, e2 = boot(stat, g), boot(stat, np.arange(len(pick)))
        out[name] = dict(diff=float(e[0]), ci_prompts=[float(e[1]), float(e[2])], ci_essays=[float(e2[1]), float(e2[2])], ci_jackknife=jack(stat, g)[1:])
    out["mean_corr"] = {a: float(np.mean(list(out["corr"][a].values()))) for a in out["corr"]}
    full = pd.read_csv(f"{MT}/dial_sample.csv")
    gone = [t for t, x in zip(full.tier, texts["dial"]) if x is None]
    out["missing_dial"] = dict(total=len(gone), haiku=gone.count("haiku"), requested=int(len(full)), missing_control=int(sum(x is None for x in texts["control"])))
    out["HD1"] = "supported" if out["corr_dial_minus_control"]["ci_prompts"][0] > 0 else "refuted" if out["corr_dial_minus_control"]["ci_prompts"][1] < 0 else "undecided"
    rest = load_essays(); rest = rest[rest.A.notna() & ~rest.k.isin(full.k)]            # none of the 60 sampled essays, complete or not
    clf = classifier().fit(np.vstack([tone.profiles(rest.human)[0], tone.profiles(rest.A)[0]]), np.r_[np.zeros(len(rest)), np.ones(len(rest))])
    out["classifier_training_essays"] = int(len(rest))
    out["mean_corr_by_model"] = {t: dict(n=int((pick.tier == t).sum()), dial=float(np.mean(corr(P["dial"], np.flatnonzero((pick.tier == t).values)))),
                                         control=float(np.mean(corr(P["control"], np.flatnonzero((pick.tier == t).values))))) for t in sorted(pick.tier.unique()) if (pick.tier == t).sum() >= 8}
    sc = {a: clf.predict_proba(P[a])[:, 1] for a in ("draft", "control", "dial", "human")}
    out["machine_score"] = {a: float(v.mean()) for a, v in sc.items()}
    from sklearn.metrics import roc_auc_score
    out["auc_vs_matched_students"] = {a: float(roc_auc_score(np.r_[np.zeros(len(pick)), np.ones(len(pick))], np.r_[sc["human"], sc[a]])) for a in ("draft", "control", "dial")}
    e, e2 = boot(lambda i: float(np.mean(sc["dial"][i] - sc["control"][i])), g), boot(lambda i: float(np.mean(sc["dial"][i] - sc["control"][i])), np.arange(len(pick)))
    out["score_dial_minus_control"] = dict(diff=float(e[0]), ci_prompts=[float(e[1]), float(e[2])], ci_essays=[float(e2[1]), float(e2[2])],
                                           ci_jackknife=jack(lambda i: float(np.mean(sc["dial"][i] - sc["control"][i])), g)[1:])
    out["HD2"] = "supported" if e[2] < 0 else "refuted" if e[1] > 0 else "undecided"
    jpath = f"{MT}/judge_results.json"
    if os.path.exists(jpath):
        key, res = pd.read_csv(f"{MT}/judge_key.csv"), json.load(open(jpath))
        rows = [(k.k, (k.first == "dial") == (r["better_written"] == 1), (k.first == "dial") == (r["more_like_a_student"] == 1)) for k, r in zip(key.itertuples(), res) if r]
        J = pd.DataFrame(rows, columns=["k", "better", "student"]).merge(pick[["k", "prompt", "tier"]], on="k")
        from scipy.stats import beta
        for col in ("better", "student"):
            e, e2 = boot(lambda i: float(J[col].values[i].mean()), J.prompt.values), boot(lambda i: float(J[col].values[i].mean()), np.arange(len(J)))
            out["judge_" + col] = dict(share=float(e[0]), ci_prompts=[float(e[1]), float(e[2])], ci_essays=[float(e2[1]), float(e2[2])], n=int(len(J)),
                                       ci_jackknife=jack(lambda i: float(J[col].values[i].mean()), J.prompt.values)[1:])
            w, n = int(J[col].sum()), len(J)                              # exact binomial interval: pairs treated as independent
            out["judge_" + col].update(wins=w, ci_exact=[float(beta.ppf(0.025, w, n - w + 1)) if w else 0.0, float(beta.ppf(0.975, w + 1, n - w)) if w < n else 1.0])
        out["judge_by_model"] = {t_: dict(n=int(len(x)), better=int(x.better.sum()), student=int(x.student.sum())) for t_, x in J.groupby("tier")}
        out["judge_both"] = int((J.better & J.student).sum())
        out["HD3"] = "supported" if out["judge_better"]["ci_prompts"][1] >= 0.5 else "refuted"
    # excerpt: the essay at the median improvement in gap (dial round 2 against draft)
    imp = gap(P["draft"]) - gap(P["dial"])
    m = int(np.argsort(imp)[len(imp) // 2])
    k, t = int(pick.k[m]), pick.tier[m]
    cut = lambda x: {"opening": " ".join(x.split()[:45]), "closing": " ".join(x.split()[-45:])}
    out["excerpt"] = dict(k=k, tier=t, prompt=pick.prompt[m], draft=cut(s.loc[k].A), dial=cut(open(f"{MT}/dial/r2/dial/{k:03d}_{t}.txt", encoding="utf-8").read()),
                          control=cut(open(f"{MT}/dial/r2/control/{k:03d}_{t}.txt", encoding="utf-8").read()),
                          values={a: {mm: float(P[a][m, M.index(mm)]) for mm in ("intensified", "negated", "ending_lift", "hedons", "dolors")} for a in ("draft", "dial", "control")},
                          target={mm: float(T[m, M.index(mm)]) for mm in ("intensified", "negated", "ending_lift", "hedons", "dolors")})
    print(json.dumps({k2: v for k2, v in out.items() if k2 not in ("corr", "excerpt")}, indent=1))
    print("corr by measure:", {m2: (round(out["corr"]["dial"][m2], 2), round(out["corr"]["control"][m2], 2)) for m2 in M})
    return out


if __name__ == "__main__":
    path = os.path.join(RESULTS, "machine.json")
    res = json.load(open(path)) if os.path.exists(path) else {}
    for name in sys.argv[1:] or ("hc3", "essays", "reference"):
        res[name] = dict(prepare=prepare, samplejobs=samplejobs, hc3=hc3, essays=essays, reference=reference, jobs=jobs, skilljobs=skilljobs, revise=revise, dialjobs=dialjobs, dialjobs2=dialjobs2, judgejobs=judgejobs, dial=dial)[name]()
        save("machine", res)
