"""Side-by-side page of the dial experiment: student essay, AI first draft, plain revision, dial revision.

Writes machine_text/side_by_side.html (a page fragment). Essays are picked by rule, not by eye:
the median, the largest and the smallest improvement in gap to the target, and one pair the
model judge scored for the dial.
"""
import html, json, os, re, sys

import numpy as np
import pandas as pd

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE); sys.path.insert(0, os.path.join(HERE, ".."))
import sgw
from exp_machine import DATA, M, MT, load_essays, stats
from sgw import tone

WORD = re.compile(r"[A-Za-z]+(?:'[A-Za-z]+)*")
OPS = {**{w: "neg" for w in sgw.NEGATORS}, **{w: "boost" for w in sgw.BOOSTERS}}


def mark(text):
    """Wrap words that carry charge, and negators and intensifiers, so the reader sees what the scorer sees."""
    out, last = [], 0
    for m in WORD.finditer(text):
        out.append(html.escape(text[last:m.start()]))
        w = m.group().lower()
        c = int(sgw.CHARGE[sgw.VOCAB.get(w, 0)])
        cls = "hd" if c > 0 else "dl" if c < 0 else OPS.get(w)
        out.append(f'<span class="{cls}">{html.escape(m.group())}</span>' if cls else html.escape(m.group()))
        last = m.end()
    out.append(html.escape(text[last:]))
    return "".join(out).replace("\n\n", "</p><p>").replace("\n", " ")


def main():
    pick, T = pd.read_csv(f"{MT}/dial_sample.csv"), np.load(f"{MT}/dial_targets.npy")
    s = load_essays().set_index("k")
    corpus = pd.read_parquet(f"{DATA}/persuade/essays.parquet")
    assign = corpus.drop_duplicates("prompt_name").set_index("prompt_name").assignment
    sd = np.array([stats(tone.profiles(corpus.full_text)[0])[m]["sd"] for m in M])
    key, res = pd.read_csv(f"{MT}/judge_key.csv"), json.load(open(f"{MT}/judge_results.json"))
    verdict = {int(k.k): ("dial" if (k.first == "dial") == (r["better_written"] == 1) else "plain",
                          "dial" if (k.first == "dial") == (r["more_like_a_student"] == 1) else "plain") for k, r in zip(key.itertuples(), res)}
    rows = []
    for i, r in enumerate(pick.itertuples()):
        p = {a: f"{MT}/dial/r2/{a}/{r.k:03d}_{r.tier}.txt" for a in ("dial", "control")}
        if all(os.path.exists(x) for x in p.values()):
            txt = {"student": s.loc[r.k].human, "draft": s.loc[r.k].A, "plain": open(p["control"]).read(), "dial": open(p["dial"]).read()}
            P = {a: tone.profiles([t])[0][0] for a, t in txt.items()}
            gap = {a: float(np.nanmean(((P[a] - T[i]) / sd) ** 2)) for a in ("draft", "plain", "dial")}
            rows.append(dict(k=int(r.k), essay_id=r.essay_id, tier=r.tier, prompt=r.prompt, grade=r.grade, txt=txt, P=P, T=T[i], gap=gap, imp=gap["draft"] - gap["dial"]))
    rows.sort(key=lambda x: x["imp"])
    chosen = [("Middle case", "the essay at the median improvement; the paper's table uses this one", rows[len(rows) // 2]),
              ("Best case for the dial", "the largest move toward the target", rows[-1]),
              ("Worst case for the dial", "the smallest move toward the target", rows[0])]
    liked = [x for x in rows if verdict[x["k"]][1] == "dial"]
    if liked:
        chosen.append(("A case the judge scored for the dial", f"of the {len(liked)} pairs where the model judge called the dial revision the more student-like, the middle one by improvement", liked[len(liked) // 2]))
    cols = [("student", "Human", "A real student wrote this", "student"), ("draft", "AI first draft", "The model's first essay for the same topic and length", "ai"),
            ("plain", "AI, plain revision", "Told twice: make it read as a student wrote it", "ai"), ("dial", "AI, dial revision", "The same, plus target numbers from the scorer", "ai")]
    show = [("hedons", "Positive words", "per 1,000 words"), ("dolors", "Negative words", "per 1,000 words"), ("negated", "Feeling under not / never", "% of feeling"),
            ("intensified", "Feeling after very / really", "% of feeling"), ("ending_lift", "Upbeat ending", "last fifth minus whole")]
    sets = []
    for n, (title, why, x) in enumerate(chosen):
        v = verdict[x["k"]]
        grade = f"grade {int(x['grade'])}" if not np.isnan(x["grade"]) else "high school"
        trs = "".join(f"<tr><th scope='row'>{lab}<small>{unit}</small></th>" + "".join(f"<td>{x['P'][a][M.index(m)]:.1f}</td>" for a in ("student", "draft"))
                      + f"<td class='tgt'>{x['T'][M.index(m)]:.1f}</td>" + "".join(f"<td>{x['P'][a][M.index(m)]:.1f}</td>" for a in ("plain", "dial")) + "</tr>" for m, lab, unit in show)
        essays = "".join(f"<article class='col {kind}'><header><h3>{lab}</h3><p>{desc}</p><span class='wc'>{len(x['txt'][a].split())} words</span></header><div class='text'><p>{mark(x['txt'][a].strip())}</p></div></article>"
                         for a, lab, desc, kind in cols)
        name = lambda a: "the dial revision" if a == "dial" else "the plain revision"
        sets.append(f"""<section class="set" id="essay-{n + 1}">
<p class="eyebrow">Essay {n + 1} of {len(chosen)} · {title}</p>
<h2>{html.escape(x['prompt'])}</h2>
<p class="why">Picked by rule: {why}. Written by Claude {x['tier'].title()}, matched to a {grade} student's essay (PERSUADE 2.0 essay {x['essay_id']}).</p>
<div class="topic"><span>The assignment both writers got</span><p>{html.escape(assign[x['prompt']].strip())}</p></div>
<div class="facts">
<div><span>Model judge: better written</span><b>{name(v[0])}</b></div>
<div><span>Model judge: more like a student</span><b>{name(v[1])}</b></div>
<div><span>Distance from the dial's target</span><b>{x['gap']['draft']:.2f} → {x['gap']['dial']:.2f}</b><small>plain revision: {x['gap']['plain']:.2f}</small></div>
</div>
<div class="scroll"><table><thead><tr><th scope="col">What the scorer measured</th><th scope="col">Human</th><th scope="col">AI first draft</th><th scope="col" class="tgt">Dial's target</th><th scope="col">AI, plain</th><th scope="col">AI, dial</th></tr></thead><tbody>{trs}</tbody></table></div>
<div class="grid">{essays}</div>
</section>""")
    page = TEMPLATE.replace("/*SETS*/", "\n".join(sets)).replace("/*N*/", str(len(rows)))
    open(f"{MT}/side_by_side.html", "w", encoding="utf-8").write(page)
    print("wrote side_by_side.html:", len(page) // 1024, "KB,", len(chosen), "essays")


TEMPLATE = open(os.path.join(HERE, "side_by_side_template.html"), encoding="utf-8").read()

if __name__ == "__main__":
    main()
