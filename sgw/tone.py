"""Tone profile: measures of a text that do not depend on its length, read from signatures,
and a comparison of a draft with the range found in human writing of the same kind.

    from sgw import tone
    tone.profile("some text")                # the ten measures
    print(tone.report(draft, "essay"))       # where the draft sits against student essays

The reference ranges in reference.json were computed from human-written corpora
(scripts/exp_machine.py). They describe those corpora, not good writing in general.
"""
import json, os, re

import numpy as np

from . import core

SENT = re.compile(r"(?<=[.!?])\s+")
MEASURES = ["hedons", "dolors", "net", "gross", "charged", "negated", "intensified", "after_contrast", "ending_lift", "clustering"]
LABEL = {
    "hedons": ("positive charge", "hedons per 1,000 words"),
    "dolors": ("negative charge", "dolors per 1,000 words"),
    "net": ("net charge", "per 1,000 words"),
    "gross": ("total charge, positive plus negative", "per 1,000 words"),
    "charged": ("words that carry charge", "% of words"),
    "negated": ("charge under a negation", "% of charge"),
    "intensified": ("charge after an intensifier such as 'very'", "% of charge"),
    "after_contrast": ("charge after a contrast word such as 'but'", "% of charge"),
    "ending_lift": ("fitted arc of net charge over the last fifth minus the text's mean", "per 1,000 words"),
    "clustering": ("how unevenly charge is spread over sentences", "1 = as if words were dealt at random"),
}
ADVICE = {
    ("hedons", 1): "The draft uses more positive words than people do here. Cut praise words that add no information.",
    ("hedons", -1): "The draft uses fewer positive words than people do here.",
    ("dolors", 1): "The draft uses more negative words than people do here.",
    ("dolors", -1): "The draft avoids negative words. People writing on this kind of prompt name problems, costs and dislikes plainly.",
    ("net", 1): "The overall tone is more positive than human writing of this kind.",
    ("net", -1): "The overall tone is more negative than human writing of this kind.",
    ("gross", 1): "The draft is more emotionally loaded than human writing of this kind.",
    ("gross", -1): "The draft is flatter than human writing of this kind: fewer words carry any feeling.",
    ("charged", 1): "A larger share of the words carry feeling than in human writing of this kind.",
    ("charged", -1): "A smaller share of the words carry feeling than in human writing of this kind.",
    ("negated", -1): "People negate more ('not good', 'isn't fair'). The draft states almost everything in the affirmative.",
    ("negated", 1): "The draft negates more than people do here.",
    ("intensified", -1): "People use more plain intensifiers ('very', 'really', 'so').",
    ("intensified", 1): "The draft leans on intensifiers more than people do here.",
    ("after_contrast", 1): "More of the feeling sits after 'but' or 'however' than in human writing: the draft keeps balancing every point.",
    ("after_contrast", -1): "Less of the feeling sits after a contrast word than in human writing.",
    ("ending_lift", 1): "The ending is more upbeat than the rest of the draft by a wider margin than in human writing. Do not close on a lift the body has not earned.",
    ("ending_lift", -1): "The ending is more downbeat, relative to the body, than in human writing of this kind.",
    ("clustering", -1): "Feeling is spread evenly over the sentences. People bunch it: some sentences are flat, some carry a lot.",
    ("clustering", 1): "Feeling is bunched into fewer sentences than in human writing of this kind.",
}
_HERE = os.path.dirname(os.path.abspath(__file__))
_REF = None


def profiles(texts):
    """Matrix with one row per text and one column per entry of MEASURES; second value is the word counts."""
    texts = list(texts)
    n = len(texts)
    S = np.atleast_2d(core.sketch(texts))
    L = np.maximum(S[:, core.L], 1)
    H, D = S[:, core.P0], S[:, core.N0]
    G = np.maximum(H + D, 1e-9)
    lift = 1000 * (core.arc(S, np.linspace(0.8, 1.0, 21)).mean(-1) - (H - D) / L)
    sents, owner = [], []
    for i, t in enumerate(texts):
        ss = [s for s in SENT.split(t.replace("\n", " ")) if s.strip()]
        sents += ss
        owner += [i] * len(ss)
    clustering = np.full(n, np.nan)
    if sents:
        owner = np.array(owner)
        T = np.atleast_2d(core.sketch(sents))
        ls, qs = T[:, core.L], T[:, core.P0] - T[:, core.N0]
        Lt = np.maximum(np.bincount(owner, weights=ls, minlength=n), 1)
        rho = np.bincount(owner, weights=qs, minlength=n) / Lt
        m = np.bincount(owner, weights=ls > 0, minlength=n)
        between = np.bincount(owner, weights=ls * (qs / np.maximum(ls, 1) - rho[owner]) ** 2, minlength=n) / Lt
        expected = (np.bincount(owner, weights=T[:, core.SS], minlength=n) / Lt - rho ** 2) * (m - 1) / Lt
        ok = (m >= 3) & (expected > 0)
        clustering[ok] = between[ok] / expected[ok]
    X = np.column_stack([1000 * H / L, 1000 * D / L, 1000 * (H - D) / L, 1000 * (H + D) / L, 100 * S[:, core.E] / L,
                         100 * (S[:, core.NEGP] + S[:, core.NEGN]) / G, 100 * (S[:, core.BOOP] + S[:, core.BOON]) / G,
                         100 * (S[:, core.POSTP] + S[:, core.POSTN]) / G, lift, clustering])
    return X, S[:, core.L]


def profile(text):
    X, n = profiles([text])
    return {"words": int(n[0]), **{k: (None if np.isnan(v) else round(float(v), 2)) for k, v in zip(MEASURES, X[0])}}


def reference(genre=None):
    """Human reference ranges: {genre: {measure: {mean, sd, p5, p25, p50, p75, p95}, "n": texts}}."""
    global _REF
    if _REF is None:
        _REF = json.load(open(os.path.join(_HERE, "reference.json")))
    if genre is not None and genre not in _REF:
        raise ValueError(f"Unknown genre {genre!r}. Choose one of: {', '.join(sorted(_REF))}.")
    return _REF if genre is None else _REF[genre]


def compare(text, genre="essay", ref=None):
    """Each measure of the draft with its z-score against the human reference and a flag when outside the middle 80%."""
    ref = ref or reference(genre)
    p = profile(text)
    out = []
    for k in MEASURES:
        r, v = ref[k], p[k]
        if v is None:
            continue
        z = (v - r["mean"]) / r["sd"] if r["sd"] > 0 else 0.0
        side = 1 if v > r["p90"] else -1 if v < r["p10"] else 0
        out.append({"measure": k, "value": v, "human_median": r["p50"], "human_low": r["p10"], "human_high": r["p90"], "z": round(z, 2), "outside": side})
    return {"words": p["words"], "genre": genre, "distance": round(float(np.mean([c["z"] ** 2 for c in out])), 2), "measures": out}


def report(text, genre="essay", ref=None):
    """Plain-language report for a writer or a language model revising a draft."""
    c = compare(text, genre, ref)
    lines = [f"Tone check against human {genre} writing ({c['words']} words). Distance from the human centre: {c['distance']} (1.0 is typical for a human text)."]
    if c["words"] < 100:
        lines.append("The draft is under 100 words, so every measure below is noisy. Read the flags as hints.")
    flagged = [m for m in c["measures"] if m["outside"]]
    if not flagged:
        lines.append("Every measure is inside the range that covers the middle 80% of human texts. Nothing to change on tone.")
    for m in sorted(flagged, key=lambda m: -abs(m["z"])):
        name, unit = LABEL[m["measure"]]
        lines.append(f"- {name}: {m['value']} ({unit}); most human texts are between {m['human_low']} and {m['human_high']}. "
                     + ADVICE.get((m["measure"], m["outside"]), ""))
    inside = [LABEL[m["measure"]][0] for m in c["measures"] if not m["outside"]]
    if flagged and inside:
        lines.append("Inside the human range: " + "; ".join(inside) + ".")
    lines.append("These are ranges, not targets. Change wording only where the change also makes the text more specific or more honest.")
    return "\n".join(lines)
