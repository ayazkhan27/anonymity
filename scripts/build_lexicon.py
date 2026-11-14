"""Build the word-charge lexicons.

Charge is a human word rating mapped to [-1, 1] with 0 at the scale's neutral point,
on the labMT scale: charge = (happiness - 5) / 4. Other lexicons are put on that scale
by a slope through the origin fitted on shared words (ratio of standard deviations,
the symmetric fit), then averaged word by word.

    open      labMT 1.0 (CC BY) + VADER word list (MIT)   -> sgw/lexicon.tsv, shipped
    extended  open + NRC-VAD + Warriner norms              -> data/lex_restricted/, not shipped
"""
import csv, json, os
import numpy as np

HERE = os.path.dirname(os.path.abspath(__file__))
DATA = os.path.join(HERE, "..", "data")


def labmt():
    return {w: (float(h) - 5) / 4 for w, h in (l.rstrip("\n").split("\t")[:2] for l in open(f"{DATA}/lex/labMT_English.tsv", encoding="utf-8"))}


def vader():
    out = {}
    for l in open(f"{DATA}/lex/vader_lexicon.txt", encoding="utf-8"):
        w, v = l.split("\t")[:2]
        out[w.lower()] = float(v) / 4
    return out


def nrcvad():
    out = {}
    for l in open(f"{DATA}/lex_restricted/nrcvad/NRC-VAD-Lexicon/NRC-VAD-Lexicon.txt", encoding="utf-8"):
        w, v = l.split("\t")[:2]
        if " " not in w:
            try:
                out[w.lower()] = 2 * float(v) - 1
            except ValueError:
                pass  # header line, if any
    return out


def warriner():
    return {r["Word"].lower(): (float(r["V.Mean.Sum"]) - 5) / 4
            for r in csv.DictReader(open(f"{DATA}/lex_restricted/warriner/BRM-emot-submit.csv", encoding="utf-8"))}


def equate(ref, other):
    """Scale `other` onto `ref`. Returns (scaled dict, slope, Pearson r, n shared)."""
    shared = sorted(set(ref) & set(other))
    a, b = np.array([ref[w] for w in shared]), np.array([other[w] for w in shared])
    slope = float(np.sqrt((a ** 2).mean() / (b ** 2).mean()))
    return {w: v * slope for w, v in other.items()}, slope, float(np.corrcoef(a, b)[0, 1]), len(shared)


def consensus(lexicons):
    words = set().union(*lexicons)
    return {w: float(np.mean([l[w] for l in lexicons if w in l])) for w in words}


def write(path, lex):
    with open(path, "w", encoding="utf-8") as f:
        for w in sorted(lex):
            if "\t" not in w and "\n" not in w and w.strip():
                f.write(f"{w}\t{round(1000 * max(-1, min(1, lex[w])))}\n")  # milli-units, integers


if __name__ == "__main__":
    ref, report = labmt(), {"labMT": {"n": 0}}
    report["labMT"]["n"] = len(ref)
    parts = [ref]
    for name, load in (("VADER", vader),):
        scaled, slope, r, n = equate(ref, load())
        parts.append(scaled)
        report[name] = {"n": len(scaled), "slope": slope, "r_with_labMT": r, "shared": n}
    write(os.path.join(HERE, "..", "sgw", "lexicon.tsv"), consensus(parts))
    report["open"] = {"n": len(consensus(parts))}
    try:
        for name, load in (("NRC-VAD", nrcvad), ("Warriner", warriner)):
            scaled, slope, r, n = equate(ref, load())
            parts.append(scaled)
            report[name] = {"n": len(scaled), "slope": slope, "r_with_labMT": r, "shared": n}
        write(f"{DATA}/lex_restricted/extended.tsv", consensus(parts))
        report["extended"] = {"n": len(consensus(parts))}
    except FileNotFoundError:
        print("restricted lexicons not downloaded; skipped the extended lexicon")
    os.makedirs(os.path.join(HERE, "..", "results"), exist_ok=True)
    json.dump(report, open(os.path.join(HERE, "..", "results", "lexicon.json"), "w"), indent=1)
    print(json.dumps(report, indent=1))
