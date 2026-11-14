"""Sentiment Gummy Worm: sentiment as an additive charge with positional moments.

Every word carries a charge from human word ratings: positive charge in hedons (hd),
negative charge in dolors (dl), both >= 0, at most 1 per word. A span of text is
summarised by a fixed-size signature (counts and power sums of charge over word
position). Signatures of adjacent spans merge exactly into the signature of their
concatenation, so a book is scored in one pass and any chapter, page or sentence
can be read off afterwards without rescoring.

    S = sketch(["A lovely day.", "Not good, but the pasta was wonderful."])
    totals(S)            # hedons, dolors, words
    merge(S[0], S[1])    # signature of the two sentences read as one text
"""
import os, re
from itertools import repeat
from math import comb

import numpy as np

K = 4              # power sums of degree 0 .. K-1 over word position
DELTA = 250        # milli-units: words with |charge| below this are buffer words
NEG_WINDOW = 3     # a negator reaches the next three words, or to the next punctuation mark
MOD_WINDOW = 2     # an intensifier or diminisher reaches the next two words

# ---- signature layout (one row per span) ----
L, E = 0, 1                                  # words, charged words
P0, N0 = 2, 2 + K                            # power sums of positive / negative charge start here
NEGP, NEGN, BOOP, BOON, DMPP, DMPN, POSTP, POSTN, SS = range(2 + 2 * K, 2 + 2 * K + 9)
SIZE = 2 + 2 * K + 9

NEGATORS = set("""not no never none nobody nothing neither nor nowhere without cannot cant n't hardly barely scarcely
    ain't aint aren't arent can't couldn't couldnt didn't didnt doesn't doesnt don't dont hadn't hadnt hasn't hasnt
    haven't havent isn't isnt mustn't needn't shouldn't shouldnt wasn't wasnt weren't werent won't wont wouldn't wouldnt""".split())
BOOSTERS = set("""very really extremely absolutely completely totally utterly incredibly so too highly especially
    particularly truly quite super such most deeply thoroughly""".split())
DAMPERS = set("slightly somewhat marginally partly partially mildly moderately fairly rather almost nearly kinda sorta".split())
CONTRAST = set("but however yet nevertheless nonetheless".split())

WORD, NEG, BOOST, DAMP, CONTR, CLAUSE, SENT, SEP = 1, 2, 4, 8, 16, 32, 64, 128
_EMOTICON = re.compile(r"[:;=][-o*']?[)(\]\[dp/\\|3]|<3")
_ASCII = str.maketrans({i: " " for i in range(128)}                 # anything not listed below separates words
                       | {ord(ch): ch for ch in "abcdefghijklmnopqrstuvwxyz'\x00"}
                       | {ord(ch): "." for ch in ".!?"} | {ord(ch): "," for ch in ",;:"})
JOIN = " \x00 "                                                    # separator between texts in a batch
_HERE = os.path.dirname(os.path.abspath(__file__))


def load_lexicon(path=None, delta=None, extra=()):
    """Build the lookup tables from a `word<TAB>charge in milli-units` file.

    path   None for the shipped norm lexicon, "refined" for the one scripts/exp_refine.py builds, or a file
    delta  neutral band in milli-units (default DELTA; the refined lexicon uses 0)
    extra  further words to give an id, with zero charge
    """
    global VOCAB, CHARGE, KIND, _EMO
    if path == "refined":                                      # built by scripts/exp_refine.py; not shipped (it absorbs social bias)
        path, delta = os.path.join(_HERE, "..", "data", "lexicons", "lexicon_refined.tsv"), 0
    delta = DELTA if delta is None else delta
    VOCAB, charge, kind, _EMO = {}, [0], [WORD], {}            # id 0: a word the lexicon does not know
    rows = [line.rstrip("\n").split("\t") for line in open(path or os.path.join(_HERE, "lexicon.tsv"), encoding="utf-8")]
    for w, c in rows + [(w, 0) for w in extra]:
        if _EMOTICON.fullmatch(w):
            _EMO[w] = w = "emoticon" + "".join(chr(97 + int(d)) for d in str(len(_EMO)))
        if w not in VOCAB:
            VOCAB[w] = len(charge)
            charge.append(int(c) if abs(int(c)) >= delta else 0)
            kind.append(WORD)
    for words, flag in ((NEGATORS, NEG), (BOOSTERS, BOOST), (DAMPERS, DAMP), (CONTRAST, CONTR)):
        for w in words:                                        # operators act on other words and carry no charge
            if w not in VOCAB:
                VOCAB[w] = len(charge); charge.append(0); kind.append(WORD)
            charge[VOCAB[w]] = 0
            kind[VOCAB[w]] |= flag
    for ch, flag in ((".", SENT), (",", CLAUSE), ("\x00", SEP)):
        VOCAB[ch] = len(charge); charge.append(0); kind.append(flag)
    CHARGE, KIND = np.array(charge, np.int64), np.array(kind, np.uint8)


load_lexicon()
_TABLE = None


def _clean(text):
    """Lower-case; for non-ASCII text also straighten quotes and spell out emoji (one C-level pass)."""
    global _TABLE
    if text.isascii():
        return text.lower()
    if _TABLE is None:
        _TABLE = {0x2019: "'", 0x2018: "'", 0x201C: '"', 0x201D: '"', 0x2014: " ", 0x2013: " ", 0x2026: ".", 0xFE0F: None, 0x200D: None}
        try:
            for line in open(os.path.join(_HERE, "emoji.tsv"), encoding="utf-8"):
                e, name = line.rstrip("\n").split("\t")
                if len(e.replace("\ufe0f", "")) == 1:              # single code point; sequences are left alone
                    _TABLE[ord(e[0])] = " " + name + " "
        except FileNotFoundError:
            pass
    return text.translate(_TABLE).lower()


def tokens(text):
    """Lower-cased words, emoticons, '.' for sentence ends and ',' for clause breaks.

    Plain string operations only (replace, translate, split): no regular expression runs here.
    """
    text = " " + _clean(text) + " "                                 # padded at both ends, so a quote mark at either edge is stripped
    for emo, word in _EMO.items():                                  # an emoticon counts only after a space
        if emo in text:
            text = text.replace(" " + emo, " " + word + " ")
    return text.translate(_ASCII).replace(".", " . ").replace(",", " , ").replace(" '", " ").replace("' ", " ").split()


# ---------------------------------------------------------------- signatures

def scan(texts, batch=20000):
    """One vectorised pass over a list of texts.

    Returns per-word arrays (document index, word position, word id, charge in hedons,
    and four flags saying whether the word sits in the reach of a negator, an intensifier,
    a diminisher, or after a contrast word) and the number of words in each text.
    Long lists are processed in batches, so working memory does not grow with the list.
    """
    if len(texts) > batch:
        parts = [scan(texts[i:i + batch], batch) for i in range(0, len(texts), batch)]
        out = {k: np.concatenate([p[k] for p in parts]) for k in parts[0] if k != "doc"}
        out["doc"] = np.concatenate([p["doc"] + i * batch for i, p in enumerate(parts)])
        return out
    toks = tokens(JOIN.join(texts))
    ids = np.fromiter(map(VOCAB.get, toks, repeat(0)), np.int32, len(toks))
    n = len(texts)
    if not len(ids):
        z = np.zeros(0)
        return dict(doc=z.astype(int), pos=z, id=z.astype(int), c=z, neg=z > 0, boost=z > 0, damp=z > 0, post=z > 0, words=np.zeros(n))
    kind = KIND[ids]
    sep = kind == SEP
    doc = np.cumsum(sep)
    assert doc[-1] == n - 1, "texts must not contain NUL characters"
    word = (kind & WORD) > 0
    w = np.cumsum(word)                                        # running word count
    idx = np.arange(len(ids))

    def last(mask):                                            # index of the latest token with mask set, or -1
        return np.maximum.accumulate(np.where(mask, idx, -1))

    clause = last((kind & (CLAUSE | SENT | SEP)) > 0)
    sent = last((kind & (SENT | SEP)) > 0)

    def reach(flag, window, stop):                             # is this token inside an operator's scope?
        j = last((kind & flag) > 0)
        gap = w - w[np.maximum(j, 0)]
        return ((j > stop) & (gap >= 1) & (gap <= window))[word]

    start = np.concatenate(([0], w[sep]))                      # words before each text
    d = doc[word]
    return dict(doc=d.astype(np.int32), pos=(w[word] - 1 - start[d]).astype(float), id=ids[word], c=CHARGE[ids[word]] / 1000.0,
                neg=reach(NEG, NEG_WINDOW, clause), boost=reach(BOOST, MOD_WINDOW, clause),
                damp=reach(DAMP, MOD_WINDOW, clause), post=(last((kind & CONTR) > 0) > sent)[word],
                words=np.bincount(d, minlength=n).astype(float))


def sketch(texts):
    """Signatures of one text (row) or a list of texts (matrix), in hedons and dolors.

    One pass over the tokens. Operator context (negation, intensity, contrast) never
    crosses a sentence boundary, so texts cut at sentence ends merge exactly.
    """
    single = isinstance(texts, str)
    S = aggregate(scan([texts] if single else texts))
    return S[0] if single else S


def aggregate(t):
    """Signatures from the per-word arrays of `scan`."""
    n = len(t["words"])
    S = np.zeros((n, SIZE))
    S[:, L] = t["words"]
    m = t["c"] != 0                                            # charged words only from here on
    d, c, pos = t["doc"][m], t["c"][m], t["pos"][m]
    p, q = np.maximum(c, 0), np.maximum(-c, 0)
    S[:, E] = np.bincount(d, minlength=n)
    S[:, SS] = np.bincount(d, weights=c * c, minlength=n)
    x = np.ones_like(pos)
    for k in range(K):
        S[:, P0 + k] = np.bincount(d, weights=p * x, minlength=n)
        S[:, N0 + k] = np.bincount(d, weights=q * x, minlength=n)
        x = x * pos
    for col, flag in ((NEGP, "neg"), (BOOP, "boost"), (DMPP, "damp"), (POSTP, "post")):
        f = t[flag][m]
        S[:, col] = np.bincount(d, weights=p * f, minlength=n)
        S[:, col + 1] = np.bincount(d, weights=q * f, minlength=n)
    return S


def sketch_exact(text):
    """Reference implementation: a plain loop in integer milli-units (1000 = one hedon).

    Slow and exact. It defines what `sketch` computes, and its integer signatures make
    merge, unmerge and prefix queries exact to the last digit.
    """
    s = [0] * SIZE
    neg = boost = damp = 0          # words left in each operator's reach
    post = False                    # inside the clause after a contrast word
    for t in tokens(text):
        i = VOCAB.get(t, 0)
        kind, c = int(KIND[i]), int(CHARGE[i])
        if not kind & WORD:
            neg = boost = damp = 0
            if kind & (SENT | SEP):
                post = False
            continue
        if c:
            p, q = max(c, 0), max(-c, 0)
            s[E] += 1
            s[SS] += c * c
            for k in range(K):
                s[P0 + k] += p * s[L] ** k
                s[N0 + k] += q * s[L] ** k
            for on, col in ((neg, NEGP), (boost, BOOP), (damp, DMPP), (post, POSTP)):
                if on:
                    s[col] += p
                    s[col + 1] += q
        s[L] += 1
        neg, boost, damp = max(neg - 1, 0), max(boost - 1, 0), max(damp - 1, 0)
        if kind & NEG: neg = NEG_WINDOW
        if kind & BOOST: boost = MOD_WINDOW
        if kind & DAMP: damp = MOD_WINDOW
        if kind & CONTR: post = True
    return np.array(s, dtype=object)


def _shift(s, by):
    """Move a signature's position origin back by `by` words (binomial theorem)."""
    out = s.copy()
    for base in (P0, N0):
        for k in range(1, K):
            out[..., base + k] = sum(comb(k, j) * by ** (k - j) * s[..., base + j] for j in range(k + 1))
    return out


def merge(a, b):
    """Signature of span a followed by span b."""
    out = a + _shift(b, a[..., L])
    out[..., L] = a[..., L] + b[..., L]
    return out


def unmerge(ab, a):
    """Signature of b, given the signatures of a+b and of its prefix a."""
    b = ab - a
    out = _shift(b, -a[..., L])
    out[..., L] = ab[..., L] - a[..., L]
    return out


def prefix(S):
    """Row j of the result is the signature of spans 0..j read as one text."""
    before = np.concatenate(([0 * S[0, L]], np.cumsum(S[:, L])[:-1]))
    out = np.cumsum(_shift(S, before), axis=0)
    return out


def span(pre, i, j):
    """Signature of spans i..j inclusive from a prefix table: two rows, constant time.

    Exact for integer tables (rows from `sketch_exact`). For float tables the counts and totals,
    and so density and level, are exact to rounding, but the position sums of a short span deep in
    a long text are lost to cancellation: read the arc or the valence of such a span from
    `fold(S[i:j + 1])`, which merges the span's own rows.
    """
    return pre[j] if i == 0 else unmerge(pre[j], pre[i - 1])


def fold(S):
    """Rows read in order as one text: their forward merge. Accurate in floats at any depth; time grows with the number of rows."""
    return prefix(S)[-1]


# ---------------------------------------------------------------- readouts

def totals(S):
    """(hedons, dolors, words)."""
    return S[..., P0], S[..., N0], S[..., L]


def exposure(S, a=1.0, kappa=0.2):
    """Evidence behind a score: a prior weight, charged words, and buffer words at weight kappa."""
    return np.maximum(a + S[..., E] + kappa * (S[..., L] - S[..., E]), 1e-9)   # empty text: 0 / tiny = 0


def level(S, a=1.0, kappa=0.2):
    """Net charge per unit exposure, in hedons per thousand (negative = dolors)."""
    return 1000 * (S[..., P0] - S[..., N0]) / exposure(S, a, kappa)


def stderr(S, a=1.0, kappa=0.2):
    """Standard error of `level`, treating words as independent draws (delta method for the ratio).

    The level is 1000 R with R = net / exposure, and both vary from word to word: a word adds its
    charge c to the numerator and 1 (charged) or kappa (buffer) to the exposure. With u = c - R w,
    sum(u^2) = SS - 2 R net + R^2 (E + kappa^2 B) and sum(u) = R a. For a = 0, kappa = 1 this is the
    usual standard error of a mean.
    """
    n = np.maximum(S[..., L], 1)
    q = S[..., P0] - S[..., N0]
    x = exposure(S, a, kappa)
    r = q / x
    uu = S[..., SS] - 2 * r * q + r * r * (S[..., E] + kappa * kappa * (S[..., L] - S[..., E]))
    return 1000 * np.sqrt(np.maximum(uu - (r * a) ** 2 / n, 0)) / x


def legendre(S, base):
    """Legendre moments sum_i c_i P_k(x_i), k = 0..3, with x_i = (2i+1)/L - 1 in (-1, 1)."""
    n = np.maximum(S[..., L], 1)
    al, be = 2 / n, 1 / n - 1
    s0, s1, s2, s3 = (S[..., base + k] for k in range(4))
    x1 = al * s1 + be * s0
    x2 = al ** 2 * s2 + 2 * al * be * s1 + be ** 2 * s0
    x3 = al ** 3 * s3 + 3 * al ** 2 * be * s2 + 3 * al * be ** 2 * s1 + be ** 3 * s0
    return np.stack([s0, x1, (3 * x2 - s0) / 2, (5 * x3 - 3 * x1) / 2], -1)


def arc(S, x):
    """Cubic fit of net charge per word at relative positions x in [0, 1]; NaN for spans under four words."""
    m = legendre(S, P0) - legendre(S, N0)
    n = np.maximum(S[..., L], 1)[..., None]
    coef = m * (2 * np.arange(4) + 1) / n
    t = 2 * np.asarray(x) - 1
    basis = np.stack([np.ones_like(t), t, (3 * t ** 2 - 1) / 2, (5 * t ** 3 - 3 * t) / 2])
    out = coef @ basis
    short = np.asarray(S[..., L]) < 4                            # a cubic needs four words; below that there is no arc
    return np.where(short[..., None] if out.ndim > short.ndim else short, np.nan, out)


FEATURES = ["hedons", "dolors", "net x P1(position)", "net x P2(position)", "net x P3(position)",
            "negated hedons", "negated dolors", "intensified net", "diminished net", "net after contrast"]


def features(S, a=1.0, kappa=0.2, position=True, shifters=True, two_channel=True):
    """Design matrix for the calibrated readout: signature entries per unit exposure."""
    S = np.atleast_2d(np.asarray(S, float))
    lp, ln = legendre(S, P0), legendre(S, N0)
    cols = [S[:, P0], S[:, N0]] if two_channel else [S[:, P0] - S[:, N0]]
    if position:
        cols += [lp[:, k] - ln[:, k] for k in (1, 2, 3)]
    if shifters:
        cols += [S[:, NEGP], S[:, NEGN]] if two_channel else [S[:, NEGP] - S[:, NEGN]]
        cols += [S[:, BOOP] - S[:, BOON], S[:, DMPP] - S[:, DMPN], S[:, POSTP] - S[:, POSTN]]
    return np.stack(cols, 1) / exposure(S, a, kappa)[:, None]


def ridge(X, y, weight=None, lam=1e-3):
    """Weighted ridge regression with an unpenalised intercept. Convex, closed form."""
    w = np.ones(len(y)) if weight is None else weight
    w = w / w.sum()
    mx, my = w @ X, w @ y
    Xc = X - mx
    A = (Xc * w[:, None]).T @ Xc
    beta = np.linalg.solve(A + lam * np.trace(A) / len(A) * np.eye(len(A)), (Xc * w[:, None]).T @ (y - my))
    return np.concatenate(([my - mx @ beta], beta))


def predict(coef, X):
    return coef[0] + X @ coef[1:]


# ---------------------------------------------------------------- one-call summary

_CONST = None


def constants():
    """Shipped constants, fitted on six human-rated corpora (see results/accuracy.json)."""
    global _CONST
    if _CONST is None:
        import json
        _CONST = json.load(open(os.path.join(_HERE, "constants.json")))
    return _CONST


def valence(S):
    """Calibrated readout: predicted human valence rating in standard-deviation units (0 = an average text)."""
    k = constants()
    return predict(np.array(k["coef"]), features(S, k["a"], k["kappa"]))


def read(text):
    """Score one text. Totals in hedons and dolors, density per 1,000 words, level with its standard error."""
    S = sketch(text)
    k = constants()
    h, d, n = totals(S)
    return {"words": int(n), "hedons": round(float(h), 3), "dolors": round(float(d), 3),
            "density": round(float(1000 * (h - d) / max(n, 1)), 1),
            "level": round(float(level(S, k["level_a"], k["level_kappa"])), 1),
            "level_se": round(float(stderr(S, k["level_a"], k["level_kappa"])), 1),
            "valence": round(float(valence(S)[0]), 3),
            "arc": [round(float(v), 1) for v in 1000 * arc(S, np.array([0.0, 0.25, 0.5, 0.75, 1.0]))] if n >= 4 else None}
