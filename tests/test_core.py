"""Run: python3 tests/test_core.py   (no data needed; also collected by pytest)"""
import os, random, sys

import numpy as np

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), ".."))
import sgw

WORDS = [w for w in sgw.VOCAB if w.isalpha()]
OPS = sorted(sgw.NEGATORS | sgw.BOOSTERS | sgw.DAMPERS | sgw.CONTRAST)


def sentence(r):
    n = r.randint(1, 25)
    toks = [r.choice(OPS) if r.random() < 0.2 else r.choice(WORDS) + (r.choice([",", ";", ""]) if r.random() < 0.15 else "") for _ in range(n)]
    return " ".join(toks) + r.choice([".", "!", "?"])


def text(r, sentences=None):
    return " ".join(sentence(r) for _ in range(sentences or r.randint(1, 8)))


def test_fast_path_equals_reference():
    r = random.Random(0)
    docs = [text(r) for _ in range(300)] + ["", "no charge here at all"]
    fast = sgw.sketch(docs)
    for d, f in zip(docs, fast):
        ref = sgw.sketch_exact(d).astype(float)
        ref[2:] /= 1000
        ref[sgw.SS] /= 1000
        assert np.allclose(f, ref, rtol=1e-9, atol=1e-9), d
    assert np.allclose(sgw.sketch(docs[5]), fast[5])            # one text == its row in a batch


def test_merge_is_exact_and_associative():
    r = random.Random(1)
    for _ in range(200):
        a, b, c = text(r), text(r), text(r)
        A, B, C = map(sgw.sketch_exact, (a, b, c))
        whole = sgw.sketch_exact(" ".join((a, b, c)))
        left, right = sgw.merge(sgw.merge(A, B), C), sgw.merge(A, sgw.merge(B, C))
        assert list(left) == list(right) == list(whole)        # integers: equal to the last digit
        assert list(sgw.unmerge(whole, A)) == list(sgw.merge(B, C))


def test_any_span_from_two_prefix_rows():
    r = random.Random(2)
    sents = [sentence(r) for _ in range(400)]                   # a "book" of 400 sentences
    S = np.array([sgw.sketch_exact(s) for s in sents], dtype=object)
    pre = sgw.prefix(S)
    for _ in range(100):
        i = r.randrange(400); j = r.randrange(i, 400)
        assert list(sgw.span(pre, i, j)) == list(sgw.sketch_exact(" ".join(sents[i:j + 1])))


def test_legendre_moments_match_direct_sum():
    r = random.Random(3)
    for _ in range(50):
        d = text(r, 6)
        s = sgw.sketch(d)
        toks = [t for t in sgw.tokens(d) if sgw.KIND[sgw.VOCAB.get(t, 0)] & sgw.WORD]
        c = np.array([sgw.CHARGE[sgw.VOCAB.get(t, 0)] for t in toks]) / 1000
        x = (2 * np.arange(len(toks)) + 1) / len(toks) - 1
        direct = [np.sum(np.maximum(c, 0) * P) for P in (1, x, (3 * x ** 2 - 1) / 2, (5 * x ** 3 - 3 * x) / 2)]
        assert np.allclose(sgw.legendre(s, sgw.P0), direct, atol=1e-9)


def test_operators():
    s = sgw.sketch_exact("not good")
    assert s[sgw.NEGP] == s[sgw.P0] > 0                         # "good" sits under the negation
    assert sgw.sketch_exact("not, good")[sgw.NEGP] == 0         # a comma ends the scope
    assert sgw.sketch_exact("bad but good")[sgw.POSTP] > 0 and sgw.sketch_exact("bad. good")[sgw.POSTP] == 0
    assert sgw.sketch_exact("very good")[sgw.BOOP] > 0 and sgw.sketch_exact("slightly bad")[sgw.DMPN] > 0


if __name__ == "__main__":
    for name, f in list(globals().items()):
        if name.startswith("test_"):
            f(); print("ok", name)
