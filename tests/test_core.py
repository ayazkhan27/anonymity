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


def test_cut_inside_a_sentence():
    """At a cut between two words of one sentence, counts, totals, position sums and squared charges still merge exactly."""
    r = random.Random(7)
    keep = [sgw.L, sgw.E] + list(range(sgw.P0, sgw.P0 + sgw.K)) + list(range(sgw.N0, sgw.N0 + sgw.K)) + [sgw.SS]
    for _ in range(300):
        words = text(r).split()
        if len(words) < 2:
            continue
        cut = r.randrange(1, len(words))
        m = sgw.merge(sgw.sketch_exact(" ".join(words[:cut])), sgw.sketch_exact(" ".join(words[cut:])))
        whole = sgw.sketch_exact(" ".join(words))
        assert [m[i] for i in keep] == [whole[i] for i in keep]


def test_arc_needs_four_words():
    assert sgw.read("good day")["arc"] is None and sgw.read("a good day for a walk")["arc"] is not None
    assert np.isnan(sgw.arc(sgw.sketch("good day"), np.array([0.5]))).all()
    assert np.isfinite(sgw.arc(sgw.sketch(["good day", "a good day for a walk"]), np.array([0.2, 0.8]))[1]).all()


def test_operators():
    s = sgw.sketch_exact("not good")
    assert s[sgw.NEGP] == s[sgw.P0] > 0                         # "good" sits under the negation
    assert sgw.sketch_exact("not, good")[sgw.NEGP] == 0         # a comma ends the scope
    assert sgw.sketch_exact("bad but good")[sgw.POSTP] > 0 and sgw.sketch_exact("bad. good")[sgw.POSTP] == 0
    assert sgw.sketch_exact("very good")[sgw.BOOP] > 0 and sgw.sketch_exact("slightly bad")[sgw.DMPN] > 0


def test_every_batch_row_equals_the_single_text():
    """Messy edges: quotes, unknown words, digits, emoticons, emoji, no final full stop."""
    r = random.Random(4)
    extra = ["'wonderful'", '"awful"', "zxqv", "2019", ":)", ":(", "\U0001F600", "caf\u00e9", "don't", "\u2018fine\u2019"]
    docs = []
    for _ in range(200):
        words = [r.choice(extra) if r.random() < 0.3 else r.choice(WORDS) for _ in range(r.randint(1, 15))]
        docs.append(" ".join(words) + r.choice(["", ".", "!", "'", '"']))
    batch = sgw.sketch(docs)
    for d, row in zip(docs, batch):
        assert np.allclose(sgw.sketch(d), row, rtol=1e-12, atol=1e-12), d
        ref = sgw.sketch_exact(d).astype(float)
        ref[2:] /= 1000
        ref[sgw.SS] /= 1000
        assert np.allclose(row, ref, rtol=1e-9, atol=1e-9), d


def test_float_span_totals_and_fold_at_depth():
    """In floats a prefix query keeps totals at any depth; position sums of a deep span come from fold."""
    r = random.Random(5)
    sents = [sentence(r) for _ in range(12000)]                 # about 200,000 words
    S = sgw.sketch(sents)
    pre = sgw.prefix(S)
    for i, j in ((11990, 11999), (6000, 6003), (11999, 11999)):
        direct = sgw.sketch(" ".join(sents[i:j + 1]))
        flat = [sgw.L, sgw.E, sgw.P0, sgw.N0, sgw.NEGP, sgw.SS]
        assert np.allclose(sgw.span(pre, i, j)[flat], direct[flat], rtol=1e-7, atol=1e-7)
        assert np.allclose(sgw.fold(S[i:j + 1]), direct, rtol=1e-9, atol=1e-9)


def test_standard_error():
    r = random.Random(6)
    d = text(r, 40)
    S = sgw.sketch(d)
    toks = [t for t in sgw.tokens(d) if sgw.KIND[sgw.VOCAB.get(t, 0)] & sgw.WORD]
    c = np.array([sgw.CHARGE[sgw.VOCAB.get(t, 0)] for t in toks]) / 1000
    assert np.isclose(sgw.stderr(S, 0, 1.0), 1000 * c.std() / np.sqrt(len(c)))   # density: the standard error of a mean
    w = np.where(c != 0, 1.0, 0.2)                                            # level: the ratio estimator, word by word
    R = c.sum() / (1 + w.sum())
    u = c - R * w
    assert np.isclose(sgw.stderr(S, 1, 0.2), 1000 * np.sqrt(np.sum(u * u) - u.sum() ** 2 / len(c)) / (1 + w.sum()))


if __name__ == "__main__":
    for name, f in list(globals().items()):
        if name.startswith("test_"):
            f(); print("ok", name)
