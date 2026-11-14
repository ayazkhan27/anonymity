"""Part-whole analyses on novels, fairy tales and film critics. No parameter is fitted here.

  novels   pooled level vs mean of chapter levels, between-chapter variance against a
           sentence-shuffle null, polynomial arc vs sliding-window arc, VADER under chunking
  tales    Alm's sentence-level human emotion labels vs signatures merged up to the story
  critics  Kitagawa decomposition of differences between critics' text levels
"""
import glob, json, re, sys
from multiprocessing import Pool

import numpy as np
from numpy.polynomial import legendre as npleg

from common import *

BOOKS = os.path.join(HERE, "..", "data", "gutenberg", "books")
START = re.compile(r"\*\*\* ?START OF (?:THE|THIS) PROJECT GUTENBERG EBOOK[^\n]*\n", re.I)
END = re.compile(r"\*\*\* ?END OF (?:THE|THIS) PROJECT GUTENBERG EBOOK", re.I)
CHAPTER = re.compile(r"^[ \t]*chapter[ \t]+(?:[ivxlcdm]+|\d+|[a-z-]+)\b[^\n]*$", re.I | re.M)
SENT = re.compile(r"(?<=[.!?])\s+")
GRID = np.linspace(0.005, 0.995, 100)


def chapters(path, min_words=300):
    """Chapters of a Gutenberg text as lists of sentences; front matter and stubs dropped."""
    t = open(path, encoding="utf-8", errors="ignore").read()
    m, e = START.search(t), END.search(t)
    t = t[m.end() if m else 0: e.start() if e else len(t)]
    parts = CHAPTER.split(t)[1:]
    # every chapter is closed with a full stop, so that no operator reaches across a chapter boundary
    return [SENT.split(" ".join(p.split()) + " .") for p in parts if len(p.split()) >= min_words]


def lvl(S):
    """Density: net charge per word (hedons minus dolors over words)."""
    return (S[..., sgw.P0] - S[..., sgw.N0]) / np.maximum(S[..., sgw.L], 1)


def expo(S):
    return S[..., sgw.L]


def window_arc(c, frac):
    """Moving average of per-word net charge, window = frac of the text, at GRID."""
    w = max(int(frac * len(c)), 1)
    cs = np.concatenate(([0], np.cumsum(c)))
    mid = (GRID * len(c)).astype(int)
    lo = np.clip(mid - w // 2, 0, len(c) - w)
    return (cs[lo + w] - cs[lo]) / w


def one_book(path, perms=200):
    ch = chapters(path)
    if len(ch) < 10:
        return None
    sents = [s for c in ch for s in c]
    if sum(len(s.split()) for s in sents) < 20000:
        return None
    S = sgw.sketch(sents)
    bounds = np.concatenate(([0], np.cumsum([len(c) for c in ch])))
    C = np.array([sgw.prefix(S[a:b])[-1] for a, b in zip(bounds[:-1], bounds[1:])])   # chapter signatures by merging
    book = sgw.prefix(C)[-1]
    whole = sgw.sketch(" ".join(sents))                                                 # one pass over the whole book
    merge_err = float(np.max(np.abs(book - whole) / np.maximum(np.abs(whole), 1e-12)))
    D, lam = expo(C), lvl(C)
    micro, macro = float(lvl(book)), float(lam.mean())
    identity = float(np.mean((D - D.mean()) * (lam - lam.mean())) / D.mean())           # Cov(D, level) / mean D
    # between-chapter variance against shuffling sentences across chapters
    q, d = S[:, sgw.P0] - S[:, sgw.N0], expo(S)
    between = lambda qq, dd: float(np.sum(dd * (qq / np.maximum(dd, 1e-9) - qq.sum() / dd.sum()) ** 2) / dd.sum())
    obs = between(np.add.reduceat(q, bounds[:-1]), np.add.reduceat(d, bounds[:-1]))
    rng = np.random.default_rng(0)
    null = []
    for _ in range(perms):
        p = rng.permutation(len(q))
        null.append(between(np.add.reduceat(q[p], bounds[:-1]), np.add.reduceat(d[p], bounds[:-1])))
    # arcs: polynomial from the signature vs moving window over the per-word series
    c = sgw.scan([" ".join(sents)])["c"]
    win = window_arc(c, 0.10)
    x = (2 * np.arange(len(c)) + 1) / len(c) - 1
    r2 = {}
    for deg in (1, 2, 3, 5, 7, 9):
        fit = npleg.legval(2 * GRID - 1, npleg.legfit(x, c, deg))
        r2[deg] = float(1 - np.sum((win - fit) ** 2) / np.sum((win - win.mean()) ** 2))
    cubic = sgw.arc(book, GRID)
    assert np.allclose(cubic, npleg.legval(2 * GRID - 1, npleg.legfit(x, c, 3)), rtol=1e-5, atol=1e-8)   # signature arc against direct least squares, to one part in 1e5
    return dict(id=os.path.basename(path)[:-4], chapters=len(ch), sentences=len(sents), words=float(book[sgw.L]),
                hedons=float(book[sgw.P0]), dolors=float(book[sgw.N0]), micro=micro, macro=macro, identity=identity,
                merge_err=merge_err, between=obs, null_mean=float(np.mean(null)), p=float((1 + np.sum(np.array(null) >= obs)) / (1 + perms)),
                r2=r2, win=win.tolist(), cubic=cubic.tolist(), chapter_levels=lam.tolist(), chapter_exposure=D.tolist(),
                chapter_se=(sgw.stderr(C, 0, 1.0) / 1000).tolist())


def vader_chunks(path):
    """Book-level VADER score as the mean compound over chunks of n sentences, for several n."""
    from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer
    v = SentimentIntensityAnalyzer()
    ch = chapters(path)
    sents = [s for c in ch for s in c]
    out = {}
    for n in (1, 5, 25, 125):
        out[n] = float(np.mean([v.polarity_scores(" ".join(sents[i:i + n]))["compound"] for i in range(0, len(sents), n)]))
    whole = [v.polarity_scores(" ".join(c))["compound"] for c in ch]
    out["chapter"], out["saturated"] = float(np.mean(whole)), float(np.mean(np.abs(whole) > 0.99))
    S = sgw.sketch(sents)
    for n in (1, 5, 25, 125):                                   # the same chunkings through signatures: merged, they are one number
        parts = np.array([sgw.prefix(S[i:i + n])[-1] for i in range(0, len(sents), n)])
        out[f"sgw_{n}"] = float(lvl(sgw.prefix(parts)[-1]))
        out[f"sgw_mean_{n}"] = float(lvl(parts).mean())         # what averaging chunk scores would give instead
    return os.path.basename(path)[:-4], out


def novels():
    paths = sorted(glob.glob(os.path.join(BOOKS, "*.txt")))
    with Pool(12) as pool:
        books = [b for b in pool.map(one_book, paths) if b]
    micro, macro = np.array([b["micro"] for b in books]), np.array([b["macro"] for b in books])
    i, j = np.triu_indices(len(books), 1)
    flips = np.sign(micro[i] - micro[j]) != np.sign(macro[i] - macro[j])
    W = np.array([b["win"] for b in books]); W = W - W.mean(1, keepdims=True)
    _, sv, Vt = np.linalg.svd(W, full_matrices=False)
    t = 2 * GRID - 1
    P = np.stack([t, (3 * t ** 2 - 1) / 2, (5 * t ** 3 - 3 * t) / 2])
    cos = [float(abs(Vt[k] @ P[k]) / np.linalg.norm(P[k])) for k in range(3)]
    keep = [b["id"] for b in books][:60]
    with Pool(12) as pool:
        vc = dict(pool.map(vader_chunks, [os.path.join(BOOKS, k + ".txt") for k in keep]))
    sizes = (1, 5, 25, 125, "chapter")
    V = np.array([[vc[k][n] for n in sizes] for k in keep])
    a, b = np.triu_indices(len(keep), 1)
    vflip = {str(n): float(np.mean(np.sign(V[a, 0] - V[b, 0]) != np.sign(V[a, m] - V[b, m]))) for m, n in enumerate(sizes)}
    G = np.array([[vc[k][f"sgw_{n}"] for n in (1, 5, 25, 125)] for k in keep])
    GM = np.array([[vc[k][f"sgw_mean_{n}"] for n in (1, 5, 25, 125)] for k in keep])
    summary = dict(
        n_books=len(books), n_files=len(paths), words=float(np.sum([b["words"] for b in books])),
        chapters_median=float(np.median([b["chapters"] for b in books])),
        merge_err_max=float(max(b["merge_err"] for b in books)),
        identity_err_max=float(max(abs(b["micro"] - b["macro"] - b["identity"]) for b in books)),
        gap_over_between_book_sd=float(np.median(np.abs(micro - macro)) / micro.std()),
        gap_abs_median=float(np.median(np.abs(micro - macro))), level_sd=float(micro.std()),
        pair_flip_share=float(flips.mean()), n_pairs=int(len(flips)),
        hetero_ratio_median=float(np.median([b["between"] / b["null_mean"] for b in books])),
        hetero_ratio_iqr=[float(v) for v in np.percentile([b["between"] / b["null_mean"] for b in books], [25, 75])],
        hetero_sig_share=float(np.mean([b["p"] <= 0.01 for b in books])),
        arc_r2_median={str(d): float(np.median([b["r2"][d] for b in books])) for d in (1, 2, 3, 5, 7, 9)},
        svd_var_top3=[float(v) for v in (sv[:3] ** 2 / np.sum(sv ** 2))], svd_legendre_cosine=cos, svd_modes=Vt[:3].tolist(),
        vader=dict(n_books=len(keep), mean_by_chunk={str(n): float(V[:, m].mean()) for m, n in enumerate(sizes)},
                   pair_flip_vs_sentence=vflip, saturated_chapter_share=float(np.mean([vc[k]["saturated"] for k in keep])),
                   book_scores=V.tolist(), sgw_merged_spread=float(np.max(np.ptp(G, axis=1) / np.abs(G[:, 0]))),
                   sgw_merged=G.tolist(), sgw_mean_of_chunks=GM.tolist(),
                   sgw_mean_flip_vs_sentence={str(n): float(np.mean(np.sign(GM[a, 0] - GM[b, 0]) != np.sign(GM[a, m] - GM[b, m]))) for m, n in enumerate((1, 5, 25, 125))}))
    save("books", dict(summary=summary, books=books))
    print(json.dumps({k: v for k, v in summary.items() if k not in ("svd_modes", "vader")}, indent=1))
    print("vader", json.dumps({k: v for k, v in summary["vader"].items() if k not in ("book_scores", "sgw_merged", "sgw_mean_of_chunks")}, indent=1))


VAL = {"H": 1, "Su+": 1, "N": 0, "A": -1, "D": -1, "F": -1, "Sa": -1, "Su-": -1, "S": -1}


def tales():
    """Human sentence labels (two annotators) against sentence scores and merged story signatures."""
    coef = json.load(open(os.path.join(HERE, "..", "sgw", "constants.json")))
    rows = []
    for path in sorted(glob.glob(os.path.join(HERE, "..", "data", "alm", "*", "*", "emmood", "*.emmood"))):
        sents, human = [], []
        for line in open(path, encoding="latin-1"):
            f = line.rstrip("\n").split("\t")
            if len(f) >= 4 and all(l in VAL for l in f[1].split(":")):
                sents.append(f[3]); human.append(np.mean([VAL[l] for l in f[1].split(":")]))
        if len(sents) >= 10:
            rows.append((os.path.basename(path), sents, np.array(human)))
    flat = [s for _, ss, _ in rows for s in ss]
    h = np.concatenate([r[2] for r in rows])
    story = np.concatenate([np.full(len(r[1]), i) for i, r in enumerate(rows)])
    S = sgw.sketch(flat)
    z = sgw.predict(np.array(coef["coef"]), sgw.features(S, coef["a"], coef["kappa"]))
    v = vader_scores(flat)
    sent = {name: [fmt(x) for x in boot(lambda i, s=s: pearson(s[i], h[i]), story)] for name, s in (("SGW", z), ("VADER", v))}
    bounds = np.concatenate(([0], np.cumsum([len(r[1]) for r in rows])))
    B = np.array([sgw.prefix(S[a:b])[-1] for a, b in zip(bounds[:-1], bounds[1:])])
    hm = np.array([r[2].mean() for r in rows])
    vm = np.array([v[a:b].mean() for a, b in zip(bounds[:-1], bounds[1:])])
    ids = np.arange(len(rows))
    zm = np.array([z[a:b].mean() for a, b in zip(bounds[:-1], bounds[1:])])
    zB = sgw.predict(np.array(coef["coef"]), sgw.features(B, coef["a"], coef["kappa"]))
    vw = vader_scores([" ".join(r[1]) for r in rows])
    story_r = {name: [fmt(x) for x in boot(lambda i, s=s: pearson(s[i], hm[i]), ids)] for name, s in (
        ("SGW merged", lvl(B)), ("SGW calibrated, merged", zB), ("SGW calibrated, mean of sentences", zm),
        ("VADER mean of sentences", vm), ("VADER whole story", vw))}
    saturated = float(np.mean(np.abs(vw) > 0.99))
    # arcs: cubic fit to the human labels over sentence position vs the cubic arc of the merged signature
    arc_r = []
    for (name, ss, hh), sig in zip(rows, B):
        x = (2 * np.arange(len(hh)) + 1) / len(hh) - 1
        human_arc = npleg.legval(2 * GRID - 1, npleg.legfit(x, hh, 3))
        arc_r.append(pearson(human_arc, sgw.arc(sig, GRID)))
    arc_r = np.array(arc_r)
    rng = np.random.default_rng(0)
    out = dict(n_stories=len(rows), n_sentences=len(flat), sentence_r=sent, story_r=story_r, vader_whole_saturated=saturated,
               arc_r_median=float(np.median(arc_r)), arc_r_positive_share=float(np.mean(arc_r > 0)),
               arc_r_sign_test_p=float(__import__("scipy.stats").stats.binomtest(int(np.sum(arc_r > 0)), len(arc_r)).pvalue),
               arc_r=arc_r.tolist())
    save("tales", out)
    print("tales", json.dumps({k: v for k, v in out.items() if k != "arc_r"}, indent=1))


def critics():
    """Split each pair's difference in pooled text level into a within-rating part and a rating-mix part."""
    base = os.path.join(HERE, "..", "data", "scale")
    data = {}
    for author in sorted(os.listdir(f"{base}/scale_data/scaledata")):
        ids = open(f"{base}/scale_data/scaledata/{author}/id.{author}").read().split()
        lab = np.array(open(f"{base}/scale_data/scaledata/{author}/label.4class.{author}").read().split(), int)
        texts = [open(f"{base}/scale_whole_review/scale_whole_review/{author}/txt.parag/{i}.txt", encoding="latin-1").read() for i in ids]
        S = sgw.sketch(texts)
        data[author.replace("+", " ")] = (S[:, sgw.P0] - S[:, sgw.N0], expo(S), lab)

    def parts(A, B, ia=None, ib=None):
        qa, da, la = (v if ia is None else v[ia] for v in data[A]); qb, db, lb = (v if ib is None else v[ib] for v in data[B])
        wa, wb = (np.array([d[l == s].sum() for s in range(4)]) / d.sum() for d, l in ((da, la), (db, lb)))
        ra, rb = (np.array([q[l == s].sum() / d[l == s].sum() for s in range(4)]) for q, d, l in ((qa, da, la), (qb, db, lb)))
        total = qa.sum() / da.sum() - qb.sum() / db.sum()
        within, mix = np.sum((wa + wb) / 2 * (ra - rb)), np.sum((ra + rb) / 2 * (wa - wb))
        return np.array([total, within, mix])

    names, out = list(data), []
    rng = np.random.default_rng(0)
    for x in range(4):
        for y in range(x + 1, 4):
            A, B = names[x], names[y]
            est = parts(A, B)
            draws = np.array([parts(A, B, rng.integers(0, len(data[A][0]), len(data[A][0])), rng.integers(0, len(data[B][0]), len(data[B][0]))) for _ in range(2000)])
            lo, hi = np.percentile(draws, [2.5, 97.5], axis=0)
            out.append(dict(a=A, b=B, total=1000 * est[0], within=1000 * est[1], mix=1000 * est[2], lo=(1000 * lo).tolist(), hi=(1000 * hi).tolist(),
                            identity_err=float(abs(est[0] - est[1] - est[2])), reversal=bool(np.sign(est[0]) != np.sign(est[1]))))
            print(f"{A:20s} vs {B:20s} total {1000*est[0]:+6.1f}  within-rating {1000*est[1]:+6.1f}  rating mix {1000*est[2]:+6.1f}  reversal={out[-1]['reversal']}")
    save("critics", dict(pairs=out, n={k: int(len(v[0])) for k, v in data.items()}))


def parts():
    """Transformers on 300 film reviews: the score of the review against the mean score of its sentences."""
    df = LOADERS["scale"]()["test"].iloc[:300]
    counts = np.load(os.path.join(CACHE, "scale_sentence_counts.npy"))
    b = np.concatenate(([0], np.cumsum(counts)))
    out = {"n_reviews": int(len(df)), "sentences": int(counts.sum())}
    for name, label in (("distilbert", "DistilBERT (SST-2)"), ("roberta", "Twitter-RoBERTa")):
        whole = np.load(os.path.join(CACHE, f"{name}_scale_test.npy"))[:300]
        sent = np.load(os.path.join(CACHE, f"{name}_scale_sentences.npy"))
        mean = np.array([sent[i:j].mean() for i, j in zip(b[:-1], b[1:])])
        out[label] = dict(r_between=pearson(whole, mean), sign_differs=float(np.mean(np.sign(whole) != np.sign(mean))),
                          abs_gap_median=float(np.median(np.abs(whole - mean))), r_rating_whole=pearson(whole, df.y.values), r_rating_sentence_mean=pearson(mean, df.y.values),
                          saturated_whole=float(np.mean(np.abs(whole) > 0.9)))
        print(label, {k: round(v, 3) for k, v in out[label].items()})
    # the plan's H3 for VADER: score of the whole review against the mean score of its sentences
    sent_text = [[x for x in SENT.split(t.replace("\n", " ")) if x.strip()] for t in df.text]
    whole = vader_scores(list(df.text))
    mean = np.array([vader_scores(ss).mean() for ss in sent_text])
    out["VADER"] = dict(r_between=pearson(whole, mean), sign_differs=float(np.mean(np.sign(whole) != np.sign(mean))), abs_gap_median=float(np.median(np.abs(whole - mean))),
                        r_rating_whole=pearson(whole, df.y.values), r_rating_sentence_mean=pearson(mean, df.y.values), saturated_whole=float(np.mean(np.abs(whole) > 0.99)))
    print("VADER", {k: round(v, 3) for k, v in out["VADER"].items()})
    S = sgw.sketch(list(df.text))
    out["SGW density r with rating"] = pearson(lvl(S), df.y.values)
    save("parts", out)


def footer(critic="Scott+Renshaw", opening="Visit Scott Renshaw's"):
    """How much one critic's standard sign-off adds to his pooled density (a check added after review; the reviews are scored as distributed)."""
    base = os.path.join(HERE, "..", "data", "scale", "scale_whole_review", "scale_whole_review", critic, "txt.parag")
    texts = [[p.strip() for p in open(os.path.join(base, f), encoding="latin-1").read().split("\n") if p.strip()] for f in sorted(os.listdir(base))]
    dens = lambda S: float(1000 * (S[:, sgw.P0].sum() - S[:, sgw.N0].sum()) / S[:, sgw.L].sum())
    whole = dens(sgw.sketch([" ".join(t) for t in texts]))
    cut = dens(sgw.sketch([" ".join(t[:-1] if t[-1].startswith(opening) else t) for t in texts]))
    out = dict(critic=critic.replace("+", " "), reviews=len(texts), with_footer=int(sum(t[-1].startswith(opening) for t in texts)), density=whole, density_without=cut, added=whole - cut)
    path = os.path.join(RESULTS, "critics.json")
    res = json.load(open(path)); res["footer"] = out; save("critics", res)
    print(out)


if __name__ == "__main__":
    for name in sys.argv[1:] or ("novels", "tales", "critics", "footer"):
        dict(novels=novels, tales=tales, critics=critics, parts=parts, footer=footer)[name]()
