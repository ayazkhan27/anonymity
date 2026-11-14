"""All figures of the paper, from results/*.json. SciencePlots 'science' + 'nature' styles.

    python3 scripts/make_figures.py            # every figure
    python3 scripts/make_figures.py fig1 fig4  # named figures
"""
import json, os, sys

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import scienceplots  # noqa: F401  (registers the styles)

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, ".."))
import sgw

plt.style.use(["science", "nature"])
plt.rcParams.update({"figure.dpi": 200, "savefig.dpi": 400, "axes.linewidth": 0.5, "xtick.minor.visible": False, "ytick.minor.visible": False,
                     "xtick.top": False, "ytick.right": False, "axes.spines.top": False, "axes.spines.right": False,
                     "legend.frameon": False, "font.size": 7, "axes.labelsize": 7, "xtick.labelsize": 6.5, "ytick.labelsize": 6.5})
BLUE, RED, GREEN, PURPLE, GREY, INK = "#0072B2", "#D55E00", "#009E73", "#AA4499", "#8A8A8A", "#222222"
OUT = os.path.join(HERE, "..", "paper", "figures")
FULL, HALF = 7.2, 3.5                                             # inches: two-column and one-column widths
R = lambda name: json.load(open(os.path.join(HERE, "..", "results", name + ".json")))
CONST = json.load(open(os.path.join(HERE, "..", "sgw", "constants.json")))


def panel(ax, letter, x=-0.02, y=1.04):
    ax.text(x, y, letter, transform=ax.transAxes, fontsize=8.5, fontweight="bold", va="bottom", ha="right", color=INK)


def save(fig, name):
    fig.savefig(os.path.join(OUT, name + ".pdf"), bbox_inches="tight", pad_inches=0.02)
    fig.savefig(os.path.join(OUT, name + ".png"), bbox_inches="tight", pad_inches=0.02, dpi=220)
    plt.close(fig)


def fig1():
    """Schematic on a real example: charges, signatures, merge, readouts."""
    A, B = "The service was slow and the soup was not good.", "But the pasta was absolutely wonderful, and we left happy."
    t = sgw.scan([A + " " + B])
    words = [w for w in sgw.tokens(A + " " + B) if sgw.KIND[sgw.VOCAB.get(w, 0)] & sgw.WORD]
    na = int(sgw.sketch(A)[sgw.L])
    fig = plt.figure(figsize=(FULL, 2.75))
    ax = fig.add_axes([0.045, 0.30, 0.50, 0.58])
    x = np.arange(len(words))
    ax.bar(x, np.maximum(t["c"], 0), color=BLUE, width=0.62, lw=0)
    ax.bar(x, -np.maximum(-t["c"], 0), color=RED, width=0.62, lw=0)
    ax.axhline(0, color=INK, lw=0.5)
    ax.axvline(na - 0.5, color=GREY, lw=0.6, ls=(0, (3, 2)))
    ax.set_xticks(x); ax.set_xticklabels(words, rotation=55, ha="right", rotation_mode="anchor")
    for lab, c in zip(ax.get_xticklabels(), t["c"]):
        lab.set_color(INK if c else GREY)
    ax.set_ylim(-0.85, 0.95); ax.set_yticks([-0.5, 0, 0.5]); ax.set_ylabel("word charge")
    ax.text(0.2, 0.86, "hedons", color=BLUE, fontsize=7); ax.text(0.2, -0.80, "dolors", color=RED, fontsize=7)
    ax.tick_params(axis="x", length=0)
    for i in np.flatnonzero(t["neg"] & (t["c"] != 0)):
        ax.annotate("negated", (i, t["c"][i]), (i - 1.2, t["c"][i] + 0.22), fontsize=6, color=INK, arrowprops=dict(arrowstyle="-", lw=0.4, color=GREY))
    for i in np.flatnonzero(t["boost"] & (t["c"] != 0)):
        ax.annotate("intensified", (i, t["c"][i]), (i - 2.6, t["c"][i] + 0.12), fontsize=6, color=INK, arrowprops=dict(arrowstyle="-", lw=0.4, color=GREY))
    post = np.flatnonzero(t["post"])
    ax.plot([post[0] - 0.4, post[-1] + 0.4], [-0.62, -0.62], color=GREY, lw=0.6)
    ax.text(post.mean(), -0.78, "after a contrast word", fontsize=6, color=INK, ha="center")
    ax.text((na - 1) / 2, 1.0, "span A", ha="center", fontsize=7, color=INK); ax.text((na + len(words) - 1) / 2, 1.0, "span B", ha="center", fontsize=7, color=INK)
    panel(ax, "a", x=-0.06, y=1.06)
    # signatures and their merge
    sa, sb = sgw.sketch(A), sgw.sketch(B)
    sab = sgw.merge(sa, sb)
    assert np.allclose(sab, sgw.sketch(A + " " + B))
    bx = fig.add_axes([0.575, 0.30, 0.245, 0.58]); bx.axis("off")
    rows = [("words", sgw.L, "{:.0f}"), ("charged words", sgw.E, "{:.0f}"), ("hedons", sgw.P0, "{:.2f}"), ("dolors", sgw.N0, "{:.2f}"),
            (r"hedons $\times$ position", sgw.P0 + 1, "{:.1f}"), (r"dolors $\times$ position", sgw.N0 + 1, "{:.1f}"), ("negated hedons", sgw.NEGP, "{:.2f}")]
    cols = (0.60, 0.77, 1.0)
    for xx, name in zip(cols, ("A", "B", "A then B")):
        bx.text(xx, 0.97, name, ha="right", fontsize=6.5, fontweight="bold", color=INK)
    bx.plot([0, 1.0], [0.93, 0.93], color=INK, lw=0.5)
    for r, (name, col, f) in enumerate(rows):
        yy = 0.845 - r * 0.105
        bx.text(0.0, yy, name, fontsize=6.3, color=INK)
        for xx, sig in zip(cols, (sa, sb, sab)):
            bx.text(xx, yy, f.format(sig[col]), ha="right", fontsize=6.3, color=INK)
    bx.text(0.0, 0.0, "Counts and charges add. Position sums of B\nare shifted by the length of A before adding.", fontsize=6, color="#555555", va="bottom")
    bx.set_xlim(0, 1.0); bx.set_ylim(0, 1.0)
    panel(bx, "b", x=-0.02, y=1.06)
    # readouts
    cx = fig.add_axes([0.885, 0.64, 0.105, 0.24])
    h, d, n = sgw.totals(sab)
    cx.barh([1, 0], [h, d], color=[BLUE, RED], height=0.6, lw=0)
    cx.set_yticks([1, 0]); cx.set_yticklabels(["hedons", "dolors"]); cx.tick_params(axis="y", length=0)
    cx.set_xlim(0, 4.6); cx.set_xticks([0, 2, 4])
    for v, yy in ((h, 1), (d, 0)):
        cx.text(v + 0.12, yy, f"{v:.2f}", va="center", fontsize=6.3)
    panel(cx, "c", x=-0.55, y=1.14)
    dx = fig.add_axes([0.885, 0.30, 0.105, 0.19])
    g = np.linspace(0, 1, 100)
    dx.plot(g, 1000 * sgw.arc(sab, g), color=INK, lw=0.9)
    dx.axhline(0, color=GREY, lw=0.4)
    dx.set_xticks([0, 1]); dx.set_xticklabels(["start", "end"]); dx.set_ylabel("net per\n1,000 words", fontsize=6)
    lvl, se = sgw.level(sab, CONST["level_a"], CONST["level_kappa"]), sgw.stderr(sab, CONST["level_a"], CONST["level_kappa"])
    dx.set_title(f"level {lvl:+.0f} $\\pm$ {se:.0f}", fontsize=6.5, pad=5, loc="left")
    save(fig, "fig1_schematic")


def fig5():
    """One novel read at three scales from one pass, then all novels."""
    sys.path.insert(0, HERE)
    from exp_books import BOOKS, chapters, window_arc, GRID
    ch = chapters(os.path.join(BOOKS, "2701.txt"))                 # Moby-Dick
    sents = [s for c in ch for s in c]
    S = sgw.sketch(sents)
    bounds = np.concatenate(([0], np.cumsum([len(c) for c in ch])))
    C = np.array([sgw.prefix(S[a:b])[-1] for a, b in zip(bounds[:-1], bounds[1:])])
    book = sgw.prefix(C)[-1]
    per_k = lambda X: 1000 * (X[..., sgw.P0] - X[..., sgw.N0]) / np.maximum(X[..., sgw.L], 1)
    se_k = lambda X: sgw.stderr(X, 0, 1.0)
    fig = plt.figure(figsize=(FULL, 4.3))
    ax = fig.add_axes([0.07, 0.56, 0.60, 0.36])
    mid = (np.cumsum(C[:, sgw.L]) - C[:, sgw.L] / 2) / book[sgw.L]
    ax.errorbar(mid, per_k(C), yerr=1.96 * se_k(C), fmt="o", ms=1.6, color=GREY, ecolor="#C9C9C9", elinewidth=0.5, lw=0, zorder=1)
    c = sgw.scan([" ".join(sents)])["c"]
    ax.plot(GRID, 1000 * window_arc(c, 0.10), color=BLUE, lw=0.9, zorder=3)
    ax.plot(GRID, 1000 * sgw.arc(book, GRID), color=INK, lw=1.1, zorder=4)
    ax.axhline(per_k(book), color=GREY, lw=0.5, ls=(0, (3, 2)), zorder=2)
    ax.set_xlim(0, 1); ax.set_ylim(-12, 88); ax.set_xticks([0, 0.25, 0.5, 0.75, 1]); ax.set_xticklabels(["start", "", "middle", "", "end"])
    ax.set_ylabel("net charge per 1,000 words")
    ax.text(0.015, 0.19, "chapters, with 95\% intervals", color="#6F6F6F", transform=ax.transAxes, fontsize=6.3)
    ax.text(0.015, 0.115, "moving window, 10\% of the book", color=BLUE, transform=ax.transAxes, fontsize=6.3)
    ax.text(0.015, 0.04, "cubic arc from the signature", color=INK, transform=ax.transAxes, fontsize=6.3)
    ax.text(0.995, per_k(book) + 2.5, "whole book", color="#6F6F6F", fontsize=6.3, ha="right")
    h, d, n = sgw.totals(book)
    ax.set_title(f"Moby-Dick: {n:,.0f} words, {h:,.0f} hedons, {d:,.0f} dolors, {len(ch)} chapters", fontsize=7, loc="left", pad=4)
    panel(ax, "a", x=-0.07)
    # zoom: the last chapter in 12 equal runs of sentences, each read from two prefix rows
    k = len(ch) - 1
    Sk = S[bounds[k]:bounds[k + 1]]
    pre = sgw.prefix(Sk)
    cuts = np.linspace(0, len(Sk), 13).astype(int)
    seg = np.array([sgw.span(pre, a, b - 1) for a, b in zip(cuts[:-1], cuts[1:])])
    bx = fig.add_axes([0.75, 0.56, 0.24, 0.36])
    base = per_k(book)
    bx.bar(np.arange(12), per_k(seg) - base, bottom=base, color=[BLUE if v >= base else RED for v in per_k(seg)], width=0.7, lw=0)
    bx.errorbar(np.arange(12), per_k(seg), yerr=1.96 * se_k(seg), fmt="none", ecolor=INK, elinewidth=0.5)
    bx.axhline(base, color="#6F6F6F", lw=0.5, ls=(0, (3, 2))); bx.set_xticks([0, 11]); bx.set_xticklabels(["start", "end"])
    bx.text(11.45, base + 2.5, "whole book", color="#6F6F6F", fontsize=6.3, ha="right")
    bx.set_title(f"final chapter ({C[k, sgw.L]:,.0f} words), twelve spans", fontsize=7, loc="left", pad=4)
    panel(bx, "b", x=-0.14)
    # all novels
    B = R("books")
    ratio = np.array([b["between"] / b["null_mean"] for b in B["books"]])
    cx = fig.add_axes([0.07, 0.09, 0.26, 0.32])
    cx.hist(ratio, bins=np.geomspace(0.8, 40, 28), color=GREY, lw=0)
    cx.axvline(1, color=INK, lw=0.6, ls=(0, (3, 2))); cx.set_xscale("log"); cx.set_xticks([1, 2, 5, 10, 20, 40]); cx.set_xticklabels(["1", "2", "5", "10", "20", "40"])
    cx.set_xlabel("between-chapter variance / shuffled-sentence null"); cx.set_ylabel("novels")
    cx.text(1.06, cx.get_ylim()[1] * 0.92, "chance", fontsize=6.3, color=INK)
    panel(cx, "c", x=-0.17)
    dx = fig.add_axes([0.41, 0.09, 0.24, 0.32])
    degs = [1, 2, 3, 5, 7, 9]
    q = np.array([[np.percentile([b["r2"][str(k)] for b in B["books"]], p) for k in degs] for p in (25, 50, 75)])
    dx.fill_between(degs, q[0], q[2], color="#DADADA", lw=0)
    dx.plot(degs, q[1], color=INK, lw=1, marker="o", ms=2.5)
    dx.set_xticks(degs); dx.set_ylim(0, 0.85); dx.set_xlabel("polynomial degree kept in the signature"); dx.set_ylabel("share of window-arc variance")
    dx.annotate("shipped", (3, q[1][2]), (3.6, q[1][2] - 0.17), fontsize=6.3, arrowprops=dict(arrowstyle="-", lw=0.4, color=GREY))
    panel(dx, "d", x=-0.19)
    ex = fig.add_axes([0.73, 0.09, 0.26, 0.32])
    modes = np.array(B["summary"]["svd_modes"]); t = 2 * GRID - 1
    leg = [t, (3 * t ** 2 - 1) / 2]
    for m, lg, col, lab in ((modes[0], leg[0], BLUE, "first mode"), (modes[1], leg[1], RED, "second mode")):
        m = m * np.sign(m @ lg)
        ex.plot(GRID, m / np.abs(m).max(), color=col, lw=1)
        ex.plot(GRID, lg / np.abs(lg).max(), color=col, lw=0.6, ls=(0, (3, 2)))
    ex.text(0.62, 0.02, "first mode", color=BLUE, fontsize=6.3); ex.text(0.30, -0.95, "second mode", color=RED, fontsize=6.3)
    ex.set_title("dashed: Legendre $P_1$ and $P_2$", fontsize=6.5, loc="left", pad=4)
    ex.set_xticks([0, 1]); ex.set_xticklabels(["start", "end"]); ex.set_yticks([-1, 0, 1]); ex.set_ylim(-1.1, 1.15); ex.set_ylabel("arc shape (scaled)")
    panel(ex, "e", x=-0.14)
    save(fig, "fig5_book")


def fig6():
    """What chunking does to a book's score, pooled against mean-of-chapters, and the critics."""
    B = R("books"); v = B["summary"]["vader"]
    fig = plt.figure(figsize=(FULL, 4.3))
    sizes = ["1", "5", "25", "125", "chapter"]
    ax = fig.add_axes([0.07, 0.58, 0.25, 0.34])
    V = np.array(v["book_scores"])
    ax.plot(range(5), V.T, color="#C9C9C9", lw=0.35)
    ax.plot(range(5), V.mean(0), color=INK, lw=1.2, marker="o", ms=2.5)
    ax.set_xticks(range(5)); ax.set_xticklabels(sizes); ax.set_xlabel("sentences per chunk"); ax.set_ylabel("VADER: mean chunk score")
    ax.set_ylim(-1.05, 1.05); ax.set_title("bounded score, averaged over chunks", fontsize=7, loc="left", pad=4)
    panel(ax, "a", x=-0.17)
    bx = fig.add_axes([0.41, 0.58, 0.25, 0.34])
    G = 1000 * np.array(v["sgw_merged"])
    bx.plot(range(4), G.T, color="#C9C9C9", lw=0.35)
    bx.plot(range(4), G.mean(0), color=INK, lw=1.2, marker="o", ms=2.5)
    bx.set_xticks(range(4)); bx.set_xticklabels(sizes[:4]); bx.set_xlabel("sentences per chunk"); bx.set_ylabel("net charge per 1,000 words")
    bx.set_title("signatures, merged over chunks", fontsize=7, loc="left", pad=4)
    panel(bx, "b", x=-0.19)
    cx = fig.add_axes([0.75, 0.58, 0.24, 0.34])
    f1 = [100 * v["pair_flip_vs_sentence"][k] for k in sizes]
    f2 = [100 * v["sgw_mean_flip_vs_sentence"][k] for k in sizes[:4]]
    cx.plot(range(5), f1, color=RED, lw=1, marker="o", ms=2.5); cx.plot(range(4), f2, color=BLUE, lw=1, marker="s", ms=2.3); cx.plot(range(4), [0] * 4, color=INK, lw=1, marker="D", ms=2.3)
    cx.text(2.55, f1[3] + 2.0, "VADER", color=RED, fontsize=6.3); cx.text(1.75, f2[2] - 4.2, "density, chunks averaged", color=BLUE, fontsize=6.3); cx.text(0.9, 1.1, "density, signatures merged", color=INK, fontsize=6.3)
    cx.set_xticks(range(5)); cx.set_xticklabels(sizes); cx.set_xlabel("sentences per chunk"); cx.set_ylabel("book pairs that swap order (\%)"); cx.set_ylim(-1.5, 24)
    panel(cx, "c", x=-0.17)
    dx = fig.add_axes([0.07, 0.09, 0.25, 0.34])
    mi, ma = 1000 * np.array([b["micro"] for b in B["books"]]), 1000 * np.array([b["macro"] for b in B["books"]])
    lim = [min(mi.min(), ma.min()) - 3, max(mi.max(), ma.max()) + 3]
    dx.plot(lim, lim, color=INK, lw=0.5); dx.scatter(mi, ma, s=2.5, color=GREY, lw=0)
    dx.set_xlim(lim); dx.set_ylim(lim); dx.set_xlabel("pooled density of the novel"); dx.set_ylabel("mean of its chapter densities")
    panel(dx, "d", x=-0.17)
    C = R("critics")["pairs"]
    ex = fig.add_axes([0.52, 0.09, 0.47, 0.34])
    short = lambda n: n.split()[-1]
    for i, p in enumerate(C):
        y0 = len(C) - 1 - i
        for j, (key, col) in enumerate((("total", INK), ("within", BLUE), ("mix", RED))):
            yy = y0 + 0.24 - 0.24 * j
            ex.plot([p["lo"][j], p["hi"][j]], [yy, yy], color=col, lw=0.8)
            ex.plot(p[key], yy, "o", color=col, ms=2.6)
    ex.axvline(0, color=GREY, lw=0.5)
    ex.set_yticks(range(len(C))); ex.set_yticklabels([f"{short(p['a'])} minus {short(p['b'])}" for p in C][::-1]); ex.tick_params(axis="y", length=0)
    ex.set_xlabel("difference in net charge per 1,000 words between two critics")
    ex.set_title("total", color=INK, fontsize=6.5, loc="left", pad=4)
    ex.text(0.10, 1.025, "within the same rating", color=BLUE, fontsize=6.5, transform=ex.transAxes); ex.text(0.42, 1.025, "from the mix of ratings", color=RED, fontsize=6.5, transform=ex.transAxes)
    panel(ex, "e", x=-0.30)
    save(fig, "fig6_aggregation")


CORPORA = ["emobank", "semeval", "fb", "sst", "scale", "yelp"]
CNAME = dict(emobank="EmoBank", semeval="Tweets", fb="Facebook posts", sst="SST sentences", scale="Film reviews", yelp="Yelp reviews")
LONG = {"EmoBank": "EmoBank", "SemEval-2018 tweets": "Tweets", "Facebook posts": "Facebook posts", "SST sentences": "SST sentences",
        "Film reviews": "Film reviews", "Yelp reviews": "Yelp reviews", "Film review paragraphs": "Review paragraphs", "Novel chapters": "Novel chapters"}
ROWS = [("SGW", "Signature, calibrated", BLUE, "o", True), ("SGW level", "Signature, level only", BLUE, "o", False),
        ("SGW, refined charges", "Signature, refined charges", BLUE, "s", False),
        ("VADER", "VADER", GREY, "o", True), ("Hedonometer", "Hedonometer", GREY, "o", True), ("TextBlob", "TextBlob", GREY, "o", True),
        ("Bag-of-words ridge (other corpora)", "Bag-of-words ridge, other corpora", GREEN, "o", True),
        ("TF-IDF ridge (in-corpus)", "Bag-of-words ridge, same corpus", GREEN, "o", False),
        ("DistilBERT (SST-2)", "DistilBERT (SST-2)", PURPLE, "o", True), ("Twitter-RoBERTa", "Twitter-RoBERTa", PURPLE, "o", True)]


def fig2():
    """Agreement with human ratings on held-out portions, cost, and ablations."""
    A, Sp = R("accuracy"), R("speed")
    fig = plt.figure(figsize=(FULL, 4.75))
    rows = [r for r in ROWS if r[0] in A["emobank"]["methods"]]
    for j, c in enumerate(CORPORA):
        ax = fig.add_axes([0.235 + j * 0.128, 0.50, 0.112, 0.43])
        for k, (key, label, col, mk, filled) in enumerate(rows):
            m = A[c]["methods"][key]; y = len(rows) - 1 - k
            ax.plot(m["r_ci"], [y, y], color=col, lw=0.8)
            ax.plot(m["r"], y, mk, color=col, ms=3.2, mfc=col if filled else "white", mew=0.8)
        ax.set_ylim(-0.6, len(rows) - 0.4); ax.set_xlim(0.1, 0.95); ax.set_xticks([0.2, 0.5, 0.8]); ax.grid(axis="x", color="#E6E6E6", lw=0.4); ax.set_axisbelow(True)
        ax.set_title(CNAME[c], fontsize=6.8, pad=3)
        ax.set_yticks(range(len(rows))); ax.tick_params(axis="y", length=0)
        ax.set_yticklabels([r[1] for r in rows][::-1] if j == 0 else [])
        if j == 0:
            panel(ax, "a", x=-1.95, y=1.02)
        if j == 2:
            ax.set_xlabel("Pearson $r$ with human ratings, test portion (95\% interval)", x=1.1)
    # cost
    bx = fig.add_axes([0.075, 0.09, 0.36, 0.29])
    mean = A["mean_r_primary"]
    pts = [("SGW", Sp["sgw"], BLUE, "Signature", (8, -2)), ("VADER", Sp["vader"], GREY, "VADER", (7, -2)), ("TextBlob", Sp["textblob"], GREY, "TextBlob", (7, -2)),
           ("Hedonometer", Sp["hedonometer"], GREY, "Hedonometer", (-6, -9)), ("DistilBERT (SST-2)", Sp["distilbert_1"], PURPLE, "DistilBERT", (7, -2)),
           ("Twitter-RoBERTa", Sp["roberta_1"], PURPLE, "Twitter-RoBERTa", (7, -2))]
    for key, x, col, lab, off in pts:
        if key in mean:
            bx.plot(x, mean[key], "o", color=col, ms=4)
            bx.annotate(lab, (x, mean[key]), xytext=off, textcoords="offset points", fontsize=6.3, color=INK, ha="left" if off[0] > 0 else "right")
    bx.set_xscale("log"); bx.set_xlim(8, 3e6); bx.set_ylim(0.36, 0.78)
    bx.set_xlabel("sentences scored per second, one CPU core"); bx.set_ylabel("mean $r$, five rated corpora")
    panel(bx, "b", x=-0.14)
    # ablations
    cx = fig.add_axes([0.62, 0.09, 0.37, 0.29])
    abl = [("SGW, no dilution", "buffer words do not dilute"), ("SGW, no shrinkage", "no neutral prior"), ("SGW, no shifters", "no operator channels"),
           ("SGW, one channel", "one bipolar channel"), ("SGW, no position", "no position moments")]
    for k, (key, lab) in enumerate(abl):
        y = len(abl) - 1 - k
        d = [A[c]["methods"][key]["sgw_minus"] for c in CORPORA]
        cx.plot(d, [y] * len(d), "o", color="#BDBDBD", ms=2.6, mew=0)
        cx.plot(np.mean(d), y, "D", color=BLUE, ms=3.6)
    cx.axvline(0, color=INK, lw=0.5); cx.set_yticks(range(len(abl))); cx.set_yticklabels([a[1] for a in abl][::-1]); cx.tick_params(axis="y", length=0)
    cx.set_ylim(-0.6, len(abl) - 0.4); cx.set_xlabel("loss in $r$ when the part is removed (grey: each corpus; blue: mean)")
    panel(cx, "c", x=-0.52)
    save(fig, "fig2_accuracy")


def fig3():
    """Where charge sits along a text, what position readers weigh, and how two parts combine."""
    P = R("position")
    fig = plt.figure(figsize=(FULL, 4.5))
    groups = [(["EmoBank", "SemEval-2018 tweets", "Facebook posts", "SST sentences"], "sentences and posts"),
              (["Film reviews", "Yelp reviews", "Film review paragraphs", "Novel chapters"], "reviews, paragraphs, chapters")]
    cols = [BLUE, RED, GREEN, PURPLE]
    x = (np.arange(10) + 0.5) / 10
    for j, (names, title) in enumerate(groups):
        ax = fig.add_axes([0.06 + j * 0.345, 0.56, 0.20, 0.36])
        ends = []
        for name, col in zip(names, cols):
            d = P["where"][name]["decile_density"]
            ax.plot(x, d, color=col, lw=1, marker="o", ms=2)
            ends.append([d[-1], LONG[name], col])
        ends.sort()
        for k in range(1, len(ends)):                               # keep the direct labels from colliding
            ends[k][0] = max(ends[k][0], ends[k - 1][0] + (0.085 if j == 0 else 0.024))
        for yv, lab, col in ends:
            ax.text(1.0, yv, lab, color=col, fontsize=6.3, va="center")
        ax.axhline(1, color=GREY, lw=0.5, ls=(0, (3, 2)))
        ax.set_xlim(0, 0.98); ax.set_xticks([0.05, 0.5, 0.95]); ax.set_xticklabels(["start", "middle", "end"])
        ax.set_title(title, fontsize=6.8, loc="left", pad=3)
        if j == 0:
            ax.set_ylabel("charge per word (text mean = 1)"); panel(ax, "a", x=-0.22)
    # treebank composition
    T = P["tree"]
    cx = fig.add_axes([0.815, 0.56, 0.175, 0.36])
    sets = [("all nodes", "all phrases"), ("both parts two or more words", "both parts\n2+ words"), ("top node of each sentence", "whole\nsentences")]
    for k, (key, lab) in enumerate(sets):
        for side, col, dx in (("left", BLUE, -0.13), ("right", RED, 0.13)):
            e, lo, hi = T[key][side]
            cx.plot([k + dx, k + dx], [lo, hi], color=col, lw=0.9); cx.plot(k + dx, e, "o", color=col, ms=3.2)
    cx.set_xticks(range(3)); cx.set_xticklabels([s[1] for s in sets], fontsize=6); cx.set_xlim(-0.5, 2.5)
    cx.set_ylabel("weight in the phrase's human rating"); cx.set_ylim(0.55, 0.85)
    cx.text(-0.4, 0.835, "later part", color=RED, fontsize=6.3); cx.text(-0.4, 0.812, "earlier part", color=BLUE, fontsize=6.3)
    panel(cx, "b", x=-0.25)
    # fitted kernels
    names = ["EmoBank", "SemEval-2018 tweets", "Facebook posts", "SST sentences", "Film reviews", "Yelp reviews"]
    for j, name in enumerate(names):
        k = P["kernel"][name]
        ax = fig.add_axes([0.065 + j * 0.157, 0.09, 0.128, 0.30])
        ax.fill_between(k["x"], k["lo"], k["hi"], color="#D6E4EE", lw=0)
        ax.plot(k["x"], k["kernel"], color=BLUE, lw=1)
        ax.axhline(1, color=GREY, lw=0.5, ls=(0, (3, 2)))
        ax.set_ylim(0, 2.2); ax.set_xticks([0, 1]); ax.set_xticklabels(["start", "end"]); ax.set_title(LONG[name], fontsize=6.8, pad=3)
        c = k["contrast"]
        ax.set_xlabel(f"{c[0]:+.2f} [{c[1]:+.2f}, {c[2]:+.2f}]", fontsize=6, labelpad=1)
        if j == 0:
            ax.set_ylabel("fitted weight of a word's charge"); panel(ax, "c", x=-0.30)
        else:
            ax.set_yticklabels([])
    save(fig, "fig3_position")


def fig4():
    """Dilution: human phrase ratings, rated intensity against buffer words, fit along kappa."""
    P = R("position"); T = P["tree"]["dilution"]
    fig = plt.figure(figsize=(FULL, 2.45))
    ax = fig.add_axes([0.065, 0.17, 0.255, 0.72])
    xs = np.arange(5)
    for side, col, lab, dx in (("neutral words after", RED, "neutral words after the rated part", 0.06), ("neutral words before", BLUE, "neutral words before it", -0.06)):
        e = np.array([c["kept"] for c in T[side]])
        ax.plot(np.stack([xs + dx, xs + dx]), e[:, 1:].T, color=col, lw=0.9)
        ax.plot(xs + dx, e[:, 0], color=col, lw=1, marker="o", ms=3)
    ax.text(0.03, 0.10, "neutral words after the rated part", color=RED, fontsize=6.3, transform=ax.transAxes)
    ax.text(0.03, 0.03, "neutral words before it", color=BLUE, fontsize=6.3, transform=ax.transAxes)
    ax.set_xticks(xs); ax.set_xticklabels([c["words"] for c in T["either"]]); ax.set_ylim(0.45, 0.9)
    ax.set_xlabel("neutral words joined to a rated phrase"); ax.set_ylabel("share of the phrase's rating kept")
    panel(ax, "a", x=-0.17)
    names = ["EmoBank", "SemEval-2018 tweets", "Facebook posts", "SST sentences", "Film reviews", "Yelp reviews"]
    bx = fig.add_axes([0.475, 0.17, 0.16, 0.72])
    for k, name in enumerate(names):
        e, lo, hi = P["dilute"][name]["partial"]; y = len(names) - 1 - k
        bx.plot([lo, hi], [y, y], color=INK, lw=0.9); bx.plot(e, y, "o", color=INK, ms=3.2)
    bx.axvline(0, color=GREY, lw=0.5); bx.set_yticks(range(len(names))); bx.set_yticklabels([LONG[n] for n in names][::-1]); bx.tick_params(axis="y", length=0)
    bx.set_xlabel("rank correlation of rated intensity\nwith buffer words, charge held fixed"); bx.set_ylim(-0.6, len(names) - 0.4)
    panel(bx, "b", x=-0.85)
    cx = fig.add_axes([0.725, 0.17, 0.20, 0.72])
    cols = [BLUE, RED, GREEN, PURPLE, INK, GREY]
    ends = []
    for name, col in zip(names, cols):
        d = P["dilute"][name]; r = np.array(d["r_by_kappa"]) - d["r_by_kappa"][0]
        cx.plot(range(len(r)), r, color=col, lw=1, marker="o", ms=2)
        ends.append([r[-1], LONG[name], col])
    ends.sort()
    for k in range(1, len(ends)):
        ends[k][0] = max(ends[k][0], ends[k - 1][0] + 0.0075)
    for yv, lab, col in ends:
        cx.text(7.15, yv, lab, color=col, fontsize=6.3, va="center")
    cx.axhline(0, color=GREY, lw=0.5)
    cx.set_xticks(range(8)); cx.set_xticklabels(["0", ".05", ".1", ".2", ".5", "1", "2", "4"]); cx.set_xlim(-0.2, 7.05)
    cx.set_xlabel(r"weight $\kappa$ of a buffer word in the exposure"); cx.set_ylabel(r"change in held-out $r$ from $\kappa=0$")
    panel(cx, "c", x=-0.24)
    save(fig, "fig4_dilution")


def figE1():
    """Extended data: are the error bars honest, and do merged signatures track human story labels?"""
    U, Tl = R("uncertainty"), R("tales")
    fig = plt.figure(figsize=(FULL, 2.35))
    ax = fig.add_axes([0.065, 0.18, 0.21, 0.70])
    cols = [BLUE, RED, GREEN, PURPLE, INK, GREY]
    for c, col in zip(CORPORA, cols):
        b = U[c]["bands"]
        ax.plot([x["predicted"] for x in b], [x["rmse"] for x in b], color=col, lw=0.8, marker="o", ms=2.5)
    ax.plot([0.55, 1.15], [0.55, 1.15], color=GREY, lw=0.5, ls=(0, (3, 2)))
    ax.set_xlabel("predicted error (s.d. units)"); ax.set_ylabel("observed error on the test portion")
    panel(ax, "a", x=-0.20)
    bx = fig.add_axes([0.40, 0.18, 0.15, 0.70])
    cov = [100 * U[c]["coverage90"] for c in CORPORA]
    bx.barh(range(6), cov[::-1], color=cols[::-1], height=0.62, lw=0)
    bx.axvline(90, color=INK, lw=0.6, ls=(0, (3, 2))); bx.set_xlim(70, 100); bx.set_yticks(range(6)); bx.set_yticklabels([CNAME[c] for c in CORPORA][::-1]); bx.tick_params(axis="y", length=0)
    bx.set_xlabel("ratings inside the\n90\% interval (\%)")
    panel(bx, "b", x=-0.72)
    cx = fig.add_axes([0.655, 0.18, 0.33, 0.70])
    cx.hist(Tl["arc_r"], bins=np.linspace(-1, 1, 21), color=GREY, lw=0)
    cx.axvline(0, color=INK, lw=0.5); cx.axvline(Tl["arc_r_median"], color=BLUE, lw=1)
    cx.text(Tl["arc_r_median"] + 0.04, cx.get_ylim()[1] * 0.93, "median", color=BLUE, fontsize=6.3)
    cx.set_xlabel("correlation between the signature arc and the arc of human labels, per tale"); cx.set_ylabel("fairy tales")
    panel(cx, "c", x=-0.11)
    save(fig, "figE1_uncertainty_tales")


FIGS = dict(fig1=fig1, fig2=fig2, fig3=fig3, fig4=fig4, fig5=fig5, fig6=fig6, figE1=figE1)


if __name__ == "__main__":
    os.makedirs(OUT, exist_ok=True)
    for name in sys.argv[1:] or FIGS:
        FIGS[name]()
        print("made", name)
