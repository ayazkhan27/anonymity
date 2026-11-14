"""Write paper/numbers.tex and the generated tables from results/*.json.

Every number in the manuscript is a macro defined here, so the text cannot drift from the results.
A result file that is missing leaves its macros undefined, and LaTeX then stops at the first one.
"""
import glob, json, os, re

import numpy as np

HERE = os.path.dirname(os.path.abspath(__file__))
RES, PAPER = os.path.join(HERE, "..", "results"), os.path.join(HERE, "..", "paper")
CORPORA = ["emobank", "semeval", "fb", "sst", "scale", "yelp"]
CN = dict(emobank="EmoBank", semeval="Tweets", fb="Facebook", sst="SST", scale="Film reviews", yelp="Yelp")
TAG = dict(emobank="Emo", semeval="Tw", fb="Fb", sst="Sst", scale="Film", yelp="Yelp")
macros = {}


def R(name):
    try:
        return json.load(open(os.path.join(RES, name + ".json")))
    except FileNotFoundError:
        return None


def M(name, value, fmt="{}"):
    macros[name] = fmt.format(value).replace("%", "\\%")


def ci(name, triple, fmt="{:.2f}", scale=1):
    e, lo, hi = (scale * v for v in triple)
    M(name, e, fmt); macros[name + "CI"] = f"[{fmt.format(lo)}, {fmt.format(hi)}]"


def sci(x):
    """x as m x 10^e with the mantissa rounded up, so that 'below' statements stay true."""
    e = int(np.floor(np.log10(x)))
    m = int(np.ceil(x / 10 ** e - 1e-9))
    if m == 10:
        m, e = 1, e + 1
    return f"{m}\\times10^{{{e}}}"


lex = R("lexicon")
if lex:
    M("nLabmt", lex["labMT"]["n"], "{:,}"); M("nVader", lex["VADER"]["n"], "{:,}"); M("nShared", lex["VADER"]["shared"], "{:,}")
    M("rLexAgree", lex["VADER"]["r_with_labMT"], "{:.2f}"); M("nLexicon", lex["open"]["n"], "{:,}")
    words = dict(l.rstrip("\n").split("\t") for l in open(os.path.join(HERE, "..", "sgw", "lexicon.tsv"), encoding="utf-8"))
    M("nCharged", sum(abs(int(v)) >= 250 for v in words.values()), "{:,}")
    M("hdLove", int(words["love"]) / 1000, "{:.2f}"); M("dlTerrible", -int(words["terrible"]) / 1000, "{:.2f}")
    for k in ("NRC-VAD", "Warriner"):
        if k in lex:
            M("r" + k.replace("-", ""), lex[k]["r_with_labMT"], "{:.2f}")
sig = glob.glob(os.path.join(HERE, "..", "data", "cache", "sigall_*.npy"))
if len(sig) == 6:
    S = np.vstack([np.load(p) for p in sig])
    M("chargedShare", 100 * S[:, 1].sum() / S[:, 0].sum(), "{:.0f}")

acc = R("accuracy")
if acc:
    names = {"SGW": "Sgw", "SGW level": "Level", "SGW, refined charges": "Refined", "VADER": "Vader", "VADER, mean of sentences": "VaderSent", "Hedonometer": "Hedo", "TextBlob": "Blob",
             "Bag-of-words ridge (other corpora)": "BowPool", "TF-IDF ridge (in-corpus)": "Tfidf", "DistilBERT (SST-2)": "Distil", "Twitter-RoBERTa": "Roberta",
             "SGW, in-corpus constants": "Own", "SGW, no position": "NoPos", "SGW, no dilution": "NoDil", "SGW, no shifters": "NoShift",
             "SGW, one channel": "OneChan", "SGW, no shrinkage": "NoShrink"}
    for key, tag in names.items():
        if key in acc["mean_r_primary"]:
            M("mean" + tag, acc["mean_r_primary"][key], "{:.2f}")
            for c in CORPORA:
                m = acc[c]["methods"][key]
                M(f"r{tag}{TAG[c]}", m["r"], "{:.2f}")
                macros[f"d{tag}{TAG[c]}"] = f"{m['sgw_minus']:+.3f} [{m['sgw_minus_ci'][0]:+.3f}, {m['sgw_minus_ci'][1]:+.3f}]"
            M("loss" + tag, np.mean([acc[c]["methods"][key]["sgw_minus"] for c in CORPORA]), "{:.3f}")
            M("wins" + tag, sum(acc[c]["methods"][key]["sgw_minus_ci"][0] > 0 for c in CORPORA))
            M("losses" + tag, sum(acc[c]["methods"][key]["sgw_minus_ci"][1] < 0 for c in CORPORA))
    M("locoA", " or ".join(sorted({f"{acc[c]['loco']['a']:g}" for c in CORPORA}, key=float))); M("locoKappa", " or ".join(sorted({f"{acc[c]['loco']['kappa']:g}" for c in CORPORA}, key=float)))
    M("shipA", acc["shipped"]["a"], "{:g}"); M("shipKappa", acc["shipped"]["kappa"], "{:g}")
    for c in CORPORA:
        M("n" + TAG[c], acc[c]["n"], "{:,}")
    # the fitted composition constants, shipped model (all corpora)
    b = acc["shipped"]["coef"]
    M("negHedon", b[6] / b[1], "{:.2f}"); M("negDolor", b[7] / b[2], "{:.2f}"); M("boostRatio", b[8] / ((b[1] - b[2]) / 2), "{:.2f}")
    M("dampRatio", b[9] / ((b[1] - b[2]) / 2), "{:.2f}"); M("postRatio", b[10] / ((b[1] - b[2]) / 2), "{:.2f}"); M("dolorOverHedon", -b[2] / b[1], "{:.2f}")
    # table of every method on every corpus
    order = ["SGW", "SGW level", "SGW, in-corpus constants", "SGW, refined charges", "VADER", "VADER, mean of sentences", "Hedonometer", "TextBlob",
             "Bag-of-words ridge (other corpora)", "TF-IDF ridge (in-corpus)", "DistilBERT (SST-2)", "Twitter-RoBERTa"]
    label = {"VADER": "VADER, whole text", "VADER, mean of sentences": "VADER, mean of sentence scores", "SGW": "Signature, calibrated", "SGW level": "Signature, level only", "SGW, in-corpus constants": "Signature, constants from the same corpus",
             "SGW, refined charges": "Signature, refined charges", "Bag-of-words ridge (other corpora)": "Bag-of-words ridge, other corpora",
             "TF-IDF ridge (in-corpus)": "Bag-of-words ridge, same corpus"}
    rows = []
    for key in order:
        if key in acc["emobank"]["methods"]:
            cells = [f"{acc[c]['methods'][key]['r']:.2f} {{\\scriptsize[{acc[c]['methods'][key]['r_ci'][0]:.2f}, {acc[c]['methods'][key]['r_ci'][1]:.2f}]}}" for c in CORPORA]
            rows.append(f"{label.get(key, key)} & " + " & ".join(cells) + f" & {acc['mean_r_primary'][key]:.2f} \\\\")
    open(os.path.join(PAPER, "table_accuracy.tex"), "w").write(
        "\\begin{tabular}{@{}l" + "c" * 7 + "@{}}\n\\toprule\n & " + " & ".join(f"{CN[c]}" for c in CORPORA) + " & Mean \\\\\n"
        + "$n$ (test portion) & " + " & ".join(f"{acc[c]['n']:,}" for c in CORPORA) + " & \\\\\n\\midrule\n" + "\n".join(rows) + "\n\\bottomrule\n\\end{tabular}\n")
    feats = ["intercept", "hedons", "dolors", "net charge $\\times P_1$", "net charge $\\times P_2$", "net charge $\\times P_3$",
             "negated hedons", "negated dolors", "intensified net charge", "diminished net charge", "net charge after contrast"]
    open(os.path.join(PAPER, "table_constants.tex"), "w").write(
        "\\begin{tabular}{@{}lr@{}}\n\\toprule\nSignature entry (per unit exposure) & Coefficient \\\\\n\\midrule\n"
        + "\n".join(f"{f} & {v:+.3f} \\\\" for f, v in zip(feats, b)) + "\n\\bottomrule\n\\end{tabular}\n")

if acc:
    from scipy.stats import t as tdist
    P5 = CORPORA[:5]                                                # the primary set: continuous human ratings
    for key, tag in (("SGW", "Sgw"), ("VADER", "Vader"), ("SGW level", "Level"), ("DistilBERT (SST-2)", "Distil"), ("Twitter-RoBERTa", "Roberta")):
        M("rho" + tag, np.mean([acc[c]["methods"][key]["rho"] for c in P5]), "{:.3f}")
        M("mean" + tag + "Three", acc["mean_r_primary"][key], "{:.3f}"); M("mean" + tag + "Six", acc["mean_r"][key], "{:.2f}")
    d = np.array([acc[c]["methods"]["VADER"]["sgw_minus"] for c in P5])
    h = tdist.ppf(0.975, len(d) - 1) * d.std(ddof=1) / np.sqrt(len(d))
    macros["dVaderMean"] = f"{d.mean():+.3f}"; macros["dVaderMeanCI"] = f"[{d.mean() - h:+.3f}, {d.mean() + h:+.3f}]"
    M("winsVaderPrimary", sum(acc[c]["methods"]["VADER"]["sgw_minus_ci"][0] > 0 for c in P5))
    M("winsDistil", sum(acc[c]["methods"]["DistilBERT (SST-2)"]["sgw_minus_ci"][0] > 0 for c in CORPORA))
    M("locoLevelA", " or ".join(sorted({f"{acc[c]['loco']['level_a']:g}" for c in CORPORA}, key=float))); M("locoLevelKappa", " or ".join(sorted({f"{acc[c]['loco']['level_kappa']:g}" for c in CORPORA}, key=float)))
    M("ownAMax", max(acc[c]["own"]["a"] for c in CORPORA), "{:g}")
    rows = []
    for key in order:                                               # the registered secondary outcome
        if key in acc["emobank"]["methods"]:
            cells = [f"{acc[c]['methods'][key]['rho']:.2f} {{\\scriptsize[{acc[c]['methods'][key]['rho_ci'][0]:.2f}, {acc[c]['methods'][key]['rho_ci'][1]:.2f}]}}" for c in CORPORA]
            rows.append(f"{label.get(key, key)} & " + " & ".join(cells) + f" & {np.mean([acc[c]['methods'][key]['rho'] for c in P5]):.2f} \\\\")
    open(os.path.join(PAPER, "table_spearman.tex"), "w").write(
        "\\begin{tabular}{@{}l" + "c" * 7 + "@{}}\n\\toprule\n & " + " & ".join(f"{CN[c]}" for c in CORPORA) + " & Mean \\\\\n\\midrule\n" + "\n".join(rows) + "\n\\bottomrule\n\\end{tabular}\n")

    def readings(b):                                                # the plain readings of a coefficient vector, as in exp_uncertainty.constants_ci
        base = (b[1] - b[2]) / 2
        return [-b[2] / b[1], 1 + b[6] / b[1], 1 + b[7] / b[2], b[8] / base]
    loco = np.array([readings(acc[c]["loco"]["coef"]) for c in CORPORA])
    for j, tag in enumerate(("Dolor", "NegH", "NegD", "Boost")):
        f = "{:.2f}" if tag == "Dolor" else "{:+.2f}"
        M("loco" + tag + "Lo", loco[:, j].min(), f); M("loco" + tag + "Hi", loco[:, j].max(), f)
    cell = lambda v: " & ".join(f"{x:+.2f}" for x in v)
    cc0 = R("constants_ci")                                         # the shipped row uses the same estimates as the text
    ship = [cc0[k][0] for k in ("dolor_over_hedon", "negated_hedon_multiplier", "negated_dolor_multiplier", "intensifier_gain")] if cc0 else readings(acc["shipped"]["coef"])
    rows = [f"Shipped fit, six corpora ($a={acc['shipped']['a']:g}$, $\\kappa={acc['shipped']['kappa']:g}$) & " + cell(ship) + " \\\\", "\\midrule"]
    rows += [f"Other five corpora, {CN[c]} left out ($a={acc[c]['loco']['a']:g}$, $\\kappa={acc[c]['loco']['kappa']:g}$) & " + cell(loco[k]) + " \\\\" for k, c in enumerate(CORPORA)]
    rows += ["\\midrule"] + [f"{CN[c]} alone ($a={acc[c]['own']['a']:g}$, $\\kappa={acc[c]['own']['kappa']:g}$) & " + cell(readings(acc[c]["own"]["coef"])) + " \\\\" for c in CORPORA]
    open(os.path.join(PAPER, "table_readings.tex"), "w").write(
        "\\begin{tabular}{@{}lrrrr@{}}\n\\toprule\nFit & Dolor / hedon & Negated hedon & Negated dolor & Intensifier \\\\\n\\midrule\n" + "\n".join(rows) + "\n\\bottomrule\n\\end{tabular}\n")

ref = R("refine")
if ref:
    M("refWords", ref["shipped"]["n_words"], "{:,}"); M("refNew", ref["shipped"]["n_new"], "{:,}")
    if "group_terms" in ref:
        G = ref["group_terms"]
        M("grpTerms", len(G)); M("grpDown", ref["group_terms_down"]); M("grpUp", ref["group_terms_up"])
        for w in ("women", "muslim", "gay", "obama"):
            M("grp" + w.title() + "Norm", G[w][0], "{:+.2f}"); M("grp" + w.title() + "Fit", G[w][1], "{:+.2f}")
        ws = list(G)
        half = (len(ws) + 1) // 2
        rows = [" & ".join(f"\\emph{{{w}}} & {G[w][0]:+.2f} & {G[w][1]:+.2f}" for w in pair) + " \\\\" for pair in zip(ws[:half], ws[half:])]
        open(os.path.join(PAPER, "table_groups.tex"), "w").write(
            "\\begin{tabular}{@{}lrr@{\\hspace{2em}}lrr@{}}\n\\toprule\nWord & Norm & Refined & Word & Norm & Refined \\\\\n\\midrule\n" + "\n".join(rows) + "\n\\bottomrule\n\\end{tabular}\n")

sp = R("speed")
if sp:
    M("cpu", sp["cpu"]); M("cores", sp["cores"]); M("sgwRate", round(sp["sgw"], -3), "{:,.0f}"); M("sgwWords", sp["sgw_words_per_s"] / 1e6, "{:.1f}")
    M("sgwAllCores", sp["sgw_all_cores"] / 1e6, "{:.1f}"); M("vaderRate", round(sp["vader"], -2), "{:,.0f}"); M("blobRate", round(sp["textblob"], -2), "{:,.0f}")
    M("hedoRate", round(sp["hedonometer"], -3), "{:,.0f}"); M("distilRate", sp["distilbert_1"], "{:.0f}"); M("robertaRate", sp["roberta_1"], "{:.0f}")
    M("distilRateAll", sp[f"distilbert_{sp['cores']}"], "{:.0f}"); M("robertaRateAll", sp[f"roberta_{sp['cores']}"], "{:.0f}")
    M("speedDistil", round(sp["sgw"] / sp["distilbert_1"], -2), "{:,.0f}"); M("speedRoberta", round(sp["sgw"] / sp["roberta_1"], -2), "{:,.0f}")
    M("speedVader", sp["sgw"] / sp["vader"], "{:.0f}")
    M("billionHours", 1e9 / sp["sgw"] / 3600, "{:.1f}"); M("billionMinutesAll", 1e9 / sp["sgw_all_cores"] / 60, "{:.0f}")
    M("billionDaysDistil", 1e9 / sp["distilbert_1"] / 86400, "{:,.0f}"); M("billionDaysDistilAll", 1e9 / sp[f"distilbert_{sp['cores']}"] / 86400, "{:.0f}")
    M("streamMillions", sp["stream"]["sentences"] / 1e6, "{:.0f}"); M("streamSeconds", sp["stream"]["seconds"], "{:.0f}"); M("streamGb", sp["stream"]["peak_rss_gb"], "{:.1f}")
    M("wordsPerSentence", sp["words_per_sentence"], "{:.0f}"); M("parSpeedup", sp["sgw_all_cores"] / sp["sgw"], "{:.1f}"); M("speedTimes", " and ".join(f"{round(sp['sgw'] / sp[k], -2):,.0f}" for k in ("distilbert_1", "roberta_1")))
    t, n = np.array([s[1] for s in sp["scaling"]]), np.array([s[0] for s in sp["scaling"]])
    M("scalingSlope", np.polyfit(np.log(n[1:]), np.log(t[1:]), 1)[0], "{:.2f}")
tm = R("transformers_meta")
if tm:
    M("truncShare", 100 * tm["share_truncated_film_reviews"], "{:.0f}")

pos = R("position")
if pos:
    if "tree" in pos:
        T = pos["tree"]
        M("treeNodes", T["n_nodes"], "{:,}"); M("treeMatch", 100 * T["match_rate"], "{:.0f}")
        for key, tag in (("all nodes", "All"), ("both parts two or more words", "Two"), ("top node of each sentence", "Top")):
            ci("wLeft" + tag, T[key]["left"]); ci("wRight" + tag, T[key]["right"]); ci("wDiff" + tag, T[key]["right_minus_left"], "{:+.3f}")
        for side, tag in (("neutral words after", "After"), ("neutral words before", "Before"), ("either", "Either")):
            ci("kept" + tag + "One", T["dilution"][side][0]["kept"]); ci("kept" + tag + "Nine", T["dilution"][side][-1]["kept"])
            M("n" + tag + "One", T["dilution"][side][0]["n"], "{:,}"); M("n" + tag + "Nine", T["dilution"][side][-1]["n"], "{:,}")
    if "where" in pos:
        W = pos["where"]
        g = [v["gross"][0] for v in W.values()]
        M("whereMin", min(g), "{:+.3f}"); M("whereMax", max(g), "{:+.3f}"); M("whereUnits", len(W)); M("wherePositive", sum(v["gross"][1] > 0 for v in W.values()))
        M("whereDolorLater", sum(v["dolors"][0] > v["hedons"][0] for v in W.values()))
        rows = [f"{k} & {v['n_texts']:,} & {v['gross'][0]:+.3f} [{v['gross'][1]:+.3f}, {v['gross'][2]:+.3f}] & {v['hedons'][0]:+.3f} & {v['dolors'][0]:+.3f} \\\\" for k, v in W.items()]
        open(os.path.join(PAPER, "table_where.tex"), "w").write(
            "\\begin{tabular}{@{}lrccc@{}}\n\\toprule\nUnit & Texts & All charge & Hedons & Dolors \\\\\n\\midrule\n" + "\n".join(rows) + "\n\\bottomrule\n\\end{tabular}\n")
    if "kernel" in pos:
        Kn = pos["kernel"]
        for name, tag in (("EmoBank", "Emo"), ("SemEval-2018 tweets", "Tw"), ("Facebook posts", "Fb"), ("SST sentences", "Sst"), ("Film reviews", "Film"), ("Yelp reviews", "Yelp")):
            if name in Kn:
                ci("kern" + tag, Kn[name]["contrast"], "{:+.2f}")
        M("kernSupport", sum(v["contrast"][1] > 0 for v in Kn.values())); M("kernAgainst", sum(v["contrast"][2] < 0 for v in Kn.values()))
        M("kernUndecided", sum(v["contrast"][1] <= 0 <= v["contrast"][2] for v in Kn.values()))
    if "emobank" in pos:
        e = pos["emobank"]["all"]
        ci("emoSlope", e["intensity"], "{:+.3f}"); M("emoDocs", e["n_docs"], "{:,}"); M("emoSents", e["n_sentences"], "{:,}")
        M("emoFirst", e["first_sentence_intensity"], "{:.2f}"); M("emoOther", e["other_intensity"], "{:.2f}")
        M("emoCatsNeg", sum(v["intensity"][2] < 0 for k, v in pos["emobank"].items() if k != "all")); M("emoCats", len(pos["emobank"]) - 1)
        M("emoCatsPos", sum(v["intensity"][1] > 0 for k, v in pos["emobank"].items() if k != "all"))
    if "dilute" in pos:
        D = pos["dilute"]
        for name, tag in (("EmoBank", "Emo"), ("SemEval-2018 tweets", "Tw"), ("Facebook posts", "Fb"), ("SST sentences", "Sst"), ("Film reviews", "Film"), ("Yelp reviews", "Yelp")):
            ci("partial" + tag, D[name]["partial"], "{:+.2f}")
            best = int(np.argmax(D[name]["r_by_kappa"]))
            M("bestKappa" + tag, D[name]["kappa"][best], "{:g}"); M("gainKappa" + tag, D[name]["r_by_kappa"][best] - D[name]["r_by_kappa"][0], "{:.3f}")
        M("partialNeg", sum(v["partial"][2] < 0 for v in D.values())); M("partialPos", sum(v["partial"][1] > 0 for v in D.values()))

bk = R("books")
if bk:
    s = bk["summary"]
    M("nNovels", s["n_books"]); M("nNovelFiles", s["n_files"]); M("novelWords", s["words"] / 1e6, "{:.1f}"); M("novelChapters", s["chapters_median"], "{:.0f}")
    macros["mergeErr"] = "$" + sci(max(s["merge_err_max"], 1e-16)) + "$"; macros["identityErr"] = "$" + sci(max(s["identity_err_max"], 1e-18)) + "$"
    M("heteroMedian", s["hetero_ratio_median"], "{:.1f}"); M("heteroLo", s["hetero_ratio_iqr"][0], "{:.1f}"); M("heteroHi", s["hetero_ratio_iqr"][1], "{:.1f}")
    M("heteroSig", 100 * s["hetero_sig_share"], "{:.0f}")
    for d, tag in ((1, "One"), (3, "Three"), (9, "Nine")):
        M("arcShare" + tag, 100 * s["arc_r2_median"][str(d)], "{:.0f}")
    M("svdCosOne", s["svd_legendre_cosine"][0], "{:.2f}"); M("svdCosTwo", s["svd_legendre_cosine"][1], "{:.2f}"); M("svdCosThree", s["svd_legendre_cosine"][2], "{:.2f}")
    M("svdVarTwo", 100 * sum(s["svd_var_top3"][:2]), "{:.0f}")
    M("pairFlip", 100 * s["pair_flip_share"], "{:.1f}"); M("nPairs", s["n_pairs"], "{:,}"); M("gapOverSd", 100 * s["gap_over_between_book_sd"], "{:.0f}")
    v = s["vader"]
    M("vaderBooks", v["n_books"]); M("vaderSaturated", 100 * v["saturated_chapter_share"], "{:.0f}")
    M("vaderOne", v["mean_by_chunk"]["1"], "{:.2f}"); M("vaderFive", v["mean_by_chunk"]["5"], "{:.2f}"); M("vaderChapter", v["mean_by_chunk"]["chapter"], "{:.2f}")
    M("vaderFlipFive", 100 * v["pair_flip_vs_sentence"]["5"], "{:.0f}"); M("vaderFlipChapter", 100 * v["pair_flip_vs_sentence"]["chapter"], "{:.0f}")
    M("densFlipFive", 100 * v["sgw_mean_flip_vs_sentence"]["5"], "{:.0f}"); M("densFlipMax", 100 * max(v["sgw_mean_flip_vs_sentence"].values()), "{:.0f}")
    macros["sgwSpread"] = "$" + sci(max(v["sgw_merged_spread"], 1e-17)) + "$"
    M("vaderFlipLast", 100 * v["pair_flip_vs_sentence"]["125"], "{:.0f}"); M("densFlipLast", 100 * v["sgw_mean_flip_vs_sentence"]["125"], "{:.0f}")
    M("vaderRise", sum(r[-1] > r[0] for r in v["book_scores"])); M("svdVarOne", 100 * s["svd_var_top3"][0], "{:.0f}"); M("svdVarSecond", 100 * s["svd_var_top3"][1], "{:.0f}")
    first = bk["books"][:v["n_books"]]                              # the same books, in the same order, as the chunking comparison
    assert np.allclose([b["micro"] for b in first], np.array(v["sgw_merged"])[:, 0])
    GM, mac = np.array(v["sgw_mean_of_chunks"])[:, 0], np.array([b["macro"] for b in first])
    i, j = np.triu_indices(len(first), 1)
    M("densFlipChapter", 100 * np.mean(np.sign(GM[i] - GM[j]) != np.sign(mac[i] - mac[j])), "{:.0f}")

tl = R("tales")
if tl:
    M("nTales", tl["n_stories"]); M("nTaleSents", tl["n_sentences"], "{:,}")
    ci("taleSentSgw", tl["sentence_r"]["SGW"]); ci("taleSentVader", tl["sentence_r"]["VADER"])
    ci("taleStorySgw", tl["story_r"]["SGW merged"]); ci("taleStoryVader", tl["story_r"]["VADER mean of sentences"])
    M("taleArc", tl["arc_r_median"], "{:.2f}"); M("taleArcPos", 100 * tl["arc_r_positive_share"], "{:.0f}")
    p = tl["arc_r_sign_test_p"]; macros["taleArcP"] = "$" + (sci(p) if p < 0.001 else f"{p:.3f}") + "$"

cr = R("critics")
if cr:
    for p in cr["pairs"]:
        tag = "crit" + p["a"].split()[-1][:3] + p["b"].split()[-1][:3]
        for j, k in enumerate(("total", "within", "mix")):
            macros[tag + k.capitalize()] = f"{p[k]:+.1f} [{p['lo'][j]:+.1f}, {p['hi'][j]:+.1f}]"
    if "footer" in cr:
        M("footReviews", cr["footer"]["reviews"]); M("footWith", cr["footer"]["with_footer"]); M("footAdded", cr["footer"]["added"], "{:.2f}")
    M("critReversals", sum(p["reversal"] for p in cr["pairs"])); M("critPairs", len(cr["pairs"])); M("nCritReviews", sum(cr["n"].values()), "{:,}")

un = R("uncertainty")
if un:
    cov = [100 * un[c]["coverage90"] for c in CORPORA]
    M("coverMin", min(cov), "{:.0f}"); M("coverMax", max(cov), "{:.0f}")
    M("errRankMin", min(un[c]["corr_abs_err_sd"] for c in CORPORA), "{:+.2f}"); M("errRankMax", max(un[c]["corr_abs_err_sd"] for c in CORPORA), "{:+.2f}")
    M("errRankPos", sum(un[c]["corr_abs_err_sd"] > 0 for c in CORPORA))
    fall = [c for c in CORPORA if un[c]["bands"][-1]["rmse"] < un[c]["bands"][0]["rmse"]]
    M("errFall", len(fall)); M("errRise", len(CORPORA) - len(fall)); M("errFallNames", " and ".join(CN[c] if c != "yelp" else "Yelp" for c in fall))

cc = R("constants_ci")
if cc:
    for key, tag in (("dolor_over_hedon", "Dolor"), ("negated_hedon_multiplier", "NegH"), ("negated_dolor_multiplier", "NegD"), ("intensifier_gain", "Boost"),
                     ("diminisher_gain", "Damp"), ("after_contrast_gain", "Post"), ("first_minus_last_third", "Pos")):
        ci("const" + tag, cc[key], "{:.2f}" if tag == "Dolor" else "{:+.2f}")

if pos and "where" in pos:
    W = pos["where"]
    short = [W[k]["decile_density"] for k in ("EmoBank", "SemEval-2018 tweets", "SST sentences", "Facebook posts")]
    M("firstDecileLo", min(d[0] for d in short[:3]), "{:.2f}"); M("firstDecileHi", max(d[0] for d in short[:3]), "{:.2f}"); M("fbFirst", short[3][0], "{:.2f}")
    M("lastDecileLo", min(d[-1] for d in short), "{:.2f}"); M("lastDecileHi", max(d[-1] for d in short), "{:.2f}")
    M("yelpFirst", W["Yelp reviews"]["decile_density"][0], "{:.2f}"); M("yelpLast", W["Yelp reviews"]["decile_density"][-1], "{:.2f}")
if pos and "dilute" in pos:
    D = pos["dilute"]
    best = [D[k]["kappa"][int(np.argmax(D[k]["r_by_kappa"]))] for k in D if k != "Film reviews"]
    M("kappaLo", min(best), "{:g}"); M("kappaHi", max(best), "{:g}")

pt = R("parts")
if pt:
    for key, tag in (("DistilBERT (SST-2)", "Distil"), ("Twitter-RoBERTa", "Roberta")):
        M(f"parts{tag}Sign", 100 * pt[key]["sign_differs"], "{:.0f}"); M(f"parts{tag}Sent", pt[key]["r_rating_sentence_mean"], "{:.2f}")
        M(f"parts{tag}Whole", pt[key]["r_rating_whole"], "{:.2f}"); M(f"parts{tag}Saturated", 100 * pt[key]["saturated_whole"], "{:.0f}")
    M("partsSentences", pt["sentences"], "{:,}"); M("partsReviews", pt["n_reviews"]); M("partsSgwDensity", pt["SGW density r with rating"], "{:.2f}")
    if "VADER" in pt:
        M("partsVaderSign", 100 * pt["VADER"]["sign_differs"], "{:.0f}"); M("partsVaderSent", pt["VADER"]["r_rating_sentence_mean"], "{:.2f}")
        M("partsVaderWhole", pt["VADER"]["r_rating_whole"], "{:.2f}"); M("partsVaderSaturated", 100 * pt["VADER"]["saturated_whole"], "{:.0f}")
if tl:
    ci("taleStoryCal", tl["story_r"]["SGW calibrated, merged"]); M("taleSaturated", 100 * tl["vader_whole_saturated"], "{:.0f}")
    M("taleWholeVader", tl["story_r"]["VADER whole story"][0], "{:.2f}")

try:                                                              # the worked example: Moby-Dick
    import sys
    sys.path.insert(0, HERE); sys.path.insert(0, os.path.join(HERE, ".."))
    import sgw
    from exp_books import BOOKS, chapters
    ch = chapters(os.path.join(BOOKS, "2701.txt"))
    S = sgw.sketch(" ".join(s for c in ch for s in c))
    M("mobyWords", S[sgw.L], "{:,.0f}"); M("mobyHedons", S[sgw.P0], "{:,.0f}"); M("mobyDolors", S[sgw.N0], "{:,.0f}")
    M("mobyDensity", 1000 * (S[sgw.P0] - S[sgw.N0]) / S[sgw.L], "{:+.0f}"); M("mobyChapters", len(ch))
except Exception as e:
    print("Moby-Dick numbers skipped:", e)

ro = R("speed_readout")
if ro:
    M("readoutOverhead", 100 * (ro["valence_over_level_time"] - 1), "{:.0f}")
gp = R("speed_gpu")
if gp and sp:
    M("gpuName", gp["device"].replace("NVIDIA GeForce ", "")); M("distilGpu", round(gp["distilbert_fp32"], -2), "{:,.0f}"); M("distilGpuHalf", round(gp["distilbert_fp16"], -2), "{:,.0f}")
    M("robertaGpu", round(gp["roberta_fp32"], -2), "{:,.0f}"); M("robertaGpuHalf", round(gp["roberta_fp16"], -2), "{:,.0f}")
    M("speedDistilGpu", sp["sgw"] / gp["distilbert_fp16"], "{:.0f}"); M("speedDistilGpuAll", sp["sgw_all_cores"] / gp["distilbert_fp16"], "{:.0f}")
    M("billionHoursDistilGpu", 1e9 / gp["distilbert_fp16"] / 3600, "{:.0f}")
if sp:
    M("billionYearsDistil", 1e9 / sp["distilbert_1"] / 86400 / 365, "{:.1f}")
    M("tenMillionSeconds", sp["scaling"][-1][1] * 10, "{:.0f}")

if pos and "confirm" in pos:
    c = pos["confirm"]
    M("confNodes", c["n_nodes"], "{:,}"); M("confSentences", c["n_sentences"], "{:,}")
    M("confEarlier", c["R1"]["earlier"], "{:.2f}"); M("confLater", c["R1"]["later"], "{:.2f}"); ci("confWeight", c["R1"]["diff"], "{:+.3f}")
    ci("confKeptAfter", c["R2"]["kept_after"]); ci("confKeptBefore", c["R2"]["kept_before"]); ci("confKept", c["R2"]["diff"], "{:+.3f}")
    M("confReviews", c["R3"]["n_reviews"], "{:,}"); ci("confAucFirst", c["R3"]["auc_first"], "{:.3f}"); ci("confAucLast", c["R3"]["auc_last"], "{:.3f}"); ci("confAuc", c["R3"]["diff"], "{:+.3f}")
    M("confSupported", sum(c[r]["verdict"] == "supported" for r in ("R1", "R2", "R3")))

if pos and "parts" in pos:
    for name, tag in (("all", ""), ("held_out", "Held"), ("training", "Train")):
        q = pos["parts"][name]
        M("ptN" + tag, q["n"], "{:,}"); M("ptSentences" + tag, q["sentences"], "{:,}")
        ci("ptOrder" + tag, q["order_term"], "{:+.3f}")
        for st, s in (("equal", "Eq"), ("later longer", "LL"), ("earlier longer", "EL")):
            ci(f"pt{s}Diff{tag}", q["strata"][st]["diff"], "{:+.3f}"); M(f"pt{s}Earlier{tag}", q["strata"][st]["earlier"][0], "{:.2f}"); M(f"pt{s}Later{tag}", q["strata"][st]["later"][0], "{:.2f}")
            M(f"pt{s}N{tag}", q["strata"][st]["n"], "{:,}")
        ci("ptDil" + tag, q["dilution"]["matched_before_minus_after"], "{:+.3f}")
    q = pos["parts"]["all"]
    md = q["dilution"]["matched_before_minus_after"]                # printed as a positive amount "lower"
    assert md[2] < 0; M("ptDilAbs", -md[0], "{:.3f}"); M("ptDilAbsLo", -md[2], "{:.3f}"); M("ptDilAbsHi", -md[1], "{:.3f}")
    M("ptWordsEarlier", q["words_earlier"], "{:.1f}"); M("ptWordsLater", q["words_later"], "{:.1f}"); M("ptLaterLonger", 100 * q["later_longer"], "{:.0f}")
    M("ptShareLo", q["share_weight"][0][0], "{:.2f}"); M("ptShareHi", q["share_weight"][-1][0], "{:.2f}")
    M("ptRsqPlain", q["r2"]["plain_mean"], "{:.2f}"); M("ptRsqLength", q["r2"]["length_weighted"], "{:.2f}"); M("ptRsqFree", q["r2"]["free_weights"], "{:.2f}")
    M("ptDilNAfter", q["dilution"]["n_after"], "{:,}"); M("ptDilNBefore", q["dilution"]["n_before"], "{:,}")
    if "by_rated" in q["dilution"]:
        for side, s in (("after", "After"), ("before", "Before")):
            cells = q["dilution"]["by_rated"][side]["1"]             # rated part of one word, by number of neutral words
            assert cells[0]["m"] == "1" and cells[-1]["m"] == ("9+" if side == "after" else "5-8"), "text says one, nine or more, five to eight"
            ci("ptKept" + s + "First", cells[0]["kept"]); ci("ptKept" + s + "Last", cells[-1]["kept"]); M("ptKept" + s + "LastM", cells[-1]["m"].replace("-", " to ").replace("9+", "nine or more"))
            M("ptKept" + s + "LastN", cells[-1]["n"], "{:,}")
    if "rated_words_two_plus" in pos["parts"]["held_out"]["dilution"]:
        rw = pos["parts"]["held_out"]["dilution"]["rated_words_two_plus"]
        M("ptRatedAfter", rw["after"], "{:.1f}"); M("ptRatedBefore", rw["before"], "{:.1f}")
    for k, tag in enumerate(("One", "Two", "Three", "Four", "Five")):
        M("imdbFifth" + tag, pos["parts"]["imdb_auc_by_fifth"][k][0], "{:.3f}")
    M("imdbMiddleLo", min(f[0] for f in pos["parts"]["imdb_auc_by_fifth"][1:4]), "{:.3f}"); M("imdbMiddleHi", max(f[0] for f in pos["parts"]["imdb_auc_by_fifth"][1:4]), "{:.3f}")

mc = R("machine")
if mc:
    if "hc3" in mc:
        h = mc["hc3"]
        M("hcN", h["n"], "{:,}"); ci("hcAuc", h["auc"], "{:.2f}"); ci("hcAucWords", h["auc_words"], "{:.2f}")
        for m in ("net", "dolors", "clustering", "ending_lift", "negated", "intensified", "after_contrast", "hedons"):
            ci("hc" + m.title().replace("_", ""), [h["measures"][m][k] for k in ("d", "d_lo", "d_hi")], "{:+.2f}")
        M("hcSupported", sum(v == "supported" for v in h["decisions"].values()))
        M("hcWordsHuman", h["words_human"], "{:.0f}"); M("hcWordsMachine", h["words_machine"], "{:.0f}")
        if "by_source" in h:
            SRC = dict(reddit_eli5="Reddit, explain like I'm five", finance="Finance", medicine="Medicine", wiki_csai="Wikipedia, computer science", open_qa="Open questions")
            B = h["by_source"]
            M("hcSources", len(B)); M("hcSourcesAll", sum(all(v == "supported" for v in s["decisions"].values()) for s in B.values()))
            M("hcRedditShare", 100 * B["reddit_eli5"]["n"] / h["n"], "{:.0f}"); M("hcRedditClustering", B["reddit_eli5"]["d"]["clustering"], "{:+.2f}")
            M("hcMedNet", B["medicine"]["d"]["net"], "{:+.2f}"); M("hcMedDolors", B["medicine"]["d"]["dolors"], "{:+.2f}"); M("hcMedLift", B["medicine"]["d"]["ending_lift"], "{:+.2f}")
            M("hcBoostPositive", sum(s["d"]["intensified"] > 0 for s in B.values())); M("hcNegPositive", sum(s["d"]["negated"] > 0 for s in B.values()))
            mark = dict(supported="", refuted="$^{\\dagger}$", undecided="$^{\\circ}$")
            rows = []
            for s in sorted(B, key=lambda s: -B[s]["n"]):
                d, dec = B[s]["d"], B[s]["decisions"]
                rows.append(f"{SRC.get(s, s)} & {B[s]['n']:,} & " + " & ".join(f"{d[m]:+.2f}{mark[dec[hyp]]}" for m, hyp in (("net", "HL1"), ("dolors", "HL2"), ("clustering", "HL3"), ("ending_lift", "HL4")))
                            + f" & {d['negated']:+.2f} & {d['intensified']:+.2f} \\\\")
            m_ = h["measures"]
            rows.append("\\midrule\nAll sources & " + f"{h['n']:,} & " + " & ".join(f"{m_[m]['d']:+.2f}" for m in ("net", "dolors", "clustering", "ending_lift", "negated", "intensified")) + " \\\\")
            open(os.path.join(PAPER, "table_sources.tex"), "w").write(
                "\\begin{tabular}{@{}lrrrrrrr@{}}\n\\toprule\nSource & Pairs & Net (HL1) & Dolors (HL2) & Clustering (HL3) & Ending lift (HL4) & Negated & Intensified \\\\\n\\midrule\n"
                + "\n".join(rows) + "\n\\bottomrule\n\\end{tabular}\n")
    if "essays" in mc:
        e = mc["essays"]
        M("esN", e["n_sampled"])
        for c in "AB":
            x = e[c]
            M("esN" + c, x["n"]); ci("esAuc" + c, x["auc"], "{:.2f}"); ci("esAucWords" + c, x["auc_words"], "{:.2f}"); M("esHc" + c, x["auc_trained_on_hc3"], "{:.2f}")
            for m in ("net", "dolors", "clustering", "ending_lift", "negated", "intensified", "after_contrast", "hedons"):
                ci("es" + m.title().replace("_", "") + c, [x["measures"][m][k] for k in ("d", "d_lo", "d_hi")], "{:+.2f}")
            M("esLiftHuman" + c, x["measures"]["ending_lift"]["human"], "{:.0f}"); M("esLiftMachine" + c, x["measures"]["ending_lift"]["machine"], "{:.0f}")
            M("esWordsHuman" + c, x["words_human"], "{:.0f}"); M("esWordsMachine" + c, x["words_machine"], "{:.0f}")
            if "intensified_strict" in x:                           # intensifiers without so, too, such, most
                z = x["intensified_strict"]
                ci("ejStrict" + c, z["d_jackknife"], "{:+.2f}"); M("esStrictHuman" + c, z["human"], "{:.1f}"); M("esStrictMachine" + c, z["machine"], "{:.1f}")
            if "ending_exact" in x:                                 # the literal last fifth, as a check on the registered measure
                ci("ejExact" + c, x["ending_exact"]["d_jackknife"], "{:+.2f}"); M("esExactHuman" + c, x["ending_exact"]["human"], "{:.0f}")
                M("esExactMachine" + c, x["ending_exact"]["machine"], "{:.0f}"); M("esExactCorr" + c, x["ending_exact"]["corr_with_registered"], "{:.2f}")
            for k, tag in (("auc_jackknife", "esAuc"), ("auc_words_jackknife", "esAucWords")):
                if k in x:
                    macros[tag + c + "CIjack"] = f"[{x[k][1]:.2f}, {x[k][2]:.2f}]"
            for m in x.get("d_jackknife", {}):                      # delete-one-assignment jackknife, t reference: the intervals printed in the text
                ci("ej" + m.title().replace("_", "") + c, x["d_jackknife"][m], "{:+.2f}")
            for hyp, v in x.get("decisions_jackknife", {}).items():
                M("ejDec" + hyp.replace("HL", "").replace("1", "one").replace("2", "two").replace("3", "three").replace("4", "four") + c, v)
            for hyp, v in x["decisions"].items():
                M("es" + hyp.replace("1", "one").replace("2", "two").replace("3", "three").replace("4", "four") + c, v)
    if "revise" in mc:
        r = mc["revise"]
        M("rvN", r["n"]); M("rvDraft", r["distance"]["draft"], "{:.2f}"); M("rvControl", r["distance"]["control"], "{:.2f}"); M("rvReport", r["distance"]["report"], "{:.2f}"); M("rvHuman", r["distance"]["human"], "{:.2f}")
        for k, tag in (("report_minus_control", "RvRC"), ("control_minus_draft", "RvCD"), ("report_minus_draft", "RvRD")):
            M("d" + tag, r[k]["diff"], "{:+.2f}"); macros["d" + tag + "CI"] = f"[{r[k]['ci_prompts'][0]:+.2f}, {r[k]['ci_prompts'][1]:+.2f}]"
            macros["d" + tag + "CIessays"] = f"[{r[k]['ci_essays'][0]:+.2f}, {r[k]['ci_essays'][1]:+.2f}]"
            macros["d" + tag + "CIjack"] = f"[{r[k]['ci_jackknife'][0]:+.2f}, {r[k]['ci_jackknife'][1]:+.2f}]"
        M("rvVerdict", r["HT1"])
        for arm in ("draft", "control", "report", "human"):
            M("rvScore" + arm.title(), r["machine_score"][arm], "{:.2f}")
        for arm in ["draft"] + r.get("arms", ["control", "report"]):
            M("rvLift" + arm.title(), r["per_measure_z"][arm]["ending_lift"], "{:+.2f}"); M("rvBoost" + arm.title(), r["per_measure_z"][arm]["intensified"], "{:+.2f}")
            M("rvNeg" + arm.title(), r["per_measure_z"][arm]["negated"], "{:+.2f}")
        M("rvDeclined", sum(r.get("declined", {}).values())); M("rvAssignments", r.get("n_assignments", "?"))
        rev = [r["machine_score"][a_] for a_ in r.get("arms", ["control", "report"])]
        M("rvScoreLo", min(rev), "{:.2f}"); M("rvScoreHi", max(rev), "{:.2f}")
        for arm, tag in (("humanizer", "H"), ("sepia", "S")):
            if arm in r.get("arms", []):
                M("rv" + arm.title(), r["distance"][arm], "{:.2f}"); M("rvScore" + arm.title(), r["machine_score"][arm], "{:.2f}"); M("rvVerdict" + arm.title(), r["HS1"][arm])
                for k, t in ((f"{arm}_minus_control", "C"), (f"{arm}_minus_draft", "D"), (f"report_minus_{arm}", "R")):
                    M("dRv" + tag + t, r[k]["diff"], "{:+.2f}"); macros["dRv" + tag + t + "CI"] = f"[{r[k]['ci_prompts'][0]:+.2f}, {r[k]['ci_prompts'][1]:+.2f}]"
                    macros["dRv" + tag + t + "CIessays"] = f"[{r[k]['ci_essays'][0]:+.2f}, {r[k]['ci_essays'][1]:+.2f}]"
                    macros["dRv" + tag + t + "CIjack"] = f"[{r[k]['ci_jackknife'][0]:+.2f}, {r[k]['ci_jackknife'][1]:+.2f}]"

def tex(x):
    for a, b in (("\\", " "), ("&", "\\&"), ("%", "\\%"), ("$", "\\$"), ("#", "\\#"), ("_", "\\_"), ("{", ""), ("}", ""), ("~", " "), ("^", " "), ("\u2019", "'"), ("\u201c", "``"), ("\u201d", "''"), ("\u2014", ", ")):
        x = x.replace(a, b)
    return x


if mc and "dial" in mc:
    d = mc["dial"]
    M("dlN", d["n"]); M("dlDeclined", d["declined"]); M("dlMissing", d["missing_dial"]["total"]); M("dlMissingHaiku", d["missing_dial"]["haiku"]); M("dlRequested", d["missing_dial"]["requested"])
    for a in ("dial", "control", "draft"):
        M("dlCorr" + a.title(), d["mean_corr"][a], "{:.2f}")
    for k, tag in (("corr_dial_minus_control", "dDlCorr"), ("gap_dial_minus_control", "dDlGap"), ("score_dial_minus_control", "dDlScore")):
        M(tag, d[k]["diff"], "{:+.2f}"); macros[tag + "CI"] = f"[{d[k]['ci_prompts'][0]:+.2f}, {d[k]['ci_prompts'][1]:+.2f}]"
        macros[tag + "CIessays"] = f"[{d[k]['ci_essays'][0]:+.2f}, {d[k]['ci_essays'][1]:+.2f}]"
        macros[tag + "CIjack"] = f"[{d[k]['ci_jackknife'][0]:+.2f}, {d[k]['ci_jackknife'][1]:+.2f}]"
    M("dlModels", len(d["mean_corr_by_model"])); M("dlCorrModelLo", min(v["dial"] for v in d["mean_corr_by_model"].values()), "{:.2f}"); M("dlCorrModelHi", max(v["dial"] for v in d["mean_corr_by_model"].values()), "{:.2f}")
    if "rank_corr" in d:
        rk = d["rank_corr"]["dial"]
        M("dlRhoDial", np.mean(list(rk.values())), "{:.2f}"); M("dlRhoControl", np.mean(list(d["rank_corr"]["control"].values())), "{:.2f}")
        best = max(rk, key=rk.get)
        assert best == "dolors" and min(rk, key=rk.get) == "clustering", "text names the best and the worst followed measure"
        M("dlRhoMax", rk[best], "{:.2f}"); M("dlRhoMin", min(rk.values()), "{:.2f}")
        M("dlClusters", ", ".join(str(v) for v in d["cluster_sizes"]))
        for arm in ("draft", "human", "dial", "control"):
            M("dlSlips" + arm.title(), d["slips_per_1000"][arm], "{:.1f}")
    M("dlAssignments", d.get("n_assignments", "?")); M("dlTrain", d["classifier_training_essays"]); M("dlMissingControl", d["missing_dial"]["missing_control"])
    for a, t in (("draft", "Draft"), ("control_r1", "ControlOne"), ("dial_r1", "DialOne"), ("control", "Control"), ("dial", "Dial")):
        M("dlGap" + t, d["gap"][a], "{:.2f}")
    for a in ("draft", "control", "dial", "human"):
        M("dlScore" + a.title(), d["machine_score"][a], "{:.2f}"); M("dlCentre" + a.title(), d["distance_to_centre"][a], "{:.2f}")
    for a in ("draft", "control", "dial"):
        M("dlAuc" + a.title(), d["auc_vs_matched_students"][a], "{:.2f}")
    M("dlHDone", d["HD1"]); M("dlHDtwo", d["HD2"])
    for m, t in (("intensified", "Boost"), ("negated", "Neg"), ("ending_lift", "Lift"), ("hedons", "Hed"), ("dolors", "Dol"), ("clustering", "Clu")):
        M("dlCorr" + t + "Dial", d["corr"]["dial"][m], "{:.2f}"); M("dlCorr" + t + "Control", d["corr"]["control"][m], "{:.2f}")
    M("dlCorrMin", min(d["corr"]["dial"].values()), "{:.2f}"); M("dlCorrMax", max(d["corr"]["dial"].values()), "{:.2f}")
    if "judge_better" in d:
        for k, t in (("judge_better", "dlJudge"), ("judge_student", "dlJudgeStudent")):
            M(t, 100 * d[k]["share"], "{:.0f}"); macros[t + "CI"] = f"[{100 * d[k]['ci_prompts'][0]:.0f}, {100 * d[k]['ci_prompts'][1]:.0f}]"
            macros[t + "Exact"] = f"[{100 * d[k]['ci_exact'][0]:.0f}, {100 * d[k]['ci_exact'][1]:.0f}]"; M(t + "Wins", d[k]["wins"])
        M("dlJudgeN", d["judge_better"]["n"]); M("dlHDthree", d["HD3"]); M("dlJudgeBoth", d["judge_both"])
        for tier, v in d["judge_by_model"].items():
            macros["dlJudge" + tier.title()] = f"{v['better']} of {v['n']}"; macros["dlJudgeStudent" + tier.title()] = f"{v['student']} of {v['n']}"
    e = d["excerpt"]
    v, tg = e["values"], e["target"]
    row = lambda name, key, f="{:.1f}": f"{name} & {f.format(v['draft'][key])} & {f.format(tg[key])} & {f.format(v['dial'][key])} & {f.format(v['control'][key])} \\\\"
    open(os.path.join(PAPER, "excerpt.tex"), "w").write(
        "\\begin{tabular}{@{}p{0.31\\textwidth}p{0.31\\textwidth}p{0.31\\textwidth}@{}}\n\\toprule\nAI first draft & After two rounds with the dial & After two plain revisions \\\\\n\\midrule\n"
        + " & ".join("\\emph{Opening.} " + tex(e[a]["opening"]) + " \\ldots" for a in ("draft", "dial", "control")) + " \\\\[4pt]\n"
        + " & ".join("\\emph{Closing.} \\ldots " + tex(e[a]["closing"]) for a in ("draft", "dial", "control")) + " \\\\\n\\bottomrule\n\\end{tabular}\n\n\\vspace{6pt}\n"
        + "\\begin{tabular}{@{}lrrrr@{}}\n\\toprule\nMeasure & Draft & Target & Dial & Plain \\\\\n\\midrule\n"
        + "\n".join([row("Hedons per 1,000 words", "hedons"), row("Dolors per 1,000 words", "dolors"), row("Charge negated (\\%)", "negated"),
                     row("Charge intensified (\\%)", "intensified"), row("Ending lift (per 1,000 words)", "ending_lift")]) + "\n\\bottomrule\n\\end{tabular}\n")
    M("exTier", e["tier"].title()); M("exPrompt", tex(e["prompt"]))

try:                                                              # words that carry charge in the scorer (operator words are given none), and the level's own constants
    macros["nChargedScorer"] = f"{int((sgw.CHARGE != 0).sum()):,}"
    k_ = json.load(open(os.path.join(HERE, "..", "sgw", "constants.json")))
    M("shipLevelA", k_["level_a"], "{:g}"); M("shipLevelKappa", k_["level_kappa"], "{:g}")
except Exception as e:
    print("scorer numbers skipped:", e)
if pos and "dilute" in pos:
    gains = [float(macros["gainKappa" + tag]) for tag in ("Emo", "Tw", "Fb", "Sst", "Yelp")]
    M("gainKappaLo", min(gains), "{:.3f}"); M("gainKappaHi", max(gains), "{:.3f}")
if pos and "parts" in pos:
    M("ptLaterLongerHeld", 100 * pos["parts"]["held_out"]["later_longer"], "{:.0f}")

dcl = os.path.join(HERE, "..", "machine_text", "declined_requests.json")
if os.path.exists(dcl):                                           # requests that left no text, with each agent's final message
    D_ = json.load(open(dcl))
    why = lambda o: "auth" if re.search(r"pass|misrepresent|disguis|detector|detect", o["final_message"].lower()) else "relay" if re.search(r"vast|request", o["final_message"].lower()) else "silent"
    M("dcTotal", len(D_)); M("dcAuthorship", sum(why(o) == "auth" for o in D_)); M("dcRelay", sum(why(o) == "relay" for o in D_)); M("dcSilent", sum(why(o) == "silent" for o in D_))
    M("dcDialFirst", sum(o["experiment"] == "dial, round 1" and o["arm"] == "dial" for o in D_)); M("dcDialSecond", sum(o["experiment"] == "dial, round 2" and o["arm"] == "dial" for o in D_))
    assert all(o["model"] == "haiku" for o in D_ if why(o) == "auth") and all(o["experiment"] == "essays" for o in D_ if why(o) == "relay"), "text says who declined and why"
    M("dcHaikuControl", len(glob.glob(os.path.join(HERE, "..", "machine_text", "dial", "r?", "control", "*_haiku.txt"))))
    M("dcHaikuSkill", len(glob.glob(os.path.join(HERE, "..", "machine_text", "revisions", "humanizer", "*_haiku.txt"))) + len(glob.glob(os.path.join(HERE, "..", "machine_text", "revisions", "sepia", "*_haiku.txt"))))
try:                                                              # how much of the charged lexicon comes from VADER's list (needs the downloaded lists)
    vad = {l.split("\t")[0] for l in open(os.path.join(HERE, "..", "data", "lex", "vader_lexicon.txt"), encoding="utf-8")}
    lab = {l.split("\t")[0] for l in open(os.path.join(HERE, "..", "data", "lex", "labMT_English.tsv"), encoding="utf-8")}
    chg = [w for w, v in words.items() if abs(int(v)) >= 250]
    M("lexFromVader", 100 * sum(w in vad for w in chg) / len(chg), "{:.0f}"); M("lexVaderOnly", 100 * sum(w in vad and w not in lab for w in chg) / len(chg), "{:.0f}")
except Exception as e:
    print("lexicon source shares skipped:", e)

if not sp:
    macros["speedParagraph"] = "\\textcolor{red}{Throughput benchmark pending.}"

if acc and cr and mc and "dial" in mc:
    assert [c for c in CORPORA if acc[c]["methods"]["SGW, no dilution"]["sgw_minus_ci"][0] > 0] == ["semeval", "fb", "sst", "yelp"], "text names the corpora where the dilution test is supported"
    assert macros["critReversals"] == "0" and macros["dlJudgeBoth"] == "0" and macros["dlMissing"] == macros["dlMissingHaiku"], "text says none / never / all"
    assert macros["winsVader"] == "3" and macros["lossesVader"] == "0" and macros["winsNoShrink"] == "2", "text says three, none, two"
    assert acc["fb"]["methods"]["VADER, mean of sentences"]["sgw_minus_ci"][0] > 0 and acc["yelp"]["methods"]["VADER, mean of sentences"]["sgw_minus_ci"][1] < 0
    assert macros["winsVaderSent"] == "1" and macros["lossesVaderSent"] == "1", "text names Facebook as the one lead and Yelp as the one deficit against sentence-averaged VADER"
    assert max(v["gross"][0] for k, v in pos["where"].items() if k in ("Film reviews", "Yelp reviews", "Film review paragraphs", "Novel chapters")) < 0.03, "text says at most +0.03 for long units"
    assert macros["lossesNoShrink"] == "1" and acc["scale"]["methods"]["SGW, no shrinkage"]["sgw_minus_ci"][1] < 0, "text says the neutral prior costs a little in film reviews"

WORDS = "zero one two three four five six seven eight nine ten".split()
COUNTS = """whereUnits whereDolorLater wherePositive partialNeg partialPos winsNoDil winsVader lossesVader winsVaderSent lossesVaderSent winsVaderPrimary
    winsDistil winsNoShrink lossesNoShrink errFall errRise errRankPos hcSourcesAll hcSources hcBoostPositive hcNegPositive hcSupported critPairs
    kernSupport kernAgainst kernUndecided dlModels dlAssignments rvAssignments rvDeclined dcRelay dcSilent dcDialFirst dcDialSecond emoCatsNeg emoCats
    emoCatsPos grpUp dlMissingControl""".split()
for k in COUNTS:                                                  # small counts in running text are set as words
    if k in macros and macros[k].isdigit() and 1 <= int(macros[k]) <= 10:
        macros[k] = WORDS[int(macros[k])]
minus = lambda s: re.sub(r"(?<![A-Za-z0-9}\\])-(?=[\d.])", lambda m: "\\ensuremath{-}", s)   # a hyphen before a number becomes a minus sign
with open(os.path.join(PAPER, "numbers.tex"), "w") as f:
    f.write("% Generated by scripts/make_numbers.py. Do not edit.\n")
    f.write("\\providecommand{\\num}[1]{\\textcolor{red}{??}}\n")
    for k in sorted(macros):
        assert re.fullmatch(r"[A-Za-z]+", k), f"macro name {k!r} must be letters only"
        f.write(f"\\newcommand{{\\{k}}}{{{minus(macros[k])}}}\n")
for name in glob.glob(os.path.join(PAPER, "table_*.tex")) + [os.path.join(PAPER, "excerpt.tex")]:
    body = open(name).read()
    open(name, "w").write(minus(body))
# ---- the "Results in brief" block of the README
g = lambda k: re.sub(r"(\d)\\times10\^\{(-?\d+)\}", r"\1e\2", macros.get(k, "?").replace("\\%", "%").replace("$", ""))
block = f"""| | |
|---|---|
| Mean Pearson r with human ratings (five rated corpora, constants fitted on other corpora) | signature {g('meanSgw')}; VADER {g('meanVader')} on whole texts and {g('meanVaderSent')} averaged over sentences; TextBlob {g('meanBlob')}; hedonometer {g('meanHedo')}; DistilBERT {g('meanDistil')}; Twitter-RoBERTa {g('meanRoberta')} |
| Sentences per second on one processor core | signature {g('sgwRate')}; VADER {g('vaderRate')}; DistilBERT {g('distilRate')}; DistilBERT on an RTX 3090 {g('distilGpuHalf')} |
| One billion sentences | {g('billionHours')} hours on one core, {g('billionMinutesAll')} minutes with twelve processes on six cores |
| Merged against single-pass signatures, {g('nNovels')} novels | relative difference below {g('mergeErr')} in floating point; exact in integers |
| Chapters of novels | between-chapter variance {g('heteroMedian')} times the shuffled-sentence value (median) |
| VADER book score by chunk size | {g('vaderOne')} with one-sentence chunks, {g('vaderChapter')} with whole chapters |
| Order of two parts of a phrase, length held fixed | weight of the later part minus the earlier at equal length {g('ptEqDiff')} {g('ptEqDiffCI')}; the raw later-part advantage is a length effect |
| Neutral words | dilute on either side of the feeling; planned test supported in {g('winsNoDil')} of six corpora |
| Human against machine essays, ten tone measures | AUC {g('esAucA')} plain, {g('esAucB')} when the model writes as a student; {g('hcAuc')} for ChatGPT answers |
| The dial | target against achieved value, mean r {g('dlCorrDial')} with the dial and {g('dlCorrControl')} without; no gain in resemblance to student essays over a plain revision was detected |"""
readme = os.path.join(HERE, "..", "README.md")
t = open(readme).read()
a, b = t.index("<!-- results:start -->") + len("<!-- results:start -->"), t.index("<!-- results:end -->")
open(readme, "w").write(t[:a] + "\n" + block + "\n" + t[b:])
print(len(macros), "macros written")
