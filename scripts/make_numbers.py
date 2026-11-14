"""Write paper/numbers.tex and the generated tables from results/*.json.

Every number in the manuscript is a macro defined here, so the text cannot drift from the results.
A result file that is missing leaves its macros as red '??' rather than stopping the build.
"""
import glob, json, os

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
    e = int(np.floor(np.log10(x)))
    return f"{x / 10 ** e:.0f}\\times10^{{{e}}}"


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
    names = {"SGW": "Sgw", "SGW level": "Level", "SGW, refined charges": "Refined", "VADER": "Vader", "Hedonometer": "Hedo", "TextBlob": "Blob",
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
    M("locoA", "/".join(sorted({f"{acc[c]['loco']['a']:g}" for c in CORPORA}))); M("locoKappa", "/".join(sorted({f"{acc[c]['loco']['kappa']:g}" for c in CORPORA})))
    M("shipA", acc["shipped"]["a"], "{:g}"); M("shipKappa", acc["shipped"]["kappa"], "{:g}")
    for c in CORPORA:
        M("n" + TAG[c], acc[c]["n"], "{:,}")
    # the fitted composition constants, shipped model (all corpora)
    b = acc["shipped"]["coef"]
    M("negHedon", b[6] / b[1], "{:.2f}"); M("negDolor", b[7] / b[2], "{:.2f}"); M("boostRatio", b[8] / ((b[1] - b[2]) / 2), "{:.2f}")
    M("dampRatio", b[9] / ((b[1] - b[2]) / 2), "{:.2f}"); M("postRatio", b[10] / ((b[1] - b[2]) / 2), "{:.2f}"); M("dolorOverHedon", -b[2] / b[1], "{:.2f}")
    # table of every method on every corpus
    order = ["SGW", "SGW level", "SGW, in-corpus constants", "SGW, refined charges", "VADER", "Hedonometer", "TextBlob",
             "Bag-of-words ridge (other corpora)", "TF-IDF ridge (in-corpus)", "DistilBERT (SST-2)", "Twitter-RoBERTa"]
    label = {"SGW": "Signature, calibrated", "SGW level": "Signature, level only", "SGW, in-corpus constants": "Signature, constants from the same corpus",
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

ref = R("refine")
if ref:
    M("refWords", ref["shipped"]["n_words"], "{:,}"); M("refNew", ref["shipped"]["n_new"], "{:,}")

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
    M("wordsPerSentence", sp["words_per_sentence"], "{:.0f}")
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
    M("critReversals", sum(p["reversal"] for p in cr["pairs"])); M("critPairs", len(cr["pairs"])); M("nCritReviews", sum(cr["n"].values()), "{:,}")

un = R("uncertainty")
if un:
    cov = [100 * un[c]["coverage90"] for c in CORPORA]
    M("coverMin", min(cov), "{:.0f}"); M("coverMax", max(cov), "{:.0f}")
    M("errRankMin", min(un[c]["corr_abs_err_sd"] for c in CORPORA), "{:+.2f}"); M("errRankMax", max(un[c]["corr_abs_err_sd"] for c in CORPORA), "{:+.2f}")
    M("errRankPos", sum(un[c]["corr_abs_err_sd"] > 0 for c in CORPORA))

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
    M("partsSentences", pt["sentences"], "{:,}")
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

mc = R("machine")
if mc:
    if "hc3" in mc:
        h = mc["hc3"]
        M("hcN", h["n"], "{:,}"); ci("hcAuc", h["auc"], "{:.2f}"); ci("hcAucWords", h["auc_words"], "{:.2f}")
        for m in ("net", "dolors", "clustering", "ending_lift", "negated", "intensified", "after_contrast", "hedons"):
            ci("hc" + m.title().replace("_", ""), [h["measures"][m][k] for k in ("d", "d_lo", "d_hi")], "{:+.2f}")
        M("hcSupported", sum(v == "supported" for v in h["decisions"].values()))
    if "essays" in mc:
        e = mc["essays"]
        M("esN", e["n_sampled"])
        for c in "AB":
            x = e[c]
            M("esN" + c, x["n"]); ci("esAuc" + c, x["auc"], "{:.2f}"); ci("esAucWords" + c, x["auc_words"], "{:.2f}"); M("esHc" + c, x["auc_trained_on_hc3"], "{:.2f}")
            for m in ("net", "dolors", "clustering", "ending_lift", "negated", "intensified", "after_contrast", "hedons"):
                ci("es" + m.title().replace("_", "") + c, [x["measures"][m][k] for k in ("d", "d_lo", "d_hi")], "{:+.2f}")
            M("esLiftHuman" + c, x["measures"]["ending_lift"]["human"], "{:.0f}"); M("esLiftMachine" + c, x["measures"]["ending_lift"]["machine"], "{:.0f}")
            for hyp, v in x["decisions"].items():
                M("es" + hyp + c, v)
    if "revise" in mc:
        r = mc["revise"]
        M("rvN", r["n"]); M("rvDraft", r["distance"]["draft"], "{:.2f}"); M("rvControl", r["distance"]["control"], "{:.2f}"); M("rvReport", r["distance"]["report"], "{:.2f}"); M("rvHuman", r["distance"]["human"], "{:.2f}")
        for k, tag in (("report_minus_control", "RvRC"), ("control_minus_draft", "RvCD"), ("report_minus_draft", "RvRD")):
            M("d" + tag, r[k]["diff"], "{:+.2f}"); macros["d" + tag + "CI"] = f"[{r[k]['ci_prompts'][0]:+.2f}, {r[k]['ci_prompts'][1]:+.2f}]"
            macros["d" + tag + "CIessays"] = f"[{r[k]['ci_essays'][0]:+.2f}, {r[k]['ci_essays'][1]:+.2f}]"
        M("rvVerdict", r["HT1"])
        for arm in ("draft", "control", "report", "human"):
            M("rvScore" + arm.title(), r["machine_score"][arm], "{:.2f}")
        for arm in ("draft", "control", "report"):
            M("rvLift" + arm.title(), r["per_measure_z"][arm]["ending_lift"], "{:+.2f}"); M("rvBoost" + arm.title(), r["per_measure_z"][arm]["intensified"], "{:+.2f}")

if not sp:
    macros["speedParagraph"] = "\\textcolor{red}{Throughput benchmark pending.}"

with open(os.path.join(PAPER, "numbers.tex"), "w") as f:
    f.write("% Generated by scripts/make_numbers.py. Do not edit.\n")
    f.write("\\providecommand{\\num}[1]{\\textcolor{red}{??}}\n")
    for k in sorted(macros):
        f.write(f"\\newcommand{{\\{k}}}{{{macros[k]}}}\n")
print(len(macros), "macros written")
