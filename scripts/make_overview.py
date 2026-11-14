"""Overview diagram (Fig. 1), drawn with the FigCraft command-line tool.

    python3 scripts/make_overview.py

Writes paper/figures/fig_overview.json (the FigCraft command script), then runs `figcraft-cli` on it to
produce fig_overview.figcraft (editable in FigCraft), fig_overview.pdf (vector, for the paper) and
fig_overview.png. Word colours come from the shipped lexicon, so the diagram cannot disagree with the scorer.
Needs `figcraft-cli` on the PATH and the Nimbus Sans font (FigCraft reports a missing font and falls back).
"""
import json, os, subprocess, sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, ".."))
import sgw

OUT = os.path.join(HERE, "..", "paper", "figures")
W, H = 1016, 500                                                  # drawn at two units per printed point
INK, MUTED, RULE = "#222222", "#5C6370", "#C5CAD1"
BLUE, RED, GREY = "#0072B2", "#D55E00", "#8A8A8A"
BLUE_T, RED_T, GREY_T, SLATE_T = "#DCEAF4", "#FADFCF", "#EEF0F2", "#DDE3E9"
FONT = "Nimbus Sans"

cmds = [{"command": "file.new", "params": {"name": "Overview"}}]


def add(command, **params):
    cmds.append({"command": command, "params": params})
    return {"$step": len(cmds) - 1}


ROOT = add("node.create", type="frame", x=0, y=0, width=W, height=H, name="Overview", props={"fill": "#FFFFFF"})


def rect(x, y, w, h, parent=None, name=None, **props):
    return add("node.create", type="rectangle", parent=parent or ROOT, x=x, y=y, width=w, height=h, name=name or "shape", props=props)


def text(s, x, y, w=None, size=13, style="Regular", color=INK, align="left", lh=None, parent=None):
    props = {"fontFamily": FONT, "fontStyle": style, "fontSize": size, "fill": color, "textAlign": align}
    if lh:
        props["lineHeight"] = lh
    p = dict(type="text", parent=parent or ROOT, x=x, y=y, text=s, name=s[:40], props=props)
    if w:
        p["width"] = w
        props["textAutoResize"] = "height"
    return add("node.create", **p)


def path(d, color=INK, weight=1.25, end=None, start=None, dashes=None):
    props = {"stroke": color, "strokeWeight": weight, "fills": [], "strokeCap": "round"}
    if end:
        props["endCap"] = end
    if start:
        props["startCap"] = start
    if dashes:
        props["dashes"] = dashes
    return add("node.create", type="vector", parent=ROOT, path=d, name="connector", props=props)


def chip(parent, word, kind):
    look = {"hedon": dict(fill=BLUE), "dolor": dict(fill=RED), "buffer": dict(fill=GREY_T), "operator": dict(fills=[], stroke=INK, strokeWeight=1)}[kind]
    ink = {"hedon": "#FFFFFF", "dolor": "#FFFFFF", "buffer": MUTED, "operator": INK}[kind]
    f = add("node.create", type="frame", parent=parent, name=word,
            props=dict(layout="horizontal", padding=[3, 7, 3, 7], cornerRadius=5, sizingH="hug", sizingV="hug", **look))
    text(word, 0, 0, size=13, style="Bold" if kind in ("hedon", "dolor") else "Italic" if kind == "operator" else "Regular", color=ink, parent=f)


def card(x, y, w, h, number, title):
    rect(x, y, w, h, name=title, fill="#FFFFFF", stroke=RULE, strokeWeight=1, cornerRadius=10)
    add("node.create", type="ellipse", parent=ROOT, x=x + 12, y=y + 12, width=20, height=20, name="badge", props={"fill": INK})
    text(str(number), x + 12, y + 14, w=20, size=12, style="Bold", color="#FFFFFF", align="center")
    text(title, x + 40, y + 13, size=15, style="Bold")


def kind_of(word):
    i = sgw.VOCAB.get(word.lower(), 0)
    if sgw.KIND[i] & (sgw.NEG | sgw.BOOST | sgw.DAMP | sgw.CONTR):
        return "operator"
    c = int(sgw.CHARGE[i])
    return "hedon" if c > 0 else "dolor" if c < 0 else "buffer"


CX, CY, CW, CH, GAP = 10, 10, 228, 296, 28                         # four cards across the top
xs = [CX + k * (CW + GAP) for k in range(4)]

# ---- 1. words carry charge
x = xs[0]
card(x, CY, CW, CH, 1, "Words carry charge")
row = add("node.create", type="frame", parent=ROOT, x=x + 12, y=CY + 46, name="example",
          props=dict(layout="horizontal", gap=5, crossGap=5, wrap=True, sizingH="fixed", sizingV="hug", width=CW - 24, fills=[]))
example = "The service was terrible but the dessert was very good".split()
for word in example:
    chip(row, word, kind_of(word))
legend = [("hedon", "hedons: positive charge"), ("dolor", "dolors: negative charge"), ("buffer", "buffer word: no charge"), ("operator", "operator: not, very, slightly, but")]
for k, (kind, label) in enumerate(legend):
    yy = CY + 150 + k * 22
    sw = dict(fill=BLUE) if kind == "hedon" else dict(fill=RED) if kind == "dolor" else dict(fill=GREY_T) if kind == "buffer" else dict(fills=[], stroke=INK, strokeWeight=1)
    rect(x + 14, yy + 2, 12, 12, name="swatch", cornerRadius=3, **sw)
    text(label, x + 34, yy, size=13)
n_words = json.load(open(os.path.join(HERE, "..", "results", "lexicon.json")))["open"]["n"]
text(f"Charges come from human ratings of {n_words:,} words (labMT, VADER).", x + 14, CY + 242, w=CW - 28, size=12, color=MUTED, lh=15)

# ---- 2. signature of a span
x = xs[1]
card(x, CY, CW, CH, 2, "Signature of a span")
text("19 numbers, whatever the length", x + 14, CY + 40, size=12, color=MUTED)
groups = [([SLATE_T] * 2, "words; charged words"),
          ([BLUE] * 4 + [RED] * 4, "hedons and dolors weighted by word position, degree 0 to 3"),
          ([BLUE_T] * 4 + [RED_T] * 4, "hedons and dolors under a negation, an intensifier, a diminisher and after a contrast word"),
          ([INK], "sum of squared charges")]
yy = CY + 66
for cells, label in groups:
    for k, col in enumerate(cells):
        rect(x + 14 + k * 15, yy, 12, 12, name="entry", fill=col, cornerRadius=2)
    lines = 1 if len(label) < 32 else 2 if len(label) < 62 else 3
    text(label, x + 14, yy + 17, w=CW - 28, size=12.5, lh=15)
    yy += 17 + 15 * lines + 11

# ---- 3. exact merge
x = xs[2]
card(x, CY, CW, CH, 3, "Exact merge")
bw, bh = 66, 28
for k, (lab, bx) in enumerate((("σ(A)", x + 22), ("σ(B)", x + CW - 22 - bw))):
    rect(bx, CY + 48, bw, bh, name=lab, fill=SLATE_T, cornerRadius=6)
    text(lab, bx, CY + 54, w=bw, size=14, align="center")
text("⊕", x + CW / 2 - 12, CY + 51, w=24, size=18, align="center")
path(f"M {x + CW / 2} {CY + 82} L {x + CW / 2} {CY + 100}", end="triangleArrow")
rect(x + CW / 2 - 42, CY + 106, 84, bh, name="merged", fill=SLATE_T, stroke=INK, strokeWeight=1, cornerRadius=6)
text("σ(AB)", x + CW / 2 - 42, CY + 112, w=84, size=14, align="center")
text("Counts and totals add. Position sums of B shift by the length of A.", x + 14, CY + 148, w=CW - 28, size=13, lh=16)
tags = add("node.create", type="frame", parent=ROOT, x=x + 14, y=CY + 194, name="properties",
           props=dict(layout="horizontal", gap=5, crossGap=5, wrap=True, sizingH="fixed", sizingV="hug", width=CW - 28, fills=[]))
for word in ("associative", "invertible", "exact in integers"):
    f = add("node.create", type="frame", parent=tags, name=word, props=dict(layout="horizontal", padding=[3, 7, 3, 7], cornerRadius=5, sizingH="hug", sizingV="hug", fill=GREY_T))
    text(word, 0, 0, size=12.5, parent=f)
text("Holds wherever the cut falls between sentences.", x + 14, CY + 256, w=CW - 28, size=12, color=MUTED, lh=15)

# ---- 4. readouts
x = xs[3]
card(x, CY, CW, CH, 4, "Readouts")
readouts = [("totals", "hedons, dolors and words", 1), ("density", "net charge per 1,000 words", 1),
            ("level, with a standard error", "net charge over exposure: a neutral prior, charged words and down-weighted buffer words", 3),
            ("calibrated valence", "a predicted human rating, fitted to rated text", 2), ("arc", "cubic of net charge over relative position", 2)]
yy = CY + 44
for name, what, lines in readouts:
    text(name, x + 14, yy, size=13, style="Bold")
    text(what, x + 14, yy + 16, w=CW - 28, size=12.5, color=MUTED, lh=15)
    yy += 16 + 15 * lines + 6
for k in range(3):                                                 # arrows between the cards
    ax = xs[k] + CW
    add("node.create", type="arrow", parent=ROOT, x=ax + 5, y=CY + CH / 2, width=GAP - 10, name="next", props={"stroke": INK, "strokeWeight": 1.5})

# ---- one pass, every scale
BY, BH = CY + CH + 16, H - (CY + CH + 16) - 10
rect(CX, BY, W - 2 * CX, BH, name="scales", fill="#FFFFFF", stroke=RULE, strokeWeight=1, cornerRadius=10)
text("One pass, every scale", CX + 14, BY + 12, size=15, style="Bold")
LX, RX0, RX1 = CX + 14, 190, 800                                   # label column; the hierarchy spans RX0..RX1
chapters, per = 4, 7
n = chapters * per
gap = 3
cw = (RX1 - RX0 - (n - 1) * gap) / n
y_s, y_c, y_b = BY + 44, BY + 88, BY + 134
for k in range(n):
    rect(RX0 + k * (cw + gap), y_s, cw, 16, name="sentence", fill=GREY_T, cornerRadius=3)
for c in range(chapters):
    a = RX0 + c * per * (cw + gap)
    b = a + per * cw + (per - 1) * gap
    rect(a, y_c, b - a, 22, name=f"chapter {c + 1}", fill=SLATE_T, cornerRadius=4)
    text(f"σ(chapter {c + 1})", a, y_c + 4, w=b - a, size=12.5, align="center")
    path(f"M {a + 2} {y_s + 22} L {b - 2} {y_s + 22}", color=GREY, weight=1)
    path(f"M {(a + b) / 2} {y_s + 22} L {(a + b) / 2} {y_c - 4}", color=GREY, weight=1, end="triangleArrow")
rect(RX0, y_b, RX1 - RX0, 24, name="book", fill=SLATE_T, stroke=INK, strokeWeight=1, cornerRadius=4)
text("σ(book)", RX0, y_b + 5, w=RX1 - RX0, size=13, align="center")
path(f"M {RX0 + 2} {y_c + 28} L {RX1 - 2} {y_c + 28}", color=GREY, weight=1)
path(f"M {(RX0 + RX1) / 2} {y_c + 28} L {(RX0 + RX1) / 2} {y_b - 4}", color=GREY, weight=1, end="triangleArrow")
for lab, yy in (("sentence signatures", y_s), ("merged into chapters", y_c + 3), ("merged into the book", y_b + 4)):
    text(lab, LX, yy, w=RX0 - LX - 14, size=13, color=INK)
text("Each sentence is scanned once. Chapters and the book are merges of signatures, with no second pass over the text, and the same readouts apply at every level. "
     "A prefix table gives the totals of any run of sentences from two of its rows.", RX1 + 18, BY + 40, w=W - CX - 14 - (RX1 + 18), size=12.5, color=MUTED, lh=15.5)

os.makedirs(OUT, exist_ok=True)
script = os.path.join(OUT, "fig_overview.json")
json.dump(cmds, open(script, "w"), indent=1, ensure_ascii=False)
doc = os.path.join(OUT, "fig_overview.figcraft")
run = lambda *a: subprocess.run(["figcraft-cli", *a], check=True, capture_output=True, text=True)
out = run("run", script, "--out", doc)
bad = [l for l in out.stdout.splitlines() if '"ok":false' in l]
assert not bad, bad[:2]
run("export", doc, "-o", os.path.join(OUT, "fig_overview.pdf"))
run("render", doc, "-o", os.path.join(OUT, "fig_overview.png"), "--scale", "2")
print(f"made fig_overview ({len(cmds)} commands)")
