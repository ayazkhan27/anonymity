"""Download every lexicon and corpus the paper uses into data/ (git-ignored).

    python3 scripts/fetch_data.py            # everything
    python3 scripts/fetch_data.py lex emobank  # named sources only

Nothing here is redistributed by this repository except the open lexicon built
from labMT (CC BY) and the VADER word list (MIT); see scripts/build_lexicon.py.
"""
import csv, io, os, random, re, shutil, sys, tarfile, time, urllib.request, zipfile
from concurrent.futures import ThreadPoolExecutor

ROOT = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "data")
UA = {"User-Agent": "Mozilla/5.0 (research script; sentiment measurement study)"}
HF = "https://huggingface.co/datasets/{}/resolve/refs%2Fconvert%2Fparquet/{}"


def get(url, dest, tries=3):
    dest = os.path.join(ROOT, dest)
    if os.path.exists(dest) and os.path.getsize(dest) > 0:
        return dest
    os.makedirs(os.path.dirname(dest), exist_ok=True)
    for k in range(tries):
        try:
            with urllib.request.urlopen(urllib.request.Request(url, headers=UA), timeout=120) as r, open(dest + ".part", "wb") as f:
                shutil.copyfileobj(r, f)
            os.replace(dest + ".part", dest)
            return dest
        except Exception as e:  # network hiccup: retry, then report
            err = e
            time.sleep(2 * (k + 1))
    print(f"  FAILED {url}: {err}")
    return None


def lex():
    get("https://raw.githubusercontent.com/ryanjgallagher/shifterator/master/shifterator/lexicons/labMT/labMT_English.tsv", "lex/labMT_English.tsv")
    import vaderSentiment
    src = os.path.dirname(vaderSentiment.__file__)
    for f in ("vader_lexicon.txt", "emoji_utf8_lexicon.txt"):
        shutil.copy(os.path.join(src, f), os.path.join(ROOT, "lex", f))
    for url, name in (("https://saifmohammad.com/WebDocs/Lexicons/NRC-VAD-Lexicon.zip", "nrcvad"),
                      ("https://static-content.springer.com/esm/art%3A10.3758%2Fs13428-012-0314-x/MediaObjects/13428_2012_314_MOESM1_ESM.zip", "warriner")):
        z = get(url, f"lex_restricted/{name}.zip")  # licences bar redistribution: sensitivity checks only
        if z:
            zipfile.ZipFile(z).extractall(os.path.join(ROOT, "lex_restricted", name))


def emobank():
    get("https://raw.githubusercontent.com/JULIELab/EmoBank/master/corpus/emobank.csv", "emobank/emobank.csv")


def fb():
    get("https://raw.githubusercontent.com/wwbp/additional_data_sets/master/valence_arousal/dataset-fb-valence-arousal-anon.csv", "fb/fb_va.csv")


def semeval():
    z = get("http://saifmohammad.com/WebDocs/AIT-2018/AIT2018-DATA/SemEval2018-Task1-all-data.zip", "semeval/all.zip")
    if z:
        with zipfile.ZipFile(z) as zf:
            for n in zf.namelist():
                if re.search(r"Valence-reg-En-(train|dev|test-gold)\.txt$", n):
                    with open(os.path.join(ROOT, "semeval", os.path.basename(n)), "wb") as f:
                        f.write(zf.read(n))


def sst():
    for part in ("default/train", "default/validation", "default/test", "dictionary/dictionary", "ptb/train"):
        get(HF.format("stanfordnlp/sst", part + "/0000.parquet"), f"sst/{part.replace('/', '_')}.parquet")


def scale():
    for name in ("scale_data", "scale_whole_review"):
        t = get(f"https://www.cs.cornell.edu/people/pabo/movie-review-data/{name}.tar.gz", f"scale/{name}.tar.gz")
        if t and not os.path.isdir(os.path.join(ROOT, "scale", name)):
            with tarfile.open(t) as tf:
                tf.extractall(os.path.join(ROOT, "scale", name), filter="data")


def yelp():
    for part in ("test", "train"):
        get(f"https://huggingface.co/api/datasets/Yelp/yelp_review_full/parquet/yelp_review_full/{part}/0.parquet", f"yelp/{part}.parquet")


def alm():
    for name in ("Potter", "HCAndersen", "Grimms"):
        t = get(f"https://people.rc.rit.edu/coagla/affectdata/{name}.tar.gz", f"alm/{name}.tar.gz")
        if t and not os.path.isdir(os.path.join(ROOT, "alm", name)):
            with tarfile.open(t) as tf:
                tf.extractall(os.path.join(ROOT, "alm", name), filter="data")


def gutenberg(want=400, seed=0):
    """Seeded sample of English fiction from the Project Gutenberg catalogue, fetched from a mirror."""
    cat = get("https://www.gutenberg.org/cache/epub/feeds/pg_catalog.csv", "gutenberg/pg_catalog.csv")
    rows = [r for r in csv.DictReader(open(cat, encoding="utf-8"))
            if r["Type"] == "Text" and r["Language"] == "en" and "iction" in r["Subjects"]
            and r["LoCC"][:2] in ("PR", "PS", "PZ")]
    random.Random(seed).shuffle(rows)
    ids = [r["Text#"] for r in rows[: int(want * 1.6)]]

    def one(i):
        d = "/".join(i[:-1]) or "0"
        for suffix in ("-0.txt", ".txt", "-8.txt"):
            if get(f"https://gutenberg.pglaf.org/{d}/{i}/{i}{suffix}", f"gutenberg/books/{i}.txt", tries=1):
                return i
        return None

    with ThreadPoolExecutor(4) as ex:
        got = [i for i in ex.map(one, ids) if i]
    with open(os.path.join(ROOT, "gutenberg", "sample.csv"), "w", newline="", encoding="utf-8") as f:
        w = csv.writer(f)
        w.writerow(["id", "title", "authors"])
        for r in rows:
            if r["Text#"] in got:
                w.writerow([r["Text#"], r["Title"].replace("\n", " "), r["Authors"]])
    print(f"  gutenberg: {len(got)} of {len(ids)} fetched")


SOURCES = dict(lex=lex, emobank=emobank, fb=fb, semeval=semeval, sst=sst, scale=scale, alm=alm, yelp=yelp, gutenberg=gutenberg)

if __name__ == "__main__":
    for name in sys.argv[1:] or SOURCES:
        print(name)
        SOURCES[name]()
