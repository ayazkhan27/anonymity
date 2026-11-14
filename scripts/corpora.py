"""Loaders for the human-rated corpora. Each returns {split: DataFrame(text, y, group, ...)}.

`group` is the unit of independence used for bootstrap resampling (the source
document for EmoBank, the item itself elsewhere).
"""
import os
import numpy as np
import pandas as pd

DATA = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "data")
SEED = 0


def _split(df, fractions=(0.4, 0.1, 0.5)):
    """Seeded shuffle into train / dev / test for corpora without an official split."""
    idx = np.random.default_rng(SEED).permutation(len(df))
    a, b = int(fractions[0] * len(df)), int((fractions[0] + fractions[1]) * len(df))
    return {"train": df.iloc[idx[:a]], "dev": df.iloc[idx[a:b]], "test": df.iloc[idx[b:]]}


def emobank():
    df = pd.read_csv(f"{DATA}/emobank/emobank.csv").merge(pd.read_csv(f"{DATA}/emobank/meta.tsv", sep="\t"), on="id")
    df = df.rename(columns={"V": "y", "document": "group"})
    df["start"] = df.id.str.rsplit("_", n=2).str[1].astype(int)
    df["text"] = df.text.astype(str)
    return {s: df[df.split == s].reset_index(drop=True) for s in ("train", "dev", "test")}


def semeval():
    def read(name):
        d = pd.read_csv(f"{DATA}/semeval/2018-Valence-reg-En-{name}.txt", sep="\t", quoting=3, encoding="utf-8-sig")
        return pd.DataFrame({"text": d["Tweet"], "y": d["Intensity Score"], "group": d["ID"]})
    return {"train": read("train"), "dev": read("dev"), "test": read("test-gold")}


def fb():
    d = pd.read_csv(f"{DATA}/fb/fb_va.csv")
    df = pd.DataFrame({"text": d["Anonymized Message"].astype(str), "y": (d.Valence1 + d.Valence2) / 2,
                       "arousal": (d.Arousal1 + d.Arousal2) / 2, "group": np.arange(len(d))})
    return _split(df)


def sst():
    out = {}
    for split, name in (("train", "train"), ("dev", "validation"), ("test", "test")):
        d = pd.read_parquet(f"{DATA}/sst/default_{name}.parquet")
        out[split] = pd.DataFrame({"text": d.sentence, "y": d.label, "group": np.arange(len(d))})
    return out


def scale():
    rows = []
    base = f"{DATA}/scale"
    for author in sorted(os.listdir(f"{base}/scale_data/scaledata")):
        ids = open(f"{base}/scale_data/scaledata/{author}/id.{author}").read().split()
        ratings = open(f"{base}/scale_data/scaledata/{author}/rating.{author}").read().split()
        for i, r in zip(ids, ratings):
            path = f"{base}/scale_whole_review/scale_whole_review/{author}/txt.parag/{i}.txt"
            if os.path.exists(path):
                rows.append((open(path, encoding="latin-1").read(), float(r), author.replace("+", " ")))
    df = pd.DataFrame(rows, columns=["text", "y", "critic"])
    df["group"] = np.arange(len(df))
    return _split(df)


def yelp(n_train=100_000, n_dev=10_000):
    tr = pd.read_parquet(f"{DATA}/yelp/train.parquet").sample(n_train + n_dev, random_state=SEED)
    te = pd.read_parquet(f"{DATA}/yelp/test.parquet")
    f = lambda d: pd.DataFrame({"text": d.text.values, "y": d.label.values + 1.0, "group": np.arange(len(d))})
    return {"train": f(tr.iloc[:n_train]), "dev": f(tr.iloc[n_train:]), "test": f(te)}


LOADERS = dict(emobank=emobank, semeval=semeval, fb=fb, sst=sst, scale=scale, yelp=yelp)
NAMES = dict(emobank="EmoBank", semeval="SemEval-2018 tweets", fb="Facebook posts", sst="SST sentences",
             scale="Film reviews", yelp="Yelp reviews")

if __name__ == "__main__":
    import sys
    sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), ".."))
    import sgw
    for name, load in LOADERS.items():
        c = load()
        words = sgw.sketch(list(c["test"].text))[:, sgw.L]
        print(f"{name:8s}", {k: len(v) for k, v in c.items()}, f"median words {np.median(words):.0f}, y range {c['test'].y.min():.2f}..{c['test'].y.max():.2f}")
