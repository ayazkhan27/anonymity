"""Transformer comparators on the same test portions (CPU). Score = P(positive) - P(negative).

Texts longer than the model's 512-token window are truncated, as the models require.
Yelp is scored on a seeded 5,000-review subsample of its test portion to bound CPU time.
Also scores every sentence of 300 film reviews for the part-versus-whole comparison.
"""
import re, time
import numpy as np
import torch
from transformers import AutoModelForSequenceClassification, AutoTokenizer

from common import *

MODELS = {"distilbert": "distilbert/distilbert-base-uncased-finetuned-sst-2-english",
          "roberta": "cardiffnlp/twitter-roberta-base-sentiment-latest"}
SENT = re.compile(r"(?<=[.!?])\s+")


def scorer(name):
    tok = AutoTokenizer.from_pretrained(MODELS[name])
    model = AutoModelForSequenceClassification.from_pretrained(MODELS[name]).eval()
    labels = {v.lower(): k for k, v in model.config.id2label.items()}
    pos, neg = labels["positive"], labels["negative"]

    def run(texts, batch=32):
        order = np.argsort([len(t) for t in texts])
        out = np.empty(len(texts))
        with torch.no_grad():
            for i in range(0, len(texts), batch):
                idx = order[i:i + batch]
                enc = tok([texts[j] or " " for j in idx], truncation=True, max_length=512, padding=True, return_tensors="pt")
                p = torch.softmax(model(**enc).logits, -1).numpy()
                out[idx] = p[:, pos] - p[:, neg]
        return out
    return run, tok


if __name__ == "__main__":
    corpora = {c: load() for c, load in LOADERS.items()}
    review = corpora["scale"]["test"].text.values[:300]
    sents = [[s for s in SENT.split(t.replace("\n", " ")) if s.strip()] for t in review]
    timing = {}
    for name in MODELS:
        run, tok = scorer(name)
        for c in corpora:
            texts = list(corpora[c]["test"].text)

            def go():
                if c != "yelp":
                    return run(texts)
                pick = np.random.default_rng(0).choice(len(texts), 5000, replace=False)
                z = np.full(len(texts), np.nan)
                z[pick] = run([texts[i] for i in pick])
                return z
            t0 = time.time()
            cached(f"{name}_{c}_test", go)
            print(name, c, f"{time.time() - t0:.0f}s", flush=True)
        if name == "roberta":
            timing["share_truncated_film_reviews"] = float(np.mean([len(tok(t)["input_ids"]) > 512 for t in review]))
        flat = [s for ss in sents for s in ss]
        z = cached(f"{name}_scale_sentences", lambda: run(flat))
        np.save(os.path.join(CACHE, "scale_sentence_counts.npy"), np.array([len(ss) for ss in sents]))
        print(name, "sentences", len(flat), flush=True)
    save("transformers_meta", timing)
