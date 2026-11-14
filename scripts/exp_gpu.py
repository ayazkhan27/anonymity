"""Transformer throughput on a graphics card. The paper's figures come from an NVIDIA RTX 3090 (CUDA, PyTorch 2.3.1):

    python3 scripts/exp_gpu.py

The ROCm build of PyTorch also works; the override below makes ROCm treat an RX 6600-class card (gfx1032) as the
supported gfx1030 target and has no effect on NVIDIA cards.
Same sentences, models and score as scripts/exp_speed.py; only the device differs.
"""
import json, os, re, sys, time

os.environ.setdefault("HSA_OVERRIDE_GFX_VERSION", "10.3.0")
os.environ.setdefault("HIP_VISIBLE_DEVICES", "0")
import numpy as np
import pandas as pd
import torch
from transformers import AutoModelForSequenceClassification, AutoTokenizer

HERE = os.path.dirname(os.path.abspath(__file__))
MODELS = {"distilbert": "distilbert/distilbert-base-uncased-finetuned-sst-2-english", "roberta": "cardiffnlp/twitter-roberta-base-sentiment-latest"}
SENT = re.compile(r"(?<=[.!?])\s+")


def main():
    assert torch.cuda.is_available(), "no CUDA or ROCm device"
    dev = torch.device("cuda")
    tr = pd.read_parquet(os.path.join(HERE, "..", "data", "yelp", "train.parquet")).sample(110000, random_state=0)
    sents = [s for r in tr.text.values[:40000] for s in SENT.split(r.replace("\n", " ")) if len(s) > 2][:200000]
    out = {"device": torch.cuda.get_device_name(0), "torch": torch.__version__, "vram_gb": torch.cuda.get_device_properties(0).total_memory / 2 ** 30}
    for name, repo in MODELS.items():
        tok = AutoTokenizer.from_pretrained(repo)
        model = AutoModelForSequenceClassification.from_pretrained(repo).eval().to(dev)
        for half in (False, True):
            m = model.half() if half else model

            def run(texts, batch=128):
                order = np.argsort([len(t) for t in texts])
                res = np.empty(len(texts))
                with torch.no_grad():
                    for i in range(0, len(texts), batch):
                        idx = order[i:i + batch]
                        enc = tok([texts[j] for j in idx], truncation=True, max_length=512, padding=True, return_tensors="pt").to(dev)
                        p = torch.softmax(m(**enc).logits.float(), -1)
                        res[idx] = (p[:, -1] - p[:, 0]).cpu().numpy()
                return res
            run(sents[:512])
            torch.cuda.synchronize()
            n = 20000
            t0 = time.perf_counter(); z = run(sents[:n]); torch.cuda.synchronize()
            out[f"{name}_{'fp16' if half else 'fp32'}"] = n / (time.perf_counter() - t0)
            out[f"{name}_check_mean_{'fp16' if half else 'fp32'}"] = float(z.mean())
            print(name, "fp16" if half else "fp32", f"{out[name + ('_fp16' if half else '_fp32')]:,.0f} sentences/s", flush=True)
    json.dump(out, open(os.path.join(HERE, "..", "results", "speed_gpu.json"), "w"), indent=1)
    print(out)


if __name__ == "__main__":
    main()
