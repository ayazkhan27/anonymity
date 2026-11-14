"""Throughput on this machine's CPU. Run it alone: other jobs distort the timings.

Sentences come from Yelp reviews. Every method scores the same sentences; slower methods get
a prefix of the list. Transformers are timed with one thread and with all threads.
"""
import multiprocessing, platform, re, resource, subprocess, time

import numpy as np

from common import *

SENT = re.compile(r"(?<=[.!?])\s+")


def rate(fn, items, repeat=3):
    best = min(timed(fn, items) for _ in range(repeat))
    return len(items) / best


def timed(fn, items):
    t0 = time.perf_counter()
    fn(items)
    return time.perf_counter() - t0


def score_chunk(chunk):
    return sgw.level(sgw.sketch(chunk)).sum()


def main():
    reviews = LOADERS["yelp"]()["train"].text.values[:40000]
    sents = [s for r in reviews for s in SENT.split(r.replace("\n", " ")) if len(s) > 2][:200000]
    words = float(sgw.sketch(sents)[:, sgw.L].sum())
    out = {"cpu": subprocess.run("lscpu | grep 'Model name' | sed 's/.*: *//'", shell=True, capture_output=True, text=True).stdout.strip(),
           "cores": multiprocessing.cpu_count(), "python": platform.python_version(), "n_sentences": len(sents), "words_per_sentence": words / len(sents)}
    out["sgw"] = rate(lambda x: sgw.level(sgw.sketch(x)), sents)
    out["sgw_words_per_s"] = out["sgw"] * words / len(sents)
    print(f"SGW, one core: {out['sgw']:,.0f} sentences/s ({out['sgw_words_per_s']/1e6:.2f} M words/s)", flush=True)
    # scaling in n, and a ten-million-sentence stream in fixed-size batches (memory stays flat)
    out["scaling"] = []
    for n in (1000, 10000, 100000, 1000000):
        items = (sents * (n // len(sents) + 1))[:n]
        out["scaling"].append([n, min(timed(lambda x: sgw.sketch(x), items) for _ in range(2))])
    t0, total = time.perf_counter(), 0
    for _ in range(50):
        total += len(sgw.sketch(sents))
    out["stream"] = {"sentences": total, "seconds": time.perf_counter() - t0, "peak_rss_gb": resource.getrusage(resource.RUSAGE_SELF).ru_maxrss / 1e6}
    print("scaling", out["scaling"], out["stream"], flush=True)
    chunks = [sents[i:i + 20000] for i in range(0, len(sents), 20000)] * 8
    with multiprocessing.Pool(out["cores"]) as pool:
        pool.map(score_chunk, chunks[:16])                        # warm up the workers
        t0 = time.perf_counter()
        pool.map(score_chunk, chunks)
        out["sgw_all_cores"] = sum(map(len, chunks)) / (time.perf_counter() - t0)
    print(f"SGW, {out['cores']} processes: {out['sgw_all_cores']:,.0f} sentences/s", flush=True)
    out["vader"] = rate(vader_scores, sents[:5000], 2)
    out["textblob"] = rate(textblob_scores, sents[:5000], 2)
    out["hedonometer"] = rate(hedonometer_scores, sents[:20000], 2)
    print({k: round(out[k]) for k in ("vader", "textblob", "hedonometer")}, flush=True)
    import torch
    from exp_transformers import MODELS, scorer
    for name in MODELS:
        run, _ = scorer(name)
        for threads, n in ((1, 600), (out["cores"], 3000)):
            torch.set_num_threads(threads)
            run(sents[:64])
            out[f"{name}_{threads}"] = rate(run, sents[:n], 1)
            print(f"{name}, {threads} thread(s): {out[f'{name}_{threads}']:,.1f} sentences/s", flush=True)
    out["params"] = {"distilbert": 66955010, "roberta": 124647939}
    save("speed", out)


def readout():
    """Cost of the calibrated readout relative to the level readout that main() times. A ratio, so any machine will do."""
    reviews = LOADERS["yelp"]()["train"].text.values[:40000]
    sents = [s for r in reviews for s in SENT.split(r.replace("\n", " ")) if len(s) > 2][:200000]
    lvl = min(timed(lambda x: sgw.level(sgw.sketch(x)), sents) for _ in range(3))
    val = min(timed(lambda x: sgw.valence(sgw.sketch(x)), sents) for _ in range(3))
    save("speed_readout", {"n_sentences": len(sents), "level_per_s": len(sents) / lvl, "valence_per_s": len(sents) / val, "valence_over_level_time": val / lvl})
    print(f"level {len(sents) / lvl:,.0f}/s, calibrated valence {len(sents) / val:,.0f}/s, time ratio {val / lvl:.3f}")


if __name__ == "__main__":
    readout() if sys.argv[1:] == ["readout"] else main()
