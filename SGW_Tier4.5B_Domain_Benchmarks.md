# SGW TIER 4.5B — DOMAIN BENCHMARK INTEGRATION

## CONTEXT FOR THE EXECUTING MODEL

You are working on the `feature/sgw-v3-plus` branch of `https://github.com/ayazkhan27/anonymity`. The SGW engine, domain-adaptive axes, explanation output, and stress tests are already implemented. Your job is to replace the broken CX benchmark with proper domain-specific benchmarks that demonstrate SGW's real advantages.

The previous CX benchmark failed because:
- The Amazon Reviews loader pulled only short texts (no reviews over 500 chars)
- SGW scored ρ=0.0 on the short-text segment (333 samples)
- The medium segment returned NaN (zero variance in one array)
- No long-text segment existed at all

You are replacing that benchmark with datasets where SGW's three structural advantages matter: domain-specific vocabulary, long-form text, and continuous scoring.

### IMPORTANT CONSTRAINTS
- Do NOT modify any files in `sgw/core/engine.py`, `sgw/core/decay/`, `sgw/core/dilution/`, or `sgw/core/valence/scorer.py`.
- Do NOT modify `sgw/core/valence/axis.py` or `sgw/core/explanation.py`.
- You ARE modifying/creating files in `sgw/evaluation/` and `tests/`.
- Use `en_core_web_sm` (already in config) for spaCy parsing. Do NOT switch to `en_core_web_trf` unless explicitly told.
- Use `load_vectors(limit=500000)` for GloVe. Do NOT load full 2.2M vectors.
- All results must be saved to `reports/metrics/` as JSON files.

---

## STEP 1: CREATE THE UNIFIED BENCHMARK LOADER

Create file `sgw/evaluation/benchmark_loaders.py`. This replaces the old `cx_benchmark.py` loader. It must handle all four target datasets through a single interface.

```python
"""Unified benchmark loader for SGW evaluation.

Loads and normalizes four datasets into a common format:
    DataFrame with columns: [text, human_valence, source, metadata]

Where human_valence is always a float in [-1.0, 1.0].

Datasets:
    1. fiqa       — FiQA Task 1 financial sentiment (continuous labels)
    2. phrasebank — Financial PhraseBank (categorical, mapped to continuous)
    3. aiera      — Aiera earnings call transcript sentiment (categorical)
    4. imdb       — IMDB movie reviews (binary, already working)
"""

import pandas as pd
import numpy as np
from datasets import load_dataset
from sgw.utils.logger import get_logger

logger = get_logger(__name__)


def load_benchmark(name: str, split: str = "test", limit: int = 5000) -> pd.DataFrame:
    """Load a benchmark dataset normalized to common format.

    Args:
        name: One of 'fiqa', 'phrasebank', 'aiera', 'imdb'.
        split: Dataset split. Ignored for datasets without splits.
        limit: Maximum number of examples to load.

    Returns:
        DataFrame with columns:
            - text (str): The input text.
            - human_valence (float): Ground truth in [-1.0, 1.0].
            - source (str): Dataset name.
            - metadata (dict): Any extra info (star rating, category, etc.)

    Raises:
        ValueError: If dataset name is not recognized.
    """
    loaders = {
        "fiqa": _load_fiqa,
        "phrasebank": _load_phrasebank,
        "aiera": _load_aiera,
        "imdb": _load_imdb,
    }

    if name not in loaders:
        raise ValueError(f"Unknown benchmark '{name}'. Available: {list(loaders.keys())}")

    df = loaders[name](split=split, limit=limit)

    # Validate output format
    assert "text" in df.columns, f"Missing 'text' column in {name}"
    assert "human_valence" in df.columns, f"Missing 'human_valence' column in {name}"
    assert df["human_valence"].between(-1.0, 1.0).all(), f"Valence out of range in {name}"
    assert df["text"].str.len().min() > 0, f"Empty texts found in {name}"

    logger.info(f"Loaded {name}: {len(df)} examples, "
                f"valence mean={df['human_valence'].mean():.3f}, "
                f"std={df['human_valence'].std():.3f}, "
                f"text length median={df['text'].str.len().median():.0f} chars")
    return df
```

Now implement each loader function inside the same file. Here is exactly how to handle each dataset:

### 1a. FiQA Loader

FiQA Task 1 has continuous sentiment scores in [-1, 1]. This is the most important benchmark because its label format matches SGW's output directly.

```python
def _load_fiqa(split: str = "test", limit: int = 5000) -> pd.DataFrame:
    """Load FiQA Task 1 financial sentiment dataset.

    Source: ChanceFocus/fiqa-sentiment-classification on HuggingFace
    Labels: Continuous float in [-1, 1]
    Size: ~1,174 total (train=822, valid=117, test=235)

    IMPORTANT: This dataset is small. Use ALL examples from ALL splits
    when split='all'. For standard evaluation, use split='test'.
    """
    logger.info(f"Loading FiQA (split={split})...")

    dataset = load_dataset("ChanceFocus/fiqa-sentiment-classification")

    if split == "all":
        # Combine all splits for maximum sample size
        frames = []
        for s in ["train", "valid", "test"]:
            if s in dataset:
                frames.append(dataset[s].to_pandas())
        df = pd.concat(frames, ignore_index=True)
    elif split in dataset:
        df = dataset[split].to_pandas()
    else:
        # Fallback: try loading without split specification
        df = dataset["train"].to_pandas()
        logger.warning(f"Split '{split}' not found in FiQA. Using 'train'.")

    # Identify columns
    # FiQA has: sentence, label (sentiment score), type (headline/post)
    text_col = None
    score_col = None
    for col in df.columns:
        if col.lower() in ["sentence", "text"]:
            text_col = col
        if col.lower() in ["label", "score", "sentiment_score"]:
            score_col = col

    if text_col is None or score_col is None:
        logger.error(f"FiQA columns: {list(df.columns)}")
        raise ValueError(f"Cannot identify text/score columns. Found: {list(df.columns)}")

    df = df.rename(columns={text_col: "text", score_col: "human_valence"})
    df["human_valence"] = df["human_valence"].astype(float)

    # Clip to [-1, 1] in case any scores exceed bounds
    df["human_valence"] = df["human_valence"].clip(-1.0, 1.0)

    # Add metadata
    df["source"] = "fiqa"
    if "type" in df.columns:
        df["metadata"] = df["type"].apply(lambda x: {"type": x})
    else:
        df["metadata"] = [{}] * len(df)

    # Remove empty texts and NaN valences
    df = df.dropna(subset=["text", "human_valence"])
    df = df[df["text"].str.strip().str.len() > 0]

    df = df.head(limit).reset_index(drop=True)
    return df[["text", "human_valence", "source", "metadata"]]
```

### 1b. Financial PhraseBank Loader

Financial PhraseBank has categorical labels (positive/negative/neutral). Map them to continuous values. Use the high-agreement subset for cleaner evaluation.

```python
def _load_phrasebank(split: str = "test", limit: int = 5000) -> pd.DataFrame:
    """Load Financial PhraseBank dataset.

    Source: takala/financial_phrasebank on HuggingFace
    Labels: Categorical (positive=2, neutral=1, negative=0)
    Size: 2,264 (sentences_allagree) to 4,846 (sentences_50agree)

    Uses sentences_allagree (100% annotator agreement) for cleanest signal.
    Since there is no train/test split, we take the last 20% as 'test'.
    """
    logger.info(f"Loading Financial PhraseBank (split={split})...")

    # Try sentences_allagree first (highest quality), fall back to sentences_75agree
    try:
        dataset = load_dataset(
            "financial_phrasebank", "sentences_allagree",
            trust_remote_code=True
        )
    except Exception:
        try:
            dataset = load_dataset(
                "takala/financial_phrasebank", "sentences_allagree",
                trust_remote_code=True
            )
        except Exception as e:
            logger.warning(f"Could not load financial_phrasebank from HuggingFace: {e}")
            logger.info("Trying alternative source...")
            dataset = load_dataset(
                "zeroshot/twitter-financial-news-sentiment",
                trust_remote_code=True
            )

    df = dataset["train"].to_pandas()

    # Identify columns - PhraseBank uses 'sentence' and 'label'
    text_col = None
    label_col = None
    for col in df.columns:
        if col.lower() in ["sentence", "text"]:
            text_col = col
        if col.lower() in ["label", "sentiment"]:
            label_col = col

    if text_col is None or label_col is None:
        logger.error(f"PhraseBank columns: {list(df.columns)}")
        raise ValueError(f"Cannot identify columns. Found: {list(df.columns)}")

    df = df.rename(columns={text_col: "text", label_col: "label_int"})

    # Map categorical labels to continuous valence
    # PhraseBank labels: 0=negative, 1=neutral, 2=positive
    PHRASEBANK_MAP = {0: -0.8, 1: 0.0, 2: 0.8}
    df["human_valence"] = df["label_int"].map(PHRASEBANK_MAP)

    # Create pseudo train/test split (no official split exists)
    # Shuffle with fixed seed, take last 20% as test
    df = df.sample(frac=1.0, random_state=42).reset_index(drop=True)
    test_start = int(len(df) * 0.8)

    if split == "test":
        df = df.iloc[test_start:]
    elif split == "train":
        df = df.iloc[:test_start]
    # split == "all" keeps everything

    df["source"] = "phrasebank"
    df["metadata"] = df["label_int"].apply(lambda x: {
        "original_label": {0: "negative", 1: "neutral", 2: "positive"}.get(x, "unknown")
    })

    df = df.dropna(subset=["text", "human_valence"])
    df = df[df["text"].str.strip().str.len() > 0]
    df = df.head(limit).reset_index(drop=True)
    return df[["text", "human_valence", "source", "metadata"]]
```

### 1c. Aiera Earnings Call Transcript Loader

Aiera provides earnings call transcript segments with sentiment labels. These are long, formal, domain-specific — ideal for SGW.

```python
def _load_aiera(split: str = "test", limit: int = 5000) -> pd.DataFrame:
    """Load Aiera earnings call transcript sentiment dataset.

    Source: Aiera/aiera-transcript-sentiment on HuggingFace
    Labels: Categorical (positive, negative, neutral)
    Text type: Long earnings call transcript segments
    """
    logger.info(f"Loading Aiera earnings call transcripts (split={split})...")

    try:
        dataset = load_dataset("Aiera/aiera-transcript-sentiment")
    except Exception as e:
        logger.error(f"Failed to load Aiera dataset: {e}")
        logger.info("Aiera dataset may require authentication or may have moved.")
        logger.info("Returning empty DataFrame — skip this benchmark.")
        return pd.DataFrame(columns=["text", "human_valence", "source", "metadata"])

    # Identify the available split
    available_splits = list(dataset.keys())
    if split in available_splits:
        df = dataset[split].to_pandas()
    elif "train" in available_splits:
        df = dataset["train"].to_pandas()
        # Create pseudo split
        df = df.sample(frac=1.0, random_state=42).reset_index(drop=True)
        test_start = int(len(df) * 0.8)
        if split == "test":
            df = df.iloc[test_start:]
        elif split == "train":
            df = df.iloc[:test_start]
    else:
        df = dataset[available_splits[0]].to_pandas()

    # Identify columns
    text_col = None
    label_col = None
    for col in df.columns:
        col_lower = col.lower()
        if col_lower in ["transcript", "text", "sentence", "content"]:
            text_col = col
        if col_lower in ["sentiment", "label"]:
            label_col = col

    if text_col is None or label_col is None:
        logger.error(f"Aiera columns: {list(df.columns)}")
        raise ValueError(f"Cannot identify columns. Found: {list(df.columns)}")

    df = df.rename(columns={text_col: "text", label_col: "sentiment_label"})

    # Map categorical to continuous
    # Use asymmetric mapping: negative stronger than positive
    # because financial language is hedged and cautious
    AIERA_MAP = {
        "positive": 0.7,
        "negative": -0.7,
        "neutral": 0.0,
    }
    df["sentiment_label"] = df["sentiment_label"].str.lower().str.strip()
    df["human_valence"] = df["sentiment_label"].map(AIERA_MAP)

    # Drop rows that didn't map
    unmapped = df["human_valence"].isna().sum()
    if unmapped > 0:
        logger.warning(f"Dropping {unmapped} rows with unmapped sentiment labels: "
                       f"{df[df['human_valence'].isna()]['sentiment_label'].unique()}")
    df = df.dropna(subset=["human_valence"])

    df["source"] = "aiera"
    df["metadata"] = df["sentiment_label"].apply(lambda x: {"original_label": x})

    df = df[df["text"].str.strip().str.len() > 0]
    df = df.head(limit).reset_index(drop=True)

    logger.info(f"Aiera: median text length = {df['text'].str.len().median():.0f} chars, "
                f"label distribution: {df['sentiment_label'].value_counts().to_dict()}")
    return df[["text", "human_valence", "source", "metadata"]]
```

### 1d. IMDB Loader (Keep Existing, Standardize Format)

```python
def _load_imdb(split: str = "test", limit: int = 5000) -> pd.DataFrame:
    """Load IMDB movie review dataset.

    Source: stanfordnlp/imdb on HuggingFace
    Labels: Binary (0=negative, 1=positive)
    Text type: Long movie reviews (avg ~230 words)
    """
    logger.info(f"Loading IMDB (split={split})...")

    dataset = load_dataset("stanfordnlp/imdb", split=split)
    df = dataset.to_pandas()

    df = df.rename(columns={"label": "label_int"})

    # Map binary to continuous (strong polarity)
    df["human_valence"] = df["label_int"].map({0: -0.9, 1: 0.9})

    df["source"] = "imdb"
    df["metadata"] = df["label_int"].apply(lambda x: {
        "original_label": "positive" if x == 1 else "negative"
    })

    # Stratified sample: equal positive and negative
    per_class = limit // 2
    pos = df[df["label_int"] == 1].sample(n=min(per_class, (df["label_int"] == 1).sum()),
                                           random_state=42)
    neg = df[df["label_int"] == 0].sample(n=min(per_class, (df["label_int"] == 0).sum()),
                                           random_state=42)
    df = pd.concat([pos, neg]).sample(frac=1.0, random_state=42).reset_index(drop=True)

    df = df[df["text"].str.strip().str.len() > 0]
    return df[["text", "human_valence", "source", "metadata"]]
```

---

## STEP 2: CREATE THE DOMAIN BENCHMARK RUNNER

Create file `sgw/evaluation/run_domain_benchmarks.py`. This is the main evaluation script that replaces the broken CX benchmark.

Key design decisions:
- Run each financial dataset with BOTH the general axis AND the finance axis
- Run IMDB with the general axis only (it's not financial text)
- Compare SGW vs VADER on every dataset
- Report domain adaptation delta (finance axis ρ minus general axis ρ) for financial datasets
- Segment by text length where sample size permits (short <100 chars, medium 100-500, long >500)
- Save everything to JSON

```python
"""Domain benchmark runner for SGW.

Evaluates SGW against VADER across domain-specific datasets,
demonstrating domain adaptation and long-form text advantages.
"""

import json
import sys
import numpy as np
import pandas as pd
from pathlib import Path
from sgw.utils.vectors import load_vectors
from sgw.utils.config import load_config
from sgw.core.valence.scorer import ValenceScorer
from sgw.core.valence.axis import SentimentAxis
from sgw.pipeline.syntax import SyntaxParser
from sgw.core.engine import SGWEngine
from sgw.evaluation.benchmark_loaders import load_benchmark
from sgw.evaluation.baselines import compute_vader_scores
from sgw.evaluation.metrics import evaluate_continuous
from sgw.utils.logger import get_logger

logger = get_logger(__name__)

FINANCIAL_BENCHMARKS = ["fiqa", "phrasebank", "aiera"]
GENERAL_BENCHMARKS = ["imdb"]
ALL_BENCHMARKS = FINANCIAL_BENCHMARKS + GENERAL_BENCHMARKS

MINIMUM_SAMPLES_FOR_SEGMENT = 30  # Don't report metrics on fewer than this


def run_single_benchmark(engine, df, dataset_name, axis_name, vader_scores=None):
    """Run SGW and VADER evaluation on a single dataset.

    Returns dict with sgw metrics, vader metrics, and length-segmented results.
    """
    texts = df["text"].tolist()
    y_true = df["human_valence"].values

    # SGW scores
    y_sgw = engine.score_batch(texts, show_progress=True)
    sgw_metrics = evaluate_continuous(y_true, y_sgw, f"SGW ({dataset_name}/{axis_name})")

    # VADER scores (compute once, reuse across axes)
    if vader_scores is None:
        vader_scores = compute_vader_scores(df["text"])
    vader_metrics = evaluate_continuous(y_true, vader_scores, f"VADER ({dataset_name})")

    result = {
        "sgw": sgw_metrics,
        "vader": vader_metrics,
        "axis_used": axis_name,
        "n_samples": len(df),
        "text_length_median": float(df["text"].str.len().median()),
        "text_length_mean": float(df["text"].str.len().mean()),
        "sgw_beats_vader": sgw_metrics["spearman_rho"] > vader_metrics["spearman_rho"],
    }

    # Length-segmented analysis
    df_eval = df.copy()
    df_eval["sgw_score"] = y_sgw
    df_eval["vader_score"] = vader_scores
    df_eval["text_length"] = df_eval["text"].str.len()

    segments = {
        "short": df_eval[df_eval["text_length"] < 100],
        "medium": df_eval[(df_eval["text_length"] >= 100) & (df_eval["text_length"] < 500)],
        "long": df_eval[df_eval["text_length"] >= 500],
    }

    for seg_name, seg_df in segments.items():
        if len(seg_df) >= MINIMUM_SAMPLES_FOR_SEGMENT:
            seg_y_true = seg_df["human_valence"].values
            seg_y_sgw = seg_df["sgw_score"].values
            seg_y_vader = seg_df["vader_score"].values

            # Check for zero variance (causes NaN in correlation)
            if np.std(seg_y_true) < 1e-6 or np.std(seg_y_sgw) < 1e-6:
                logger.warning(f"Skipping {seg_name} segment for {dataset_name}: "
                              f"zero variance detected")
                continue

            seg_sgw = evaluate_continuous(seg_y_true, seg_y_sgw,
                                          f"SGW ({dataset_name}/{seg_name})")
            seg_vader = evaluate_continuous(seg_y_true, seg_y_vader,
                                            f"VADER ({dataset_name}/{seg_name})")
            result[f"sgw_{seg_name}"] = seg_sgw
            result[f"vader_{seg_name}"] = seg_vader
            result[f"n_{seg_name}"] = len(seg_df)

    return result, vader_scores


def main():
    print("=" * 70)
    print("SGW DOMAIN BENCHMARK SUITE")
    print("=" * 70)

    # --- Setup ---
    vectors = load_vectors(limit=500000)
    config = load_config()
    scorer = ValenceScorer(vectors, config)
    parser = SyntaxParser(config)
    engine = SGWEngine(scorer, parser, config)

    # Load calibration if available
    cal_path = Path("reports/metrics/calibration_results.json")
    if cal_path.exists():
        with open(cal_path) as f:
            cal = json.load(f)
        engine.set_lambda(cal["best_lambda"])
        engine.set_k(cal["best_k"])
        logger.info(f"Loaded calibration: lambda={cal['best_lambda']}, k={cal['best_k']}")

    final_path = Path("reports/metrics/final_evaluation.json")
    if final_path.exists():
        with open(final_path) as f:
            final = json.load(f)
        vm = final.get("variance_matching", {})
        if vm and vm.get("sigma_s", 1.0) != 1.0:
            engine.set_calibration_scale(vm["mu_s"], vm["sigma_s"], vm["sigma_y"])
            logger.info("Loaded variance-matching calibration.")

    # Build axes
    general_axis = SentimentAxis.from_preset(vectors, "general")
    finance_axis = SentimentAxis.from_preset(vectors, "finance")

    all_results = {}

    # ================================================================
    # FINANCIAL BENCHMARKS: Test with both general and finance axes
    # ================================================================
    for bench_name in FINANCIAL_BENCHMARKS:
        print(f"\n{'=' * 50}")
        print(f"BENCHMARK: {bench_name.upper()}")
        print(f"{'=' * 50}")

        try:
            # Use split='all' for FiQA since it's small (1,174 total)
            # Use split='test' for others
            use_split = "all" if bench_name == "fiqa" else "test"
            df = load_benchmark(bench_name, split=use_split, limit=5000)
        except Exception as e:
            logger.error(f"Failed to load {bench_name}: {e}")
            all_results[bench_name] = {"status": "load_failed", "error": str(e)}
            continue

        if len(df) == 0:
            logger.warning(f"No data for {bench_name}. Skipping.")
            all_results[bench_name] = {"status": "empty"}
            continue

        # Compute VADER once (same regardless of axis)
        vader_scores = compute_vader_scores(df["text"])

        # Test 1: General axis
        logger.info(f"\n--- {bench_name} with GENERAL axis ---")
        scorer.swap_axis(general_axis)
        result_general, _ = run_single_benchmark(
            engine, df, bench_name, "general", vader_scores
        )

        # Test 2: Finance axis
        logger.info(f"\n--- {bench_name} with FINANCE axis ---")
        scorer.swap_axis(finance_axis)
        result_finance, _ = run_single_benchmark(
            engine, df, bench_name, "finance", vader_scores
        )

        # Domain adaptation delta
        delta = (result_finance["sgw"]["spearman_rho"] -
                 result_general["sgw"]["spearman_rho"])

        all_results[bench_name] = {
            "status": "ok",
            "general_axis": result_general,
            "finance_axis": result_finance,
            "domain_adaptation_delta": delta,
            "best_axis": "finance" if delta > 0 else "general",
            "best_sgw_rho": max(
                result_general["sgw"]["spearman_rho"],
                result_finance["sgw"]["spearman_rho"]
            ),
            "vader_rho": result_general["vader"]["spearman_rho"],
            "sgw_beats_vader": max(
                result_general["sgw"]["spearman_rho"],
                result_finance["sgw"]["spearman_rho"]
            ) > result_general["vader"]["spearman_rho"],
        }

        logger.info(f"\n{bench_name} SUMMARY:")
        logger.info(f"  General axis ρ = {result_general['sgw']['spearman_rho']:.4f}")
        logger.info(f"  Finance axis ρ = {result_finance['sgw']['spearman_rho']:.4f}")
        logger.info(f"  VADER ρ        = {result_general['vader']['spearman_rho']:.4f}")
        logger.info(f"  Adaptation Δ   = {delta:+.4f}")

    # ================================================================
    # GENERAL BENCHMARKS: Test with general axis only
    # ================================================================
    scorer.swap_axis(general_axis)

    for bench_name in GENERAL_BENCHMARKS:
        print(f"\n{'=' * 50}")
        print(f"BENCHMARK: {bench_name.upper()}")
        print(f"{'=' * 50}")

        try:
            df = load_benchmark(bench_name, split="test", limit=5000)
        except Exception as e:
            logger.error(f"Failed to load {bench_name}: {e}")
            all_results[bench_name] = {"status": "load_failed", "error": str(e)}
            continue

        result, _ = run_single_benchmark(engine, df, bench_name, "general")
        all_results[bench_name] = {
            "status": "ok",
            "general_axis": result,
            "sgw_rho": result["sgw"]["spearman_rho"],
            "vader_rho": result["vader"]["spearman_rho"],
            "sgw_beats_vader": result["sgw_beats_vader"],
        }

    # ================================================================
    # CROSS-DOMAIN SUMMARY
    # ================================================================
    print("\n" + "=" * 70)
    print("CROSS-DOMAIN SUMMARY")
    print("=" * 70)

    summary_rows = []
    wins = 0
    total = 0

    for bench_name, res in all_results.items():
        if res.get("status") != "ok":
            continue

        if bench_name in FINANCIAL_BENCHMARKS:
            best_rho = res["best_sgw_rho"]
            vader_rho = res["vader_rho"]
            axis = res["best_axis"]
            delta = res["domain_adaptation_delta"]
        else:
            best_rho = res["sgw_rho"]
            vader_rho = res["vader_rho"]
            axis = "general"
            delta = 0.0

        beats = best_rho > vader_rho
        if beats:
            wins += 1
        total += 1

        summary_rows.append({
            "dataset": bench_name,
            "sgw_rho": best_rho,
            "vader_rho": vader_rho,
            "winner": "SGW" if beats else "VADER",
            "margin": best_rho - vader_rho,
            "best_axis": axis,
            "adaptation_delta": delta,
        })

        status = "WIN" if beats else "LOSS"
        print(f"  {bench_name:15s} | SGW ρ={best_rho:.4f} vs VADER ρ={vader_rho:.4f} "
              f"| {status} by {abs(best_rho - vader_rho):.4f} | axis={axis}")

    print(f"\n  SGW wins {wins}/{total} benchmarks")

    all_results["summary"] = {
        "datasets_evaluated": total,
        "sgw_wins": wins,
        "details": summary_rows,
    }

    # ================================================================
    # GATE CHECK
    # ================================================================
    gate_pass = wins >= 2
    all_results["gate_beats_vader_2of4"] = gate_pass
    print(f"\n  GATE (beats VADER on ≥2 datasets): {'PASS' if gate_pass else 'FAIL'}")

    # Save
    save_path = "reports/metrics/domain_benchmarks.json"
    with open(save_path, "w") as f:
        json.dump(all_results, f, indent=2, default=str)
    print(f"\n  Results saved to {save_path}")
    print("=" * 70)

    return all_results


if __name__ == "__main__":
    main()
```

---

## STEP 3: UPDATE run_evaluation.py TO USE NEW BENCHMARKS

In `sgw/evaluation/run_evaluation.py`, replace the Phase 5 benchmarks section. Find the block that starts with `# --- Phase 5: Structure Validation Benchmarks ---` and replace it entirely with:

```python
    # --- Phase 5: Domain Benchmarks ---
    logger.info("\n" + "=" * 50)
    logger.info("PHASE 5: DOMAIN-SPECIFIC BENCHMARKS")
    logger.info("=" * 50)

    from sgw.evaluation.run_domain_benchmarks import main as run_domain_benchmarks
    domain_results = run_domain_benchmarks()
    benchmark_results = domain_results.get("summary", {}).get("details", [])
```

Also update Gate 2. Find this line:
```python
    gates["beats_vader"] = sgw_results["spearman_rho"] > vader_results["spearman_rho"]
```

Replace it with:
```python
    # Gate 2: Beat VADER on at least 2 of 4 domain benchmarks
    sgw_domain_wins = domain_results.get("summary", {}).get("sgw_wins", 0)
    gates["beats_vader_2of4"] = sgw_domain_wins >= 2
    logger.info(f"Gate 2 - Beats VADER on ≥2 benchmarks: {sgw_domain_wins} wins -> "
                f"{'PASS' if gates['beats_vader_2of4'] else 'FAIL'}")
```

---

## STEP 4: UPDATE configs/sgw_config.yaml WITH CALIBRATED DEFAULTS

The config still has placeholder values. Replace the decay and dilution sections at the top of the file:

```yaml
decay:
  lambda_default: 0.7  # Calibrated on GoEmotions validation set
  lambda_search_range: [0.1, 0.2, 0.3, 0.5, 0.7, 1.0, 1.5, 2.0, 2.5, 3.0]

dilution:
  k_default: 3.0  # Calibrated on GoEmotions validation set
  k_search_range: [0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 4.0, 5.0]
```

---

## STEP 5: TESTS

Create file `tests/unit/test_benchmark_loaders.py`:

```python
"""Unit tests for benchmark data loaders."""

import pytest
import pandas as pd


class TestBenchmarkLoaders:

    def test_fiqa_loads(self):
        from sgw.evaluation.benchmark_loaders import load_benchmark
        df = load_benchmark("fiqa", split="test", limit=50)
        assert len(df) > 0, "FiQA returned no data"
        assert "text" in df.columns
        assert "human_valence" in df.columns
        assert df["human_valence"].between(-1.0, 1.0).all()
        # FiQA has continuous labels — should have many unique values
        assert df["human_valence"].nunique() > 3, \
            f"FiQA should have continuous labels, got {df['human_valence'].nunique()} unique"

    def test_phrasebank_loads(self):
        from sgw.evaluation.benchmark_loaders import load_benchmark
        df = load_benchmark("phrasebank", split="test", limit=50)
        assert len(df) > 0
        assert df["human_valence"].between(-1.0, 1.0).all()
        # PhraseBank maps to 3 values: -0.8, 0.0, 0.8
        assert df["human_valence"].nunique() <= 3

    def test_imdb_loads(self):
        from sgw.evaluation.benchmark_loaders import load_benchmark
        df = load_benchmark("imdb", split="test", limit=50)
        assert len(df) > 0
        # IMDB reviews should be long
        assert df["text"].str.len().median() > 200, \
            f"IMDB reviews should be long, median={df['text'].str.len().median()}"

    def test_unknown_dataset_raises(self):
        from sgw.evaluation.benchmark_loaders import load_benchmark
        with pytest.raises(ValueError, match="Unknown benchmark"):
            load_benchmark("nonexistent_dataset")

    def test_fiqa_continuous_labels_have_variance(self):
        """Critical: FiQA labels must have real variance for Spearman to work."""
        from sgw.evaluation.benchmark_loaders import load_benchmark
        df = load_benchmark("fiqa", split="all", limit=200)
        assert df["human_valence"].std() > 0.1, \
            f"FiQA labels have insufficient variance: std={df['human_valence'].std()}"
```

Run: `python -m pytest tests/unit/test_benchmark_loaders.py -v`

---

## STEP 6: RUN THE FULL BENCHMARK SUITE

```bash
python sgw/evaluation/run_domain_benchmarks.py
```

This will:
1. Load all four datasets (FiQA, PhraseBank, Aiera, IMDB)
2. Score each financial dataset with both general and finance axes
3. Score IMDB with general axis
4. Report per-dataset and per-length-segment results
5. Compute domain adaptation deltas
6. Check the gate (SGW beats VADER on ≥2 datasets)
7. Save everything to `reports/metrics/domain_benchmarks.json`

Expected runtime: 15-45 minutes depending on hardware and spaCy model.

---

## EXPECTED OUTCOMES AND WHAT TO DO IF THINGS GO WRONG

### If FiQA doesn't load
The `ChanceFocus/fiqa-sentiment-classification` dataset has had HuggingFace hosting issues. If `load_dataset` fails, try:
```python
dataset = load_dataset("TheFinAI/fiqa-sentiment-classification")
```
If both fail, skip FiQA and document it. PhraseBank is the fallback financial benchmark.

### If PhraseBank triggers a "Dataset scripts are no longer supported" error
This is a known HuggingFace issue with older dataset scripts. Try:
```python
dataset = load_dataset("financial_phrasebank", "sentences_allagree",
                       trust_remote_code=True, revision="main")
```
Or download manually from: https://huggingface.co/datasets/takala/financial_phrasebank

### If Aiera dataset is empty or requires authentication
Skip it gracefully. The loader already returns an empty DataFrame on failure. The benchmark runner checks `len(df) == 0` and skips.

### If SGW doesn't beat VADER on financial datasets
This would mean the finance axis seeds are not effective. Debug by running:
```python
scorer.swap_axis(finance_axis)
print(scorer.get_valence("growth"))    # Should be positive
print(scorer.get_valence("loss"))      # Should be negative
print(scorer.get_valence("recession")) # Should be negative
print(scorer.get_valence("revenue"))   # Should be positive
```
If these don't show clear polarity separation, the GloVe vectors for these financial terms may not project well onto the axis. Consider expanding the finance seed list with more terms from the Loughran-McDonald financial sentiment dictionary.

### If the gate fails (fewer than 2 wins)
Report the results honestly. Document which datasets SGW won and lost on, and why. The length-segmented analysis will show whether SGW performs better on longer texts within each dataset — this is a publishable finding even if the overall ρ doesn't beat VADER.

---

## GIT COMMIT

After all tests pass and benchmarks complete:
```bash
git add -A
git commit -m "Tier 4.5B: Domain-specific benchmarks (FiQA, PhraseBank, Aiera, IMDB)"
git push origin feature/sgw-v3-plus
```
