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


def _load_fiqa(split: str = "test", limit: int = 5000) -> pd.DataFrame:
    """Load Twitter Financial News Sentiment as a replacement for FiQA.

    Source: zeroshot/twitter-financial-news-sentiment on HuggingFace
    Labels: Categorical (0: negative, 1: neutral, 2: positive) mapped to [-1.0, 0.0, 1.0].
    """
    logger.info(f"Loading Twitter Financial News (split={split})...")

    # FiQA is down on HF, substituting with Twitter Financial News Sentiment
    try:
        dataset = load_dataset("zeroshot/twitter-financial-news-sentiment")
    except Exception as e:
        logger.error(f"Failed to load Twitter Financial News dataset: {e}")
        return pd.DataFrame(columns=["text", "human_valence", "source", "metadata"])

    if split == "all":
        frames = []
        for s in dataset.keys():
            frames.append(dataset[s].to_pandas())
        df = pd.concat(frames, ignore_index=True)
    elif split in dataset:
        df = dataset[split].to_pandas()
    else:
        df = dataset["train"].to_pandas()
        logger.warning(f"Split '{split}' not found. Using 'train'.")

    # Identify columns
    text_col = None
    label_col = None
    for col in df.columns:
        if col.lower() in ["sentence", "text", "tweet"]:
            text_col = col
        if col.lower() in ["label", "sentiment"]:
            label_col = col

    if text_col is None or label_col is None:
        logger.error(f"Dataset columns: {list(df.columns)}")
        raise ValueError(f"Cannot identify text/label columns. Found: {list(df.columns)}")

    df = df.rename(columns={text_col: "text", label_col: "label_int"})

    # Map categorical labels (0: bearish/negative, 1: neutral, 2: bullish/positive) to continuous valence
    VALENCE_MAP = {0: -1.0, 1: 0.0, 2: 1.0}
    df["human_valence"] = df["label_int"].map(VALENCE_MAP)

    df["source"] = "fiqa_replacement"
    df["metadata"] = df["label_int"].apply(lambda x: {
        "original_label": {0: "negative", 1: "neutral", 2: "positive"}.get(x, "unknown")
    })

    df = df.dropna(subset=["text", "human_valence"])
    df = df[df["text"].str.strip().str.len() > 0]

    df = df.head(limit).reset_index(drop=True)
    return df[["text", "human_valence", "source", "metadata"]]


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
