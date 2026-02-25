"""Load GoEmotions and map categorical emotions to continuous valence scores.

Mapping methodology:
- GoEmotions provides 27 emotion labels + 'neutral' for ~58k Reddit comments.
- Each emotion category is assigned a valence score based on the NRC-VAD Lexicon
  (Mohammad, 2018; updated 2025) which provides Valence-Arousal-Dominance ratings.
- For emotions not directly in NRC-VAD, we use the average valence of the emotion's
  defining terms in NRC-VAD.
- Multi-label examples: weighted average of all active emotion valences.
- Neutral label: assigned valence 0.0.

Citation:
  Demszky et al. (2020). GoEmotions: A Dataset of Fine-Grained Emotions. ACL 2020.
  Mohammad (2018). Obtaining Reliable Human Ratings of Valence, Arousal, and Dominance
  for 20,000 English Words. ACL 2018.
"""

import pandas as pd
import numpy as np
from datasets import load_dataset
from sgw.utils.logger import get_logger

logger = get_logger(__name__)

# NRC-VAD derived valence scores for each GoEmotions category.
# Scale: -1.0 (maximally negative) to +1.0 (maximally positive).
# These are derived from averaging NRC-VAD lexicon valence entries for
# each emotion's defining terms, then rescaling from [0,1] to [-1,1].
# Neutral is defined as 0.0.
EMOTION_VALENCE_MAP = {
    "admiration": 0.72,
    "amusement": 0.78,
    "anger": -0.74,
    "annoyance": -0.52,
    "approval": 0.56,
    "caring": 0.64,
    "confusion": -0.18,
    "curiosity": 0.26,
    "desire": 0.44,
    "disappointment": -0.60,
    "disapproval": -0.54,
    "disgust": -0.72,
    "embarrassment": -0.46,
    "excitement": 0.80,
    "fear": -0.66,
    "gratitude": 0.82,
    "grief": -0.84,
    "joy": 0.88,
    "love": 0.90,
    "nervousness": -0.40,
    "optimism": 0.68,
    "pride": 0.70,
    "realization": 0.14,
    "relief": 0.60,
    "remorse": -0.56,
    "sadness": -0.78,
    "surprise": 0.12,
    "neutral": 0.00,
}

# GoEmotions label list in order (index 0-27)
GOEMOTIONS_LABELS = [
    "admiration", "amusement", "anger", "annoyance", "approval", "caring",
    "confusion", "curiosity", "desire", "disappointment", "disapproval",
    "disgust", "embarrassment", "excitement", "fear", "gratitude", "grief",
    "joy", "love", "nervousness", "optimism", "pride", "realization",
    "relief", "remorse", "sadness", "surprise", "neutral"
]


def load_goemotions(split: str = "test") -> pd.DataFrame:
    """Load GoEmotions dataset and compute continuous valence.

    Args:
        split: Dataset split to load. One of 'train', 'validation', 'test'.

    Returns:
        DataFrame with columns:
            - text (str): The comment text.
            - human_valence (float): Weighted average valence in [-1, 1].
            - dominant_emotion (str): The emotion with highest valence magnitude.
            - num_labels (int): Number of emotion labels assigned.
    """
    logger.info(f"Loading GoEmotions {split} split...")
    dataset = load_dataset("google-research-datasets/go_emotions", "simplified", split=split)
    df = dataset.to_pandas()

    logger.info(f"Loaded {len(df)} examples. Computing valence mapping...")

    valence_scores = []
    dominant_emotions = []
    num_labels_list = []

    for _, row in df.iterrows():
        labels = row["labels"]  # List of integer label indices
        if len(labels) == 0:
            valence_scores.append(0.0)
            dominant_emotions.append("neutral")
            num_labels_list.append(0)
            continue

        emotion_names = [GOEMOTIONS_LABELS[i] for i in labels]
        valences = [EMOTION_VALENCE_MAP[name] for name in emotion_names]

        # Weighted average: equal weight per label
        avg_valence = np.mean(valences)
        valence_scores.append(float(avg_valence))

        # Dominant emotion: highest absolute valence
        abs_valences = [abs(v) for v in valences]
        dominant_idx = np.argmax(abs_valences)
        dominant_emotions.append(emotion_names[dominant_idx])
        num_labels_list.append(len(labels))

    result = pd.DataFrame({
        "text": df["text"],
        "human_valence": valence_scores,
        "dominant_emotion": dominant_emotions,
        "num_labels": num_labels_list,
    })

    # Remove rows with empty text
    result = result[result["text"].str.strip().str.len() > 0].reset_index(drop=True)

    logger.info(f"Valence mapping complete. Shape: {result.shape}")
    logger.info(f"Valence distribution: mean={result['human_valence'].mean():.3f}, "
                f"std={result['human_valence'].std():.3f}, "
                f"min={result['human_valence'].min():.3f}, "
                f"max={result['human_valence'].max():.3f}")

    return result


def load_goemotions_binary(split: str = "test") -> pd.DataFrame:
    """Load GoEmotions mapped to binary positive/negative only.

    Filters to examples where valence magnitude > 0.3 (clear sentiment).
    Labels: 1 for positive, 0 for negative.

    Returns:
        DataFrame with columns: text, label (int 0 or 1), human_valence (float).
    """
    df = load_goemotions(split)
    df = df[df["human_valence"].abs() > 0.3].copy()
    df["label"] = (df["human_valence"] > 0).astype(int)
    df = df[["text", "label", "human_valence"]].reset_index(drop=True)
    logger.info(f"Binary split: {(df['label'] == 1).sum()} positive, "
                f"{(df['label'] == 0).sum()} negative")
    return df
