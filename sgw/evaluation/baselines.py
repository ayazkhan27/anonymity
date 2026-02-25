"""Baseline sentiment methods for comparison against SGW."""

import pandas as pd
import numpy as np
from nltk.sentiment.vader import SentimentIntensityAnalyzer
import nltk
from sgw.utils.logger import get_logger

logger = get_logger(__name__)


def ensure_vader_lexicon():
    """Download VADER lexicon if not present."""
    try:
        nltk.data.find("sentiment/vader_lexicon.zip")
    except LookupError:
        nltk.download("vader_lexicon", quiet=True)


def compute_vader_scores(texts: pd.Series) -> np.ndarray:
    """Compute VADER compound scores for a series of texts.

    Args:
        texts: Series of text strings.

    Returns:
        numpy array of compound scores in [-1, 1].
    """
    ensure_vader_lexicon()
    sia = SentimentIntensityAnalyzer()

    scores = []
    for text in texts:
        if not isinstance(text, str) or len(text.strip()) == 0:
            scores.append(0.0)
        else:
            compound = sia.polarity_scores(text)["compound"]
            scores.append(compound)

    return np.array(scores)
