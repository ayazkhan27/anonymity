"""Syntactic dilution via buffer ratio.

Buffer words (function words: determiners, conjunctions, prepositions, etc.)
dilute the emotional impact of a text. The more "filler" a text contains,
the less intense its sentiment score should be.

This formalizes the observation from the original SGW manuscript that
"sentiment itself gets diluted as a sentence lengthens" due to neutral
and buffer words.

The dilution factor is: D(B) = exp(-k * B)
Where B = buffer_count / total_count and k is a tunable constant.

When B = 0 (all emotion words), D = 1.0 (no dilution).
When B = 1 (all buffer words), D = exp(-k) ≈ 0.05 for k=3.
"""

import numpy as np
from sgw.utils.logger import get_logger

logger = get_logger(__name__)


def compute_buffer_ratio(buffer_count: int, total_count: int, affective_mass: float = 0.0) -> float:
    """Compute the emotion-conditioned buffer ratio for a document.

    Args:
        buffer_count: Number of buffer (function) word tokens.
        total_count: Total number of tokens.
        affective_mass: Sum of absolute normalized valences (A).

    Returns:
        Emotion-conditioned buffer ratio B* in [0, 1].
    """
    if total_count <= 0:
        return 0.0
    
    B = float(buffer_count / total_count)
    B_star = B / (1.0 + affective_mass)
    return B_star


def dilution_factor(buffer_ratio: float, k: float) -> float:
    """Compute the dilution factor from the buffer ratio.

    Formula: D(B) = exp(-k * B)

    Args:
        buffer_ratio: Ratio of buffer words to total words, in [0, 1].
        k: Dilution constant. Higher = more dilution per buffer word.

    Returns:
        Dilution factor in (0, 1].
    """
    return float(np.exp(-k * buffer_ratio))


def no_dilution() -> float:
    """Return 1.0 (no dilution). Used for ablation studies."""
    return 1.0
