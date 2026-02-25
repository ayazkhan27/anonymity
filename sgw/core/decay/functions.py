"""Spatial decay functions for positional weighting.

The decay function reduces the contribution of tokens based on their position
in the document. Tokens at the beginning contribute more; tokens at the end
contribute less. This models the psycholinguistic primacy effect.

Reference:
  Ebbinghaus (1885). Serial position effect.
  Asch (1946). Primacy in impression formation.
  Gjergjizi, Tran & Felfernig (2022). PriRec-BiLSTM.
  Press, Smith & Lewis (2022). ALiBi.
"""

import numpy as np
from sgw.utils.logger import get_logger

logger = get_logger(__name__)


def exponential_decay(position: int, total_length: int, lambda_c: float) -> float:
    """Compute exponential decay weight for a token at a given position.

    Formula: w(i) = exp(-lambda * i / L)

    Args:
        position: Token position (0-indexed).
        total_length: Total number of tokens in the document.
        lambda_c: Decay constant. Higher = faster decay.

    Returns:
        Decay weight in (0, 1]. Position 0 always returns 1.0.
    """
    if total_length <= 0 or position <= 0:
        return 1.0

    relative_position = position / total_length
    weight = np.exp(-lambda_c * relative_position)
    return float(weight)


def compute_decay_weights(num_tokens: int, lambda_c: float, pivot_idx: int = -1, eta: float = 0.4) -> np.ndarray:
    """Compute decay weights for all positions in a document.

    If a contrastive pivot exists, applies discourse-aware bidirectional decay:
        w_i = eta * exp(-lambda * i / L)             for i < p
        w_i = (1 - eta) * exp(-lambda * (1 - i/L))   for i >= p
    Otherwise falls back to standard exponential decay:
        w_i = exp(-lambda * i / L)

    Args:
        num_tokens: Number of tokens in the document.
        lambda_c: Decay constant.
        pivot_idx: Token index of a contrastive pivot. -1 if none.
        eta: Weighting parameter for the bidirectionality.

    Returns:
        numpy array of shape (num_tokens,) with decay weights.
    """
    if num_tokens <= 0:
        return np.array([])

    if num_tokens == 1:
        return np.array([1.0])

    positions = np.arange(num_tokens, dtype=np.float64)
    relative_positions = positions / num_tokens

    if 0 <= pivot_idx < num_tokens:
        weights = np.zeros(num_tokens, dtype=np.float64)
        
        pre_mask = positions < pivot_idx
        weights[pre_mask] = eta * np.exp(-lambda_c * relative_positions[pre_mask])
        
        post_mask = positions >= pivot_idx
        weights[post_mask] = (1.0 - eta) * np.exp(-lambda_c * (1.0 - relative_positions[post_mask]))
        return weights
    else:
        # Standard decay
        return np.exp(-lambda_c * relative_positions)


def no_decay(num_tokens: int) -> np.ndarray:
    """Return uniform weights (no decay). Used for ablation studies.

    Args:
        num_tokens: Number of tokens.

    Returns:
        numpy array of ones with shape (num_tokens,).
    """
    return np.ones(num_tokens)
