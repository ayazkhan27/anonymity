"""Structured explanation output for SGW scores.

Every score is decomposable into:
- Token-level contributions (valence × modifier × negation × decay)
- Aggregate statistics (buffer ratio, dilution factor, pivot position)
- Metadata (axis used, parameters)
"""

from dataclasses import dataclass, field, asdict
from typing import List, Optional
import json


@dataclass
class TokenContribution:
    """A single token's contribution to the overall score."""
    token: str
    lemma: str
    position: int
    pos_tag: str
    is_emotion_carrier: bool
    base_valence: float  # V*_i from embedding projection
    modifier_weight: float  # M_i = exp(alpha_i)
    negation_factor: float  # N_i: 1.0 or -(gamma + delta * |V*|)
    decay_weight: float  # w_i from positional decay
    raw_contribution: float  # V* × M × N × w
    is_negated: bool
    negation_trigger: Optional[str] = None  # The word that triggered negation
    modifier_trigger: Optional[str] = None  # The adverb that modified intensity
    is_buffer: bool = False


@dataclass
class SGWExplanation:
    """Full explanation for a single SGW score."""
    text: str
    score: float
    score_before_calibration: float
    score_raw: float  # Pre-clip, pre-calibration energy

    # Decomposition
    token_contributions: List[TokenContribution]
    sum_energy: float  # Σ (V* × M × N × w)
    emotion_token_count: int
    normalization_divisor: float  # sqrt(1 + E_count)
    normalized_energy: float  # sum_energy / normalization_divisor
    dilution_factor: float
    diluted_energy: float  # normalized_energy × dilution

    # Context
    total_tokens: int
    buffer_count: int
    buffer_ratio: float
    affective_mass: float  # A = Σ|V*|
    effective_buffer_ratio: float  # B* = B / (1 + A)

    # Discourse
    pivot_detected: bool
    pivot_token: Optional[str] = None
    pivot_position: Optional[int] = None
    eta: float = 0.4  # Pre-pivot weight proportion

    # Parameters used
    lambda_c: float = 0.0
    k: float = 0.0
    axis_name: str = "default"

    # Calibration
    mu_s: float = 0.0
    sigma_s: float = 1.0
    sigma_y: float = 1.0

    def top_contributors(self, n: int = 5, direction: str = "both") -> List[TokenContribution]:
        """Get the top N contributing tokens."""
        emotion_tokens = [t for t in self.token_contributions if t.is_emotion_carrier]
        if direction == "positive":
            sorted_tokens = sorted(emotion_tokens, key=lambda t: t.raw_contribution, reverse=True)
        elif direction == "negative":
            sorted_tokens = sorted(emotion_tokens, key=lambda t: t.raw_contribution)
        else:
            sorted_tokens = sorted(emotion_tokens, key=lambda t: abs(t.raw_contribution), reverse=True)
        return sorted_tokens[:n]

    def to_dict(self) -> dict:
        """Serialize to dict for JSON output."""
        return asdict(self)

    def to_json(self, indent: int = 2) -> str:
        """Serialize to JSON string."""
        return json.dumps(self.to_dict(), indent=indent, default=str)

    def summary(self) -> dict:
        """Compact summary for API responses."""
        top_pos = self.top_contributors(3, "positive")
        top_neg = self.top_contributors(3, "negative")
        return {
            "score": self.score,
            "axis": self.axis_name,
            "emotion_tokens": self.emotion_token_count,
            "total_tokens": self.total_tokens,
            "buffer_ratio": round(self.effective_buffer_ratio, 3),
            "dilution": round(self.dilution_factor, 3),
            "pivot": self.pivot_token,
            "top_positive": [
                {"token": t.token, "contribution": round(t.raw_contribution, 4),
                 "position": t.position}
                for t in top_pos if t.raw_contribution > 0
            ],
            "top_negative": [
                {"token": t.token, "contribution": round(t.raw_contribution, 4),
                 "position": t.position}
                for t in top_neg if t.raw_contribution < 0
            ],
        }
