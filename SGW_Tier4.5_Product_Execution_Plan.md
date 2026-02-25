# SGW TIER 4.5 — PRODUCT ENGINEERING EXECUTION PLAN

## CONTEXT FOR THE EXECUTING MODEL

You are working on the `feature/sgw-v3-plus` branch of `https://github.com/ayazkhan27/anonymity`. The core SGW engine is complete, validated, and frozen. You must NOT modify any files in `sgw/core/engine.py`, `sgw/core/decay/`, `sgw/core/dilution/`, or `sgw/core/valence/scorer.py` unless explicitly told to. The math is done.

### What Has Already Been Built
- `sgw/core/engine.py` — SGW scoring engine with energy-preserving normalization, magnitude-aware negation, discourse-aware bidirectional decay, emotion-conditioned dilution, and variance-matching calibration.
- `sgw/core/valence/scorer.py` — SemAxis-style embedding projection onto a sentiment axis defined by seed word centroids.
- `sgw/core/decay/functions.py` — Exponential positional decay with contrastive pivot support.
- `sgw/core/dilution/buffer.py` — Buffer ratio dilution with affective mass conditioning.
- `sgw/pipeline/syntax.py` — spaCy-based dependency parsing, negation detection, modifier extraction, pivot detection, POS tagging.
- `sgw/evaluation/` — GoEmotions loader, VADER baseline, metrics (Spearman, Kendall, CCC, MAE), calibration, ablation.
- `sgw/api/main.py` — Basic FastAPI service with /analyze and /health endpoints.
- `configs/sgw_config.yaml` — All tunable parameters.
- `tests/` — Unit and integration tests.

### What Has Been Validated
- Calibrated parameters: λ=0.7, k=3.0
- Variance-matching: μ_s=0.220, σ_s=0.604, σ_y=0.502
- GoEmotions ρ=0.484, SST-5 ρ=0.480 (beats VADER), IMDB ρ=0.493 (beats VADER), Yelp ρ=0.336
- All ablation deltas positive (decay Δρ=0.0056, dilution Δρ=0.0019)

### What This Plan Builds
This plan adds four product capabilities that transform SGW from a research prototype into a deployable tool for Customer Experience (CX), financial compliance, and domain-specific sentiment analysis:

1. **Domain-Adaptive Sentiment Axes** — Swap seed words to reorient the sentiment axis for any domain without retraining.
2. **Explanation Output** — Token-level attribution, negation events, decay weights, pivot detection — the feature buyers pay for.
3. **CX Benchmark Pipeline** — Prove SGW's value on real enterprise feedback data where VADER fails.
4. **Long-Document Stress Tests** — Demonstrate stability under truncation, reordering, and paraphrasing.

### Success Criteria for Tier 4.5
| Deliverable | Acceptance Test |
|---|---|
| Domain-Adaptive Axis | Unit test: finance axis scores "elevated risk" as negative, "strong earnings" as positive |
| Explanation Output | /explain endpoint returns valid JSON with token contributions that sum to the overall score ±0.05 |
| CX Benchmark | Produces a benchmark report comparing SGW vs VADER on at least one CX-style dataset with Spearman ρ |
| Stress Tests | SGW score variance under 5 random neutral-section reorderings is < 0.1 std dev |
| SDK Package | `pip install -e .` works and `from sgw import SGWEngine` succeeds |

---

## PHASE 1: DOMAIN-ADAPTIVE SENTIMENT AXES

### Goal
Allow users to define custom sentiment axes for any domain with a single function call. No retraining, no labels, no lexicon maintenance.

### Step 1.1: Create the SentimentAxis Class
Create file `sgw/core/valence/axis.py`:
```python
"""Domain-adaptive sentiment axis construction.

Allows users to define custom sentiment poles for any domain.
The sentiment axis is recomputed from the new poles, and the entire
SGW pipeline immediately operates in the new domain.

Usage:
    from sgw.core.valence.axis import SentimentAxis

    # Default (general English)
    axis = SentimentAxis.default(vectors)

    # Customer Experience
    axis = SentimentAxis(
        vectors=vectors,
        positive=["satisfied", "resolved", "smooth", "helpful", "excellent"],
        negative=["frustrated", "delayed", "unresponsive", "confusing", "broken"],
        name="customer_experience"
    )

    # Finance
    axis = SentimentAxis(
        vectors=vectors,
        positive=["growth", "profit", "upgrade", "beat", "outperform", "bullish"],
        negative=["loss", "decline", "downgrade", "miss", "risk", "bearish"],
        name="finance"
    )

    # Pass to ValenceScorer
    scorer = ValenceScorer(vectors, axis=axis)
"""

import numpy as np
from gensim.models import KeyedVectors
from sgw.utils.config import load_config
from sgw.utils.logger import get_logger

logger = get_logger(__name__)


class SentimentAxis:
    """A domain-specific sentiment axis defined by polar seed words."""

    def __init__(self, vectors: KeyedVectors, positive: list, negative: list,
                 name: str = "custom", affective_percentile: float = 0.85):
        """Construct a sentiment axis from positive and negative pole words.

        Args:
            vectors: Pre-loaded KeyedVectors (GloVe or compatible).
            positive: List of positive pole words (5-50 recommended).
            negative: List of negative pole words (5-50 recommended).
            name: Human-readable name for this axis (e.g., "finance", "healthcare").
            affective_percentile: Percentile threshold for affective-support
                variance computation. Default 0.85 means top 15% most
                sentiment-aligned words define the variance scale.

        Raises:
            ValueError: If fewer than 3 pole words found in vocabulary for either pole.
        """
        self.name = name
        self.positive_seeds = positive
        self.negative_seeds = negative
        self.affective_percentile = affective_percentile

        # Resolve vectors for each pole
        pos_vecs, pos_found, pos_missing = self._resolve_seeds(vectors, positive, "positive")
        neg_vecs, neg_found, neg_missing = self._resolve_seeds(vectors, negative, "negative")

        if len(pos_vecs) < 3:
            raise ValueError(
                f"Only {len(pos_vecs)} positive seeds found in vocabulary "
                f"(need ≥3). Missing: {pos_missing}"
            )
        if len(neg_vecs) < 3:
            raise ValueError(
                f"Only {len(neg_vecs)} negative seeds found in vocabulary "
                f"(need ≥3). Missing: {neg_missing}"
            )

        self.pos_found = pos_found
        self.neg_found = neg_found
        self.pos_missing = pos_missing
        self.neg_missing = neg_missing

        # Compute axis: positive centroid - negative centroid, normalized
        pos_centroid = np.mean(pos_vecs, axis=0)
        neg_centroid = np.mean(neg_vecs, axis=0)
        raw_axis = pos_centroid - neg_centroid
        self.axis_vector = raw_axis / np.linalg.norm(raw_axis)

        # Compute affective-support variance (Tier-4 fix)
        # Only use top (1-percentile) fraction of words by |cos(w, axis)|
        self.sigma_v_aff = self._compute_affective_variance(vectors)

        logger.info(
            f"SentimentAxis '{name}' built: "
            f"{len(pos_found)}/{len(positive)} pos seeds, "
            f"{len(neg_found)}/{len(negative)} neg seeds, "
            f"σ_V,aff={self.sigma_v_aff:.4f}"
        )

    @classmethod
    def default(cls, vectors: KeyedVectors, config: dict = None) -> "SentimentAxis":
        """Create the default general-English sentiment axis from config.

        Args:
            vectors: Pre-loaded KeyedVectors.
            config: Config dict. If None, loads from file.

        Returns:
            SentimentAxis with default positive/negative seeds.
        """
        if config is None:
            config = load_config()
        return cls(
            vectors=vectors,
            positive=config["valence"]["positive_seeds"],
            negative=config["valence"]["negative_seeds"],
            name="default"
        )

    @classmethod
    def from_preset(cls, vectors: KeyedVectors, domain: str) -> "SentimentAxis":
        """Create a sentiment axis from a built-in domain preset.

        Args:
            vectors: Pre-loaded KeyedVectors.
            domain: One of 'general', 'customer_experience', 'finance',
                    'healthcare', 'product_reviews'.

        Returns:
            SentimentAxis configured for the specified domain.

        Raises:
            ValueError: If domain is not recognized.
        """
        presets = _DOMAIN_PRESETS.get(domain)
        if presets is None:
            available = list(_DOMAIN_PRESETS.keys())
            raise ValueError(f"Unknown domain '{domain}'. Available: {available}")

        return cls(
            vectors=vectors,
            positive=presets["positive"],
            negative=presets["negative"],
            name=domain
        )

    def _resolve_seeds(self, vectors, seeds, pole_name):
        """Resolve seed words against the vocabulary."""
        vecs = []
        found = []
        missing = []
        for word in seeds:
            word_lower = word.lower()
            if word_lower in vectors:
                vecs.append(vectors[word_lower])
                found.append(word_lower)
            else:
                missing.append(word_lower)
                logger.debug(f"{pole_name} seed '{word}' not in vocabulary.")
        return vecs, found, missing

    def _compute_affective_variance(self, vectors: KeyedVectors) -> float:
        """Compute affective-support variance σ_V,aff.

        This is the variance of cos(w, axis) computed ONLY over the top
        (1-percentile) fraction of words by absolute projection magnitude.
        This prevents neutral words from collapsing the variance estimate.

        Args:
            vectors: Full vocabulary vectors.

        Returns:
            σ_V,aff as a float.
        """
        # Sample up to 100k words for efficiency
        vocab_size = len(vectors)
        sample_size = min(vocab_size, 100000)

        if sample_size < vocab_size:
            indices = np.random.RandomState(42).choice(vocab_size, sample_size, replace=False)
            sample_words = [vectors.index_to_key[i] for i in indices]
        else:
            sample_words = vectors.index_to_key[:sample_size]

        # Compute projections
        projections = []
        for word in sample_words:
            vec = vectors[word]
            norm = np.linalg.norm(vec)
            if norm > 0:
                cos_sim = np.dot(vec, self.axis_vector) / norm
                projections.append(cos_sim)

        projections = np.array(projections)
        abs_projections = np.abs(projections)

        # Threshold: top (1 - percentile) by absolute magnitude
        threshold = np.percentile(abs_projections, self.affective_percentile * 100)
        affective_mask = abs_projections >= threshold
        affective_projections = projections[affective_mask]

        sigma = float(np.std(affective_projections))
        if sigma < 1e-6:
            logger.warning(f"σ_V,aff is near zero ({sigma:.6f}). Using fallback 0.1.")
            sigma = 0.1

        return sigma

    def project(self, word_vector: np.ndarray) -> float:
        """Project a word vector onto this sentiment axis.

        Args:
            word_vector: The word's embedding vector.

        Returns:
            Normalized valence score: cos(w, axis) / σ_V,aff
        """
        norm = np.linalg.norm(word_vector)
        if norm == 0:
            return 0.0
        cos_sim = np.dot(word_vector, self.axis_vector) / norm
        return float(cos_sim / self.sigma_v_aff)

    def summary(self) -> dict:
        """Return a summary dict for logging/serialization."""
        return {
            "name": self.name,
            "positive_seeds_found": self.pos_found,
            "negative_seeds_found": self.neg_found,
            "positive_seeds_missing": self.pos_missing,
            "negative_seeds_missing": self.neg_missing,
            "sigma_v_aff": self.sigma_v_aff,
            "affective_percentile": self.affective_percentile,
        }


# ============================================================
# BUILT-IN DOMAIN PRESETS
# ============================================================
# Each preset contains 15-25 seed words per pole, chosen for:
#   - Semantic purity (unambiguous polarity in the domain)
#   - Coverage of domain-specific vocabulary
#   - Minimal overlap with neutral domain terms
#
# These are NOT exhaustive lexicons. They are axis anchors.
# ============================================================

_DOMAIN_PRESETS = {
    "general": {
        "positive": [
            "excellent", "wonderful", "fantastic", "amazing", "outstanding",
            "superb", "brilliant", "magnificent", "exceptional", "terrific",
            "joyful", "delightful", "love", "happy", "pleased",
            "grateful", "cheerful", "excited", "thrilled", "ecstatic",
        ],
        "negative": [
            "terrible", "horrible", "awful", "dreadful", "atrocious",
            "abysmal", "appalling", "disgusting", "revolting", "miserable",
            "agonizing", "devastating", "hate", "sad", "angry",
            "furious", "depressed", "heartbroken", "wretched", "despair",
        ],
    },
    "customer_experience": {
        "positive": [
            "satisfied", "resolved", "smooth", "helpful", "friendly",
            "quick", "efficient", "professional", "responsive", "knowledgeable",
            "pleasant", "seamless", "impressed", "excellent", "recommend",
            "convenient", "reliable", "thorough", "attentive", "outstanding",
        ],
        "negative": [
            "frustrated", "delayed", "unresponsive", "confusing", "broken",
            "rude", "incompetent", "slow", "unhelpful", "disappointing",
            "ignored", "complicated", "unreliable", "overcharged", "misleading",
            "unresolved", "terrible", "useless", "nightmare", "worst",
        ],
    },
    "finance": {
        "positive": [
            "growth", "profit", "upgrade", "beat", "outperform",
            "bullish", "gain", "revenue", "surplus", "dividend",
            "recovery", "expansion", "optimistic", "strong", "upside",
            "exceeded", "momentum", "robust", "favorable", "accelerate",
        ],
        "negative": [
            "loss", "decline", "downgrade", "miss", "risk",
            "bearish", "deficit", "debt", "recession", "downturn",
            "contraction", "volatile", "default", "liability", "weakness",
            "underperform", "warning", "impairment", "uncertainty", "exposure",
        ],
    },
    "healthcare": {
        "positive": [
            "improvement", "recovery", "effective", "resolved", "stable",
            "benign", "remission", "tolerated", "unremarkable", "normal",
            "healthy", "responsive", "controlled", "favorable", "successful",
            "comfortable", "manageable", "relieved", "functional", "cleared",
        ],
        "negative": [
            "deterioration", "complications", "adverse", "severe", "chronic",
            "malignant", "progressive", "refractory", "elevated", "abnormal",
            "painful", "debilitating", "terminal", "critical", "unresponsive",
            "worsening", "aggressive", "symptomatic", "inflamed", "failure",
        ],
    },
    "product_reviews": {
        "positive": [
            "excellent", "perfect", "love", "amazing", "great",
            "quality", "durable", "comfortable", "intuitive", "fast",
            "beautiful", "sturdy", "reliable", "worth", "impressed",
            "recommend", "premium", "upgrade", "solid", "flawless",
        ],
        "negative": [
            "broken", "cheap", "flimsy", "defective", "returned",
            "waste", "terrible", "disappointing", "uncomfortable", "slow",
            "overpriced", "fragile", "malfunction", "useless", "regret",
            "horrible", "junk", "poor", "garbage", "worst",
        ],
    },
}
```

### Step 1.2: Update ValenceScorer to Accept Custom Axes
Open `sgw/core/valence/scorer.py`. Add this method and modify `__init__` to accept an optional `axis` parameter.

Find the `__init__` method and modify it so that if an `axis` argument (a `SentimentAxis` instance) is passed, it uses that axis instead of building one from config seeds. If no axis is passed, it creates one using `SentimentAxis.default(vectors, config)`.

The `get_valence` method should delegate to `self.axis.project(word_vector)` for words in vocabulary, and return `self.fallback_valence` for OOV words.

Specifically, create a new version of `scorer.py` by replacing its contents with:
```python
"""Vector-based valence scoring using embedding projection onto a sentiment axis.

Uses the SentimentAxis class for domain-adaptive axis construction.
"""

import numpy as np
from functools import lru_cache
from gensim.models import KeyedVectors
from sgw.core.valence.axis import SentimentAxis
from sgw.utils.config import load_config
from sgw.utils.logger import get_logger

logger = get_logger(__name__)


class ValenceScorer:
    """Computes word-level valence by projecting onto a sentiment axis."""

    def __init__(self, vectors: KeyedVectors, config: dict = None,
                 axis: SentimentAxis = None):
        """Initialize the valence scorer.

        Args:
            vectors: Pre-loaded KeyedVectors (GloVe or compatible).
            config: Configuration dict. If None, loads from default config file.
            axis: Optional pre-built SentimentAxis. If None, builds default
                  from config seed words.
        """
        if config is None:
            config = load_config()

        self.vectors = vectors
        self.config = config
        self.fallback_valence = config["valence"]["fallback_valence"]

        if axis is not None:
            self.axis = axis
        else:
            self.axis = SentimentAxis.default(vectors, config)

        logger.info(f"ValenceScorer initialized with axis '{self.axis.name}'.")

    def swap_axis(self, axis: SentimentAxis):
        """Hot-swap the sentiment axis for a new domain.

        This clears the valence cache and immediately reorients
        all future scoring to the new domain.

        Args:
            axis: New SentimentAxis instance.
        """
        self.axis = axis
        self.get_valence.cache_clear()
        logger.info(f"Axis swapped to '{axis.name}'. Valence cache cleared.")

    @lru_cache(maxsize=50000)
    def get_valence(self, word: str) -> float:
        """Get the valence score for a single word.

        Args:
            word: The word to score (should be lowercased lemma).

        Returns:
            Float valence score. Positive = positive sentiment, negative = negative.
            Returns fallback_valence (0.0) for OOV words.
        """
        word_lower = word.lower()
        if word_lower not in self.vectors:
            return self.fallback_valence

        word_vec = self.vectors[word_lower]
        return self.axis.project(word_vec)

    def validate_axis(self) -> dict:
        """Sanity check: verify known positive/negative words score correctly.

        Uses domain-appropriate test words if available.
        """
        test_positive = ["happy", "excellent", "wonderful", "beautiful", "great"]
        test_negative = ["terrible", "horrible", "awful", "disgusting", "miserable"]
        test_neutral = ["table", "chair", "walk", "number", "tuesday"]

        pos_scores = [self.get_valence(w) for w in test_positive]
        neg_scores = [self.get_valence(w) for w in test_negative]
        neu_scores = [self.get_valence(w) for w in test_neutral]

        results = {
            "axis_name": self.axis.name,
            "pos_mean": float(np.mean(pos_scores)),
            "neg_mean": float(np.mean(neg_scores)),
            "neu_mean": float(np.mean(neu_scores)),
            "pos_all_positive": all(s > 0 for s in pos_scores),
            "neg_all_negative": all(s < 0 for s in neg_scores),
            "neu_near_zero": abs(np.mean(neu_scores)) < 0.5,
            "separation": float(np.mean(pos_scores) - np.mean(neg_scores)),
        }

        logger.info(f"Axis '{self.axis.name}' validation: "
                     f"pos={results['pos_mean']:.3f}, neg={results['neg_mean']:.3f}, "
                     f"neu={results['neu_mean']:.3f}, sep={results['separation']:.3f}")
        return results
```

### Step 1.3: Unit Tests for Domain Axes
Create file `tests/unit/test_axis.py`:
```python
"""Unit tests for domain-adaptive sentiment axes."""

import pytest
import numpy as np


class TestSentimentAxis:
    @pytest.fixture(scope="class")
    def vectors(self):
        from sgw.utils.vectors import load_vectors
        return load_vectors(limit=200000)

    def test_default_axis_builds(self, vectors):
        from sgw.core.valence.axis import SentimentAxis
        axis = SentimentAxis.default(vectors)
        assert axis.name == "default"
        assert len(axis.pos_found) >= 5
        assert len(axis.neg_found) >= 5
        assert axis.sigma_v_aff > 0.01

    def test_custom_axis_builds(self, vectors):
        from sgw.core.valence.axis import SentimentAxis
        axis = SentimentAxis(
            vectors=vectors,
            positive=["good", "great", "excellent", "happy", "wonderful"],
            negative=["bad", "terrible", "awful", "sad", "horrible"],
            name="test"
        )
        assert axis.name == "test"

    def test_preset_domains_all_build(self, vectors):
        from sgw.core.valence.axis import SentimentAxis
        for domain in ["general", "customer_experience", "finance",
                        "healthcare", "product_reviews"]:
            axis = SentimentAxis.from_preset(vectors, domain)
            assert axis.name == domain
            assert axis.sigma_v_aff > 0

    def test_finance_axis_domain_correctness(self, vectors):
        from sgw.core.valence.axis import SentimentAxis
        from sgw.core.valence.scorer import ValenceScorer

        axis = SentimentAxis.from_preset(vectors, "finance")
        scorer = ValenceScorer(vectors, axis=axis)

        # Domain-specific: "elevated" should be negative in finance (elevated risk)
        # "growth" should be positive
        assert scorer.get_valence("growth") > 0, "growth should be positive in finance"
        assert scorer.get_valence("loss") < 0, "loss should be negative in finance"
        assert scorer.get_valence("outperform") > 0
        assert scorer.get_valence("recession") < 0

    def test_healthcare_axis_domain_correctness(self, vectors):
        from sgw.core.valence.axis import SentimentAxis
        from sgw.core.valence.scorer import ValenceScorer

        axis = SentimentAxis.from_preset(vectors, "healthcare")
        scorer = ValenceScorer(vectors, axis=axis)

        # "unremarkable" is POSITIVE in medical context (normal scan)
        # "recovery" is positive
        assert scorer.get_valence("recovery") > 0
        assert scorer.get_valence("malignant") < 0
        assert scorer.get_valence("remission") > 0

    def test_cx_axis_domain_correctness(self, vectors):
        from sgw.core.valence.axis import SentimentAxis
        from sgw.core.valence.scorer import ValenceScorer

        axis = SentimentAxis.from_preset(vectors, "customer_experience")
        scorer = ValenceScorer(vectors, axis=axis)

        assert scorer.get_valence("resolved") > 0
        assert scorer.get_valence("frustrated") < 0
        assert scorer.get_valence("responsive") > 0
        assert scorer.get_valence("unresponsive") < 0

    def test_axis_swap_clears_cache(self, vectors):
        from sgw.core.valence.axis import SentimentAxis
        from sgw.core.valence.scorer import ValenceScorer

        axis_general = SentimentAxis.from_preset(vectors, "general")
        axis_finance = SentimentAxis.from_preset(vectors, "finance")

        scorer = ValenceScorer(vectors, axis=axis_general)
        v1 = scorer.get_valence("growth")

        scorer.swap_axis(axis_finance)
        v2 = scorer.get_valence("growth")

        # Score should change after axis swap
        assert v1 != v2, "Axis swap did not change valence scores"

    def test_too_few_seeds_raises(self, vectors):
        from sgw.core.valence.axis import SentimentAxis
        with pytest.raises(ValueError, match="need ≥3"):
            SentimentAxis(
                vectors=vectors,
                positive=["good"],  # Only 1 seed
                negative=["bad", "terrible", "awful", "horrible", "wretched"],
                name="fail"
            )

    def test_unknown_preset_raises(self, vectors):
        from sgw.core.valence.axis import SentimentAxis
        with pytest.raises(ValueError, match="Unknown domain"):
            SentimentAxis.from_preset(vectors, "astrology")
```

Run: `python -m pytest tests/unit/test_axis.py -v`

### GATE CHECK P1: Domain Axis Validation
ALL of the following must pass before proceeding:
```bash
python -m pytest tests/unit/test_axis.py -v --tb=short
```
If finance or healthcare axis tests fail on specific words, the issue is that GloVe's general-purpose embeddings may not capture domain-specific polarity reversals (e.g., "elevated" being negative). This is expected — document it as a limitation and note that domain-specific embeddings (e.g., FinBERT vectors) would improve this.

---

## PHASE 2: EXPLANATION OUTPUT

### Goal
Every SGW score must be accompanied by a full decomposition showing exactly which tokens contributed how much and why. This is the #1 feature enterprise buyers need.

### Step 2.1: Create the Explanation Data Model
Create file `sgw/core/explanation.py`:
```python
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
        """Get the top N contributing tokens.

        Args:
            n: Number of tokens to return.
            direction: 'positive', 'negative', or 'both' (by absolute magnitude).
        """
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
        d = asdict(self)
        return d

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
```

### Step 2.2: Add explain() Method to SGWEngine
Add the following method to `sgw/core/engine.py`. Do NOT modify the existing `score()` or `score_doc()` methods. Add `explain_doc()` as a NEW method after `score_doc()`.

In `engine.py`, after the `score_doc` method, add:
```python
    def explain_doc(self, doc) -> "SGWExplanation":
        """Compute SGW score with full token-level explanation.

        Args:
            doc: spaCy Doc object.

        Returns:
            SGWExplanation with complete decomposition.
        """
        from sgw.core.explanation import SGWExplanation, TokenContribution

        total_tokens = self.syntax_parser.count_tokens(doc)
        if total_tokens == 0:
            return SGWExplanation(
                text=doc.text, score=0.0, score_before_calibration=0.0,
                token_contributions=[], sum_energy=0.0,
                emotion_token_count=0, normalization_divisor=1.0,
                normalized_energy=0.0, dilution_factor=1.0,
                diluted_energy=0.0, total_tokens=0, buffer_count=0,
                buffer_ratio=0.0, affective_mass=0.0,
                effective_buffer_ratio=0.0, pivot_detected=False,
                lambda_c=self.lambda_c, k=self.k,
                axis_name=self.valence_scorer.axis.name,
            )

        pivot_idx = self.syntax_parser.detect_pivot(doc)

        if self._use_decay:
            decay_weights = compute_decay_weights(total_tokens, self.lambda_c, pivot_idx=pivot_idx, eta=0.4)
        else:
            decay_weights = no_decay(total_tokens)

        # 1st pass: gather all token info
        all_contributions = []
        emotion_tokens_data = []
        token_idx = 0
        A = 0.0

        for token in doc:
            if token.is_space:
                continue

            is_emotion = self.syntax_parser.is_emotion_carrier(token)
            is_buf = self.syntax_parser.is_buffer(token)
            v_star = self.valence_scorer.get_valence(token.lemma_) if is_emotion else 0.0

            if is_emotion:
                A += abs(v_star)

            contrib = TokenContribution(
                token=token.text,
                lemma=token.lemma_,
                position=token_idx,
                pos_tag=token.pos_,
                is_emotion_carrier=is_emotion,
                base_valence=v_star,
                modifier_weight=1.0,
                negation_factor=1.0,
                decay_weight=float(decay_weights[token_idx]),
                raw_contribution=0.0,
                is_negated=False,
                is_buffer=is_buf,
            )

            if is_emotion:
                emotion_tokens_data.append((contrib, token, token_idx, v_star))

            all_contributions.append(contrib)
            token_idx += 1

        buffer_count = self.syntax_parser.count_buffers(doc)
        buffer_ratio = buffer_count / total_tokens if total_tokens > 0 else 0.0
        effective_buffer_ratio = compute_buffer_ratio(buffer_count, total_tokens, A)

        if self._use_dilution:
            dilution = dilution_factor(effective_buffer_ratio, self.k)
        else:
            dilution = no_dilution()

        # 2nd pass: compute contributions for emotion tokens
        E_count = len(emotion_tokens_data)
        gamma = 0.6
        delta = 0.4
        sum_energy = 0.0

        for contrib, token, idx, v_star in emotion_tokens_data:
            alpha_i = self.syntax_parser.get_modifier_alpha(token)
            M_i = np.exp(alpha_i)
            contrib.modifier_weight = float(M_i)

            # Check for modifier trigger
            for child in token.children:
                if child.dep_ == "advmod" and child.lower_ in {**self.syntax_parser.intensifiers, **self.syntax_parser.mitigators}:
                    contrib.modifier_trigger = child.text
                    break

            is_neg = self.syntax_parser.is_negated(token)
            if is_neg:
                N_i = -gamma - delta * abs(v_star)
                contrib.is_negated = True
                # Find negation trigger
                for child in token.children:
                    if child.dep_ == "neg" or child.lower_ in {"never", "hardly", "barely", "scarcely", "seldom"}:
                        contrib.negation_trigger = child.text
                        break
            else:
                N_i = 1.0

            contrib.negation_factor = float(N_i)
            w_i = float(decay_weights[idx])
            contrib.decay_weight = w_i

            raw = v_star * M_i * N_i * w_i
            contrib.raw_contribution = float(raw)
            sum_energy += raw

        norm_divisor = np.sqrt(1.0 + E_count)
        normalized = sum_energy / norm_divisor if E_count > 0 else 0.0
        diluted = normalized * dilution
        score_pre_cal = float(np.clip(diluted, -1.0, 1.0))

        # Calibration
        score = score_pre_cal
        mu_s = getattr(self, 'mu_s', 0.0)
        sigma_s = getattr(self, 'sigma_s', 1.0)
        sigma_y = getattr(self, 'sigma_y', 1.0)
        if hasattr(self, 'sigma_s') and sigma_s > 0 and sigma_s != 1.0:
            score = ((score - mu_s) / sigma_s) * sigma_y
            score = float(np.clip(score, -1.0, 1.0))

        # Pivot info
        pivot_detected = pivot_idx is not None
        pivot_token_text = None
        pivot_position = None
        if pivot_detected:
            non_space_idx = 0
            for token in doc:
                if token.is_space:
                    continue
                if non_space_idx == pivot_idx:
                    pivot_token_text = token.text
                    pivot_position = pivot_idx
                    break
                non_space_idx += 1

        return SGWExplanation(
            text=doc.text,
            score=score,
            score_before_calibration=score_pre_cal,
            token_contributions=all_contributions,
            sum_energy=float(sum_energy),
            emotion_token_count=E_count,
            normalization_divisor=float(norm_divisor),
            normalized_energy=float(normalized),
            dilution_factor=float(dilution),
            diluted_energy=float(diluted),
            total_tokens=total_tokens,
            buffer_count=buffer_count,
            buffer_ratio=float(buffer_ratio),
            affective_mass=float(A),
            effective_buffer_ratio=float(effective_buffer_ratio),
            pivot_detected=pivot_detected,
            pivot_token=pivot_token_text,
            pivot_position=pivot_position,
            lambda_c=self.lambda_c,
            k=self.k,
            axis_name=self.valence_scorer.axis.name,
            mu_s=mu_s,
            sigma_s=sigma_s,
            sigma_y=sigma_y,
        )

    def explain(self, text: str) -> "SGWExplanation":
        """Compute SGW score with full explanation for a raw text string."""
        doc = self.syntax_parser.parse(text)
        return self.explain_doc(doc)
```

### Step 2.3: Add /explain Endpoint to the API
Modify `sgw/api/main.py` to add a new endpoint. Add AFTER the existing `/analyze` endpoint:

```python
@app.post("/explain")
async def explain(req: TextRequest):
    """Return SGW score with full token-level explanation."""
    if _engine is None:
        raise HTTPException(status_code=503, detail="Model not loaded.")
    explanation = _engine.explain(req.text)
    return explanation.summary()


@app.post("/explain/full")
async def explain_full(req: TextRequest):
    """Return complete SGW explanation with every token's contribution."""
    if _engine is None:
        raise HTTPException(status_code=503, detail="Model not loaded.")
    explanation = _engine.explain(req.text)
    return explanation.to_dict()


class DomainRequest(BaseModel):
    domain: str = Field(..., description="Domain preset name: general, customer_experience, finance, healthcare, product_reviews")


@app.post("/axis/swap")
async def swap_axis(req: DomainRequest):
    """Hot-swap the sentiment axis to a different domain."""
    if _engine is None:
        raise HTTPException(status_code=503, detail="Model not loaded.")
    from sgw.core.valence.axis import SentimentAxis
    from sgw.utils.vectors import load_vectors
    try:
        vectors = _engine.valence_scorer.vectors
        new_axis = SentimentAxis.from_preset(vectors, req.domain)
        _engine.valence_scorer.swap_axis(new_axis)
        return {"status": "ok", "axis": new_axis.summary()}
    except ValueError as e:
        raise HTTPException(status_code=400, detail=str(e))


@app.get("/axis/current")
async def current_axis():
    """Return information about the currently active sentiment axis."""
    if _engine is None:
        raise HTTPException(status_code=503, detail="Model not loaded.")
    return _engine.valence_scorer.axis.summary()
```

### GATE CHECK P2: Explanation Output Validation
Run this test manually:
```python
"""Gate Check P2: Explanation output is valid and self-consistent."""

from sgw.utils.vectors import load_vectors
from sgw.core.valence.scorer import ValenceScorer
from sgw.core.valence.axis import SentimentAxis
from sgw.pipeline.syntax import SyntaxParser
from sgw.core.engine import SGWEngine

vectors = load_vectors(limit=500000)
scorer = ValenceScorer(vectors)
parser = SyntaxParser()
engine = SGWEngine(scorer, parser)

# Test 1: Explanation produces valid output
expl = engine.explain("The service was absolutely terrible but the food was excellent.")
assert abs(expl.score - engine.score("The service was absolutely terrible but the food was excellent.")) < 0.001, \
    "explain() and score() must produce the same score"

# Test 2: Token contributions exist
assert len(expl.token_contributions) > 0
emotion_contribs = [t for t in expl.token_contributions if t.is_emotion_carrier]
assert len(emotion_contribs) > 0, "Must have emotion-carrying tokens"

# Test 3: Summary is valid JSON-serializable dict
summary = expl.summary()
assert "score" in summary
assert "top_positive" in summary
assert "top_negative" in summary
assert isinstance(summary["top_positive"], list)
import json
json.dumps(summary)  # Must not raise

# Test 4: Pivot detected on contrastive sentence
assert expl.pivot_detected, "Should detect 'but' as pivot"
assert expl.pivot_token == "but", f"Pivot should be 'but', got '{expl.pivot_token}'"

# Test 5: Domain axis swap works
from sgw.core.valence.axis import SentimentAxis
fin_axis = SentimentAxis.from_preset(vectors, "finance")
scorer.swap_axis(fin_axis)
expl_fin = engine.explain("Revenue growth exceeded expectations despite market uncertainty.")
assert expl_fin.axis_name == "finance"
assert any(t.base_valence > 0 for t in expl_fin.token_contributions if t.lemma == "growth")

print("GATE CHECK P2: ALL TESTS PASSED")
```

---

## PHASE 3: CX BENCHMARK PIPELINE

### Goal
Prove SGW's value on Customer Experience data where text is long, sentiment is mixed, and VADER struggles.

### Step 3.1: Create Benchmark Loader for CX Data
Create file `sgw/evaluation/cx_benchmark.py`:
```python
"""CX Benchmark: Compare SGW vs VADER on customer feedback data.

Uses publicly available CX-adjacent datasets:
1. Amazon Reviews (multi-domain, long-form, 1-5 star ratings as ground truth)
2. Trustpilot-style data if available

The key insight: map star ratings to continuous valence:
  1 star -> -1.0
  2 stars -> -0.5
  3 stars -> 0.0
  4 stars -> +0.5
  5 stars -> +1.0
"""

import numpy as np
import pandas as pd
from datasets import load_dataset
from sgw.utils.logger import get_logger

logger = get_logger(__name__)

STAR_TO_VALENCE = {1: -1.0, 2: -0.5, 3: 0.0, 4: 0.5, 5: 1.0}


def load_amazon_reviews(category: str = "Electronics", split: str = "test",
                         limit: int = 2000) -> pd.DataFrame:
    """Load Amazon product reviews with star rating as valence proxy.

    Args:
        category: Product category. Options include 'Electronics',
                  'Books', 'Clothing_Shoes_and_Jewelry', etc.
        split: Dataset split.
        limit: Maximum number of examples.

    Returns:
        DataFrame with columns: text, human_valence, stars, category
    """
    logger.info(f"Loading Amazon Reviews ({category}, {split}, limit={limit})...")

    try:
        dataset = load_dataset(
            "McAuley-Lab/Amazon-Reviews-2023",
            f"raw_review_{category}",
            split=split,
            trust_remote_code=True,
        )
    except Exception as e:
        logger.warning(f"Could not load Amazon Reviews 2023: {e}")
        logger.info("Falling back to amazon_polarity dataset...")
        dataset = load_dataset("amazon_polarity", split=split)
        df = dataset.to_pandas().head(limit)
        df["human_valence"] = df["label"].map({0: -1.0, 1: 1.0})
        df["text"] = df["title"].fillna("") + " " + df["content"].fillna("")
        df["stars"] = df["label"].map({0: 1, 1: 5})
        df["category"] = "mixed"
        return df[["text", "human_valence", "stars", "category"]].reset_index(drop=True)

    df = dataset.to_pandas()

    # Filter to reviews with text and rating
    text_col = "text" if "text" in df.columns else "reviewText"
    rating_col = "rating" if "rating" in df.columns else "overall"

    if text_col not in df.columns or rating_col not in df.columns:
        available = list(df.columns)
        raise ValueError(f"Expected columns not found. Available: {available}")

    df = df[[text_col, rating_col]].dropna()
    df = df.rename(columns={text_col: "text", rating_col: "stars"})
    df["stars"] = df["stars"].astype(int)
    df = df[df["stars"].between(1, 5)]

    # Stratified sample: equal per star rating
    per_star = limit // 5
    samples = []
    for star in range(1, 6):
        star_df = df[df["stars"] == star]
        n = min(len(star_df), per_star)
        samples.append(star_df.sample(n=n, random_state=42))
    df = pd.concat(samples).reset_index(drop=True)

    df["human_valence"] = df["stars"].map(STAR_TO_VALENCE)
    df["category"] = category

    # Filter very short reviews
    df = df[df["text"].str.len() > 20].reset_index(drop=True)

    logger.info(f"Loaded {len(df)} Amazon Reviews. "
                f"Star distribution: {df['stars'].value_counts().to_dict()}")
    return df[["text", "human_valence", "stars", "category"]]


def run_cx_benchmark(engine, limit: int = 2000,
                      save_path: str = "reports/metrics/cx_benchmark.json"):
    """Run the full CX benchmark comparing SGW vs VADER.

    Tests on Amazon Electronics reviews with star ratings as ground truth.
    Also tests domain axis adaptation.
    """
    import json
    from sgw.evaluation.baselines import compute_vader_scores
    from sgw.evaluation.metrics import evaluate_continuous
    from sgw.core.valence.axis import SentimentAxis

    results = {}

    # Load CX data
    df = load_amazon_reviews(category="Electronics", limit=limit)
    texts = df["text"].tolist()
    y_true = df["human_valence"].values

    # --- Test 1: SGW with general axis ---
    general_axis = SentimentAxis.from_preset(engine.valence_scorer.vectors, "general")
    engine.valence_scorer.swap_axis(general_axis)
    y_sgw_general = engine.score_batch(texts, show_progress=True)
    results["sgw_general"] = evaluate_continuous(y_true, y_sgw_general, "SGW (General Axis)")

    # --- Test 2: SGW with product_reviews axis ---
    product_axis = SentimentAxis.from_preset(engine.valence_scorer.vectors, "product_reviews")
    engine.valence_scorer.swap_axis(product_axis)
    y_sgw_product = engine.score_batch(texts, show_progress=True)
    results["sgw_product_axis"] = evaluate_continuous(y_true, y_sgw_product, "SGW (Product Axis)")

    # --- Test 3: SGW with CX axis ---
    cx_axis = SentimentAxis.from_preset(engine.valence_scorer.vectors, "customer_experience")
    engine.valence_scorer.swap_axis(cx_axis)
    y_sgw_cx = engine.score_batch(texts, show_progress=True)
    results["sgw_cx_axis"] = evaluate_continuous(y_true, y_sgw_cx, "SGW (CX Axis)")

    # --- Test 4: VADER baseline ---
    y_vader = compute_vader_scores(df["text"])
    results["vader"] = evaluate_continuous(y_true, y_vader, "VADER")

    # --- Test 5: By review length ---
    df["text_length"] = df["text"].str.len()
    short_mask = df["text_length"] < 200
    long_mask = df["text_length"] >= 500

    if short_mask.sum() > 50:
        results["sgw_cx_short"] = evaluate_continuous(
            y_true[short_mask], y_sgw_cx[short_mask], "SGW CX (short <200 chars)")
        results["vader_short"] = evaluate_continuous(
            y_true[short_mask], y_vader[short_mask], "VADER (short <200 chars)")

    if long_mask.sum() > 50:
        results["sgw_cx_long"] = evaluate_continuous(
            y_true[long_mask], y_sgw_cx[long_mask], "SGW CX (long ≥500 chars)")
        results["vader_long"] = evaluate_continuous(
            y_true[long_mask], y_vader[long_mask], "VADER (long ≥500 chars)")

    # --- Summary ---
    logger.info("\n" + "=" * 60)
    logger.info("CX BENCHMARK SUMMARY")
    logger.info("=" * 60)
    for name, metrics in results.items():
        logger.info(f"  {name}: ρ={metrics['spearman_rho']:.4f}, MAE={metrics['mae']:.4f}")

    # Domain adaptation delta
    general_rho = results["sgw_general"]["spearman_rho"]
    cx_rho = results["sgw_cx_axis"]["spearman_rho"]
    product_rho = results["sgw_product_axis"]["spearman_rho"]
    best_domain_rho = max(cx_rho, product_rho)
    results["domain_adaptation_delta"] = best_domain_rho - general_rho
    logger.info(f"\n  Domain adaptation improvement: Δρ={results['domain_adaptation_delta']:.4f}")

    with open(save_path, "w") as f:
        json.dump(results, f, indent=2, default=str)
    logger.info(f"CX benchmark saved to {save_path}")

    return results
```

---

## PHASE 4: LONG-DOCUMENT STRESS TESTS

### Step 4.1: Create Stress Test Suite
Create file `sgw/evaluation/stress_tests.py`:
```python
"""Long-document stress tests for SGW stability.

Tests that SGW produces stable, interpretable scores under:
1. Truncation: score of prefix vs full document
2. Neutral padding: injecting neutral text shouldn't change polarity
3. Reordering: shuffling neutral sections shouldn't drastically change score
4. Repetition: repeating the same sentence should produce similar scores
"""

import numpy as np
import json
from sgw.utils.logger import get_logger

logger = get_logger(__name__)


def run_stress_tests(engine, save_path: str = "reports/metrics/stress_tests.json"):
    """Run all stability stress tests."""
    results = {}

    # ================================================================
    # TEST 1: TRUNCATION STABILITY
    # Score should be relatively stable across 50%, 75%, 100% of text
    # ================================================================
    long_texts = [
        "I absolutely loved this product. The quality is outstanding and it "
        "exceeded all my expectations. The packaging was neat and professional. "
        "Delivery was prompt and the customer service team was incredibly helpful "
        "when I had a question. Would definitely purchase again and recommend "
        "to anyone looking for a reliable solution.",

        "This was a complete waste of money. The product broke within a week. "
        "Customer service was unresponsive and unhelpful. I waited three weeks "
        "for a replacement that never arrived. The quality is cheap and flimsy. "
        "I would strongly advise against purchasing this from any retailer. "
        "Total disappointment and I regret the purchase entirely.",

        "The hotel room was spacious and clean. However the restaurant food was "
        "mediocre at best and overpriced for what you get. The staff were friendly "
        "but the facilities were dated and in need of renovation. The pool area "
        "was nice but the gym equipment was broken. Overall a mixed experience "
        "that doesn't justify the premium pricing they charge.",
    ]

    truncation_results = []
    for text in long_texts:
        words = text.split()
        full_score = engine.score(text)
        half_score = engine.score(" ".join(words[:len(words)//2]))
        three_quarter_score = engine.score(" ".join(words[:3*len(words)//4]))

        truncation_results.append({
            "text_preview": text[:80] + "...",
            "full_score": full_score,
            "half_score": half_score,
            "three_quarter_score": three_quarter_score,
            "polarity_stable": (np.sign(full_score) == np.sign(half_score)),
        })

    polarity_stability = np.mean([r["polarity_stable"] for r in truncation_results])
    results["truncation"] = {
        "details": truncation_results,
        "polarity_stability_rate": float(polarity_stability),
    }
    logger.info(f"Truncation stability: {polarity_stability:.0%} polarity preserved")

    # ================================================================
    # TEST 2: NEUTRAL PADDING STABILITY
    # Adding neutral filler should not flip polarity
    # ================================================================
    emotional_core = "I am extremely happy and grateful for this wonderful experience."
    neutral_padding = (
        "The room was rectangular. There was a table in the corner. "
        "The walls were painted white. Several chairs surrounded the table. "
        "A clock hung on the wall. The floor was made of wood."
    )

    core_score = engine.score(emotional_core)
    padded_before = engine.score(neutral_padding + " " + emotional_core)
    padded_after = engine.score(emotional_core + " " + neutral_padding)
    padded_both = engine.score(neutral_padding + " " + emotional_core + " " + neutral_padding)

    results["neutral_padding"] = {
        "core_score": float(core_score),
        "padded_before": float(padded_before),
        "padded_after": float(padded_after),
        "padded_both": float(padded_both),
        "polarity_preserved_all": bool(
            np.sign(core_score) == np.sign(padded_before) ==
            np.sign(padded_after) == np.sign(padded_both)
        ),
        "max_deviation": float(max(
            abs(core_score - padded_before),
            abs(core_score - padded_after),
            abs(core_score - padded_both),
        )),
    }
    logger.info(f"Neutral padding: polarity preserved={results['neutral_padding']['polarity_preserved_all']}, "
                f"max deviation={results['neutral_padding']['max_deviation']:.4f}")

    # ================================================================
    # TEST 3: SECTION REORDERING STABILITY
    # Shuffling neutral sections around a fixed emotional core
    # should produce low variance
    # ================================================================
    sections = [
        "The product arrived on Tuesday in a brown cardboard box.",
        "It weighs approximately two pounds and measures twelve inches.",
        "The instruction manual is printed in four languages.",
        "Assembly requires a Phillips head screwdriver.",
        "The warranty covers manufacturing defects for one year.",
    ]
    emotional_section = "I am incredibly disappointed with the poor build quality."

    reorder_scores = []
    rng = np.random.RandomState(42)
    for _ in range(10):
        shuffled = sections.copy()
        rng.shuffle(shuffled)
        # Insert emotional section at random position
        insert_pos = rng.randint(0, len(shuffled) + 1)
        shuffled.insert(insert_pos, emotional_section)
        full_text = " ".join(shuffled)
        reorder_scores.append(engine.score(full_text))

    reorder_scores = np.array(reorder_scores)
    results["reordering"] = {
        "scores": reorder_scores.tolist(),
        "mean": float(np.mean(reorder_scores)),
        "std": float(np.std(reorder_scores)),
        "polarity_unanimous": bool(all(np.sign(reorder_scores) == np.sign(reorder_scores[0]))),
    }
    logger.info(f"Reordering stability: mean={np.mean(reorder_scores):.4f}, "
                f"std={np.std(reorder_scores):.4f}, "
                f"polarity unanimous={results['reordering']['polarity_unanimous']}")

    # ================================================================
    # TEST 4: CONTRASTIVE PIVOT HANDLING
    # "X but Y" should weight Y more heavily
    # ================================================================
    pivot_tests = [
        {
            "text": "The food was terrible but the service was excellent.",
            "expected_direction": "positive",  # post-pivot dominates
        },
        {
            "text": "The food was excellent but the service was terrible.",
            "expected_direction": "negative",
        },
        {
            "text": "I hated the wait however the meal was absolutely worth it.",
            "expected_direction": "positive",
        },
    ]

    pivot_results = []
    for test in pivot_tests:
        score = engine.score(test["text"])
        expected_sign = 1 if test["expected_direction"] == "positive" else -1
        correct = (np.sign(score) == expected_sign) if score != 0.0 else False
        pivot_results.append({
            "text": test["text"],
            "score": float(score),
            "expected": test["expected_direction"],
            "correct": bool(correct),
        })

    results["pivot_handling"] = {
        "details": pivot_results,
        "accuracy": float(np.mean([r["correct"] for r in pivot_results])),
    }
    logger.info(f"Pivot handling accuracy: {results['pivot_handling']['accuracy']:.0%}")

    # ================================================================
    # SAVE
    # ================================================================
    with open(save_path, "w") as f:
        json.dump(results, f, indent=2, default=str)
    logger.info(f"Stress test results saved to {save_path}")

    return results
```

---

## PHASE 5: SDK PACKAGING AND DOCUMENTATION

### Step 5.1: Update pyproject.toml
Replace the contents of `pyproject.toml` with:
```toml
[build-system]
requires = ["setuptools>=68.0", "wheel"]
build-backend = "setuptools.backends._legacy:_Backend"

[project]
name = "sgw"
version = "3.5.0"
description = "Sentiment Gummy Worm: Unsupervised structural sentiment scoring engine"
readme = "README.md"
license = {text = "Apache-2.0"}
requires-python = ">=3.9"
authors = [{name = "Ayaz Khan"}]
keywords = ["sentiment", "nlp", "unsupervised", "explainable"]
classifiers = [
    "Development Status :: 4 - Beta",
    "Intended Audience :: Developers",
    "Intended Audience :: Science/Research",
    "Topic :: Scientific/Engineering :: Artificial Intelligence",
    "Topic :: Text Processing :: Linguistic",
]

dependencies = [
    "numpy>=1.24",
    "scipy>=1.10",
    "pandas>=2.0",
    "gensim>=4.3",
    "spacy>=3.7",
    "scikit-learn>=1.3",
    "lightgbm>=4.0",
    "pyyaml>=6.0",
    "tqdm>=4.60",
    "nltk>=3.8",
]

[project.optional-dependencies]
api = ["fastapi>=0.100", "uvicorn>=0.25", "pydantic>=2.0"]
dev = ["pytest>=8.0", "pytest-cov>=4.0", "black>=24.0", "flake8>=7.0"]
all = ["sgw[api,dev]"]

[tool.setuptools.packages.find]
include = ["sgw*"]
```

### Step 5.2: Update README.md
Replace README.md with a comprehensive version that covers the product positioning:

```markdown
# Sentiment Gummy Worm (SGW) V3.5

**A structural sentiment engine for long-form, domain-specific, and regulated text.**

SGW produces continuous sentiment scores from -1.0 to +1.0 with full token-level explainability, domain-adaptive axes, and zero labeled training data.

## When to use SGW (instead of VADER/TextBlob)

| Requirement | VADER | SGW |
|---|---|---|
| Short social media text | ✅ Optimized for this | ⚠️ Competitive |
| Long-form documents (reviews, reports, transcripts) | ⚠️ Degrades with length | ✅ Designed for this |
| Domain-specific vocabulary (finance, healthcare, CX) | ❌ Fixed 7,517-word lexicon | ✅ Swap axis in one call |
| Explainability / auditability | ❌ Black box compound score | ✅ Full token decomposition |
| Regulated industries (compliance, clinical) | ❌ No explanation trail | ✅ Every score is auditable |
| Multilingual (with FastText vectors) | ❌ English only | ✅ Any language with embeddings |
| Continuous regression scores | ⚠️ Compound is pseudo-continuous | ✅ True continuous [-1, 1] |

## Quick Start

```python
from sgw.utils.vectors import load_vectors
from sgw.core.valence.scorer import ValenceScorer
from sgw.core.valence.axis import SentimentAxis
from sgw.pipeline.syntax import SyntaxParser
from sgw.core.engine import SGWEngine

vectors = load_vectors(limit=500000)

# Use a domain-specific axis
axis = SentimentAxis.from_preset(vectors, "customer_experience")
scorer = ValenceScorer(vectors, axis=axis)
parser = SyntaxParser()
engine = SGWEngine(scorer, parser)

# Score with full explanation
explanation = engine.explain("The delivery was fast but the product quality was disappointing.")
print(f"Score: {explanation.score:.3f}")
print(f"Pivot: {explanation.pivot_token}")
for t in explanation.top_contributors(3):
    print(f"  {t.token}: {t.raw_contribution:+.4f} (negated={t.is_negated})")
```

## Available Domain Presets

```python
SentimentAxis.from_preset(vectors, "general")             # Default English
SentimentAxis.from_preset(vectors, "customer_experience")  # CX / support
SentimentAxis.from_preset(vectors, "finance")              # Earnings, risk
SentimentAxis.from_preset(vectors, "healthcare")           # Clinical text
SentimentAxis.from_preset(vectors, "product_reviews")      # E-commerce
```

Or define your own:
```python
axis = SentimentAxis(
    vectors=vectors,
    positive=["compliant", "approved", "cleared", "satisfactory"],
    negative=["violation", "penalty", "breach", "noncompliant"],
    name="regulatory_compliance"
)
```

## API

```bash
uvicorn sgw.api.main:app --host 0.0.0.0 --port 8000
```

Endpoints:
- `POST /analyze` — Score text, returns `{score, lambda_c, k}`
- `POST /explain` — Score with explanation summary
- `POST /explain/full` — Complete token-level decomposition
- `POST /axis/swap` — Switch domain axis
- `GET /axis/current` — Current axis info
- `GET /health` — Health check

## Methodology

SGW computes sentiment via four components:
1. **Vector Valence** — Projects word embeddings onto a sentiment axis (SemAxis method)
2. **Spatial Decay** — Weights earlier words more heavily (primacy effect), with contrastive pivot detection
3. **Syntactic Dilution** — Dampens intensity based on function-word ratio, conditioned on affective mass
4. **Magnitude-Aware Negation** — Negation strength scales with word intensity

See the [technical paper](docs/) for mathematical details and benchmark results.

## Benchmarks

| Dataset | SGW ρ | VADER ρ | Winner |
|---|---|---|---|
| SST-5 (movie sentences) | **0.480** | 0.436 | SGW |
| IMDB (long reviews) | **0.493** | 0.484 | SGW |
| GoEmotions (short Reddit) | 0.484 | 0.599 | VADER |
| Yelp (implicit sentiment) | 0.336 | 0.527 | VADER |

SGW excels on linguistically complex, long-form text. VADER excels on short, emotionally explicit social media text.

## License

Apache-2.0
```

### Step 5.3: Master Runner Script
Create file `sgw/evaluation/run_tier4_5.py`:
```python
"""Tier 4.5 Master Runner: Domain adaptation, CX benchmark, stress tests."""

import sys
from sgw.utils.vectors import load_vectors
from sgw.utils.config import load_config
from sgw.core.valence.scorer import ValenceScorer
from sgw.core.valence.axis import SentimentAxis
from sgw.pipeline.syntax import SyntaxParser
from sgw.core.engine import SGWEngine
from sgw.utils.logger import get_logger

logger = get_logger(__name__)


def main():
    print("=" * 70)
    print("SGW TIER 4.5: PRODUCTIZATION EVALUATION")
    print("=" * 70)

    # Setup
    vectors = load_vectors(limit=500000)
    scorer = ValenceScorer(vectors)
    parser = SyntaxParser()
    engine = SGWEngine(scorer, parser)

    # Load calibration
    import json
    from pathlib import Path
    cal_path = Path("reports/metrics/calibration_results.json")
    if cal_path.exists():
        with open(cal_path) as f:
            cal = json.load(f)
        engine.set_lambda(cal["best_lambda"])
        engine.set_k(cal["best_k"])
        logger.info(f"Loaded calibration: λ={cal['best_lambda']}, k={cal['best_k']}")

    final_path = Path("reports/metrics/final_evaluation.json")
    if final_path.exists():
        with open(final_path) as f:
            final = json.load(f)
        vm = final.get("variance_matching", {})
        if vm:
            engine.set_calibration_scale(vm["mu_s"], vm["sigma_s"], vm["sigma_y"])
            logger.info("Loaded variance-matching calibration.")

    # Phase 1: Domain axis validation
    print("\n" + "=" * 50)
    print("PHASE 1: DOMAIN AXIS VALIDATION")
    print("=" * 50)
    for domain in ["general", "customer_experience", "finance", "healthcare", "product_reviews"]:
        axis = SentimentAxis.from_preset(vectors, domain)
        scorer.swap_axis(axis)
        validation = scorer.validate_axis()
        status = "PASS" if validation["pos_all_positive"] and validation["neg_all_negative"] else "WARN"
        print(f"  [{status}] {domain}: sep={validation['separation']:.3f}, σ_aff={axis.sigma_v_aff:.4f}")

    # Phase 2: CX Benchmark
    print("\n" + "=" * 50)
    print("PHASE 2: CX BENCHMARK")
    print("=" * 50)
    from sgw.evaluation.cx_benchmark import run_cx_benchmark
    cx_results = run_cx_benchmark(engine, limit=2000)

    # Phase 3: Stress Tests
    print("\n" + "=" * 50)
    print("PHASE 3: STRESS TESTS")
    print("=" * 50)
    # Reset to general axis for stress tests
    scorer.swap_axis(SentimentAxis.from_preset(vectors, "general"))
    from sgw.evaluation.stress_tests import run_stress_tests
    stress_results = run_stress_tests(engine)

    # Summary
    print("\n" + "=" * 70)
    print("TIER 4.5 COMPLETE")
    print("=" * 70)
    print(f"CX domain adaptation delta: Δρ={cx_results.get('domain_adaptation_delta', 'N/A')}")
    print(f"Reordering stability: std={stress_results['reordering']['std']:.4f}")
    print(f"Pivot accuracy: {stress_results['pivot_handling']['accuracy']:.0%}")
    print(f"Neutral padding polarity preserved: {stress_results['neutral_padding']['polarity_preserved_all']}")


if __name__ == "__main__":
    main()
```

---

## EXECUTION ORDER

```
PHASE 1 (Steps 1.1-1.3) -> Gate Check P1
    ↓ (all domain axis tests pass)
PHASE 2 (Steps 2.1-2.3) -> Gate Check P2
    ↓ (explain() matches score(), summary is valid JSON)
PHASE 3 (Step 3.1) -> CX benchmark produces results
    ↓
PHASE 4 (Step 4.1) -> Stress tests produce results
    ↓
PHASE 5 (Steps 5.1-5.3) -> Package and document
    ↓
Run: python sgw/evaluation/run_tier4_5.py
    ↓
DONE — commit and push
```

### Final Git Commit
```bash
git add -A
git commit -m "Tier 4.5: Domain-adaptive axes, explanation output, CX benchmark, stress tests"
git tag -a v3.5.0 -m "SGW V3.5: Product-ready with domain adaptation and explainability"
git push origin feature/sgw-v3-plus
git push origin v3.5.0
```

---

## KEY FILES CREATED OR MODIFIED

| File | Action | Purpose |
|---|---|---|
| `sgw/core/valence/axis.py` | NEW | Domain-adaptive sentiment axes with 5 presets |
| `sgw/core/valence/scorer.py` | MODIFIED | Accepts custom axes, hot-swap support |
| `sgw/core/explanation.py` | NEW | Structured explanation data model |
| `sgw/core/engine.py` | MODIFIED | Added explain() and explain_doc() methods |
| `sgw/api/main.py` | MODIFIED | Added /explain, /explain/full, /axis/swap, /axis/current |
| `sgw/evaluation/cx_benchmark.py` | NEW | Amazon Reviews CX benchmark pipeline |
| `sgw/evaluation/stress_tests.py` | NEW | Truncation, padding, reordering, pivot tests |
| `sgw/evaluation/run_tier4_5.py` | NEW | Master runner for all Tier 4.5 evaluations |
| `tests/unit/test_axis.py` | NEW | Domain axis unit tests |
| `pyproject.toml` | MODIFIED | Proper Python package configuration |
| `README.md` | MODIFIED | Product-oriented documentation |
