"""Domain-adaptive sentiment axis construction.

Allows users to define custom sentiment poles for any domain.
The sentiment axis is recomputed from the new poles, and the entire
SGW pipeline immediately operates in the new domain.
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
        """Compute affective-support variance σ_V,aff using vectorized operations.

        This is the variance of cos(w, axis) computed ONLY over the top
        (1-percentile) fraction of words by absolute projection magnitude.
        
        Args:
            vectors: Full vocabulary vectors.

        Returns:
            σ_V,aff as a float.
        """
        # Get matrix of vectors (normalized)
        all_vecs = vectors.vectors[:200000] # Use top 200k for variance estimate
        norms = np.linalg.norm(all_vecs, axis=1, keepdims=True)
        norms[norms == 0] = 1.0
        normalized_vecs = all_vecs / norms
        
        # Projections are dot product with unit axis
        projections = np.dot(normalized_vecs, self.axis_vector)
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
