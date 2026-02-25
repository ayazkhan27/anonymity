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
        self._get_valence_cached.cache_clear()
        logger.info(f"Axis swapped to '{axis.name}'. Valence cache cleared.")

    @lru_cache(maxsize=50000)
    def _get_valence_cached(self, word_lower: str) -> float:
        """Internal cached method for float valence lookup."""
        if word_lower not in self.vectors:
            return self.fallback_valence
        word_vec = self.vectors[word_lower]
        return self.axis.project(word_vec)

    def get_valence(self, token_or_str) -> float:
        """Get the valence score for a single word or spaCy token.
        
        Args:
            token_or_str: spaCy Token object or string.
        """
        # Dictionary of overrides (should be cleansed by config loader)
        overrides = self.config.get("valence", {}).get("valence_overrides", {})

        if isinstance(token_or_str, str):
            if not token_or_str:
                return self.fallback_valence
            word_lower = token_or_str.lower()
            if word_lower in overrides:
                return float(overrides[word_lower])
            return self._get_valence_cached(word_lower)
        
        # Handle spacy Token
        surface = token_or_str.text.lower()
        lemma = token_or_str.lemma_.lower()
        
        if surface in overrides:
            return float(overrides[surface])
        if lemma in overrides:
            return float(overrides[lemma])
            
        # Try embedding lookup (lemma first, then surface)
        val_lemma = self._get_valence_cached(lemma)
        if val_lemma != self.fallback_valence:
             return val_lemma
             
        return self._get_valence_cached(surface)

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
