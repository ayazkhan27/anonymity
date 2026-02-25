"""Vector-based valence scoring using embedding projection onto a sentiment axis.

Methodology:
  1. Define positive centroid: average of positive seed word vectors.
  2. Define negative centroid: average of negative seed word vectors.
  3. Sentiment axis = positive_centroid - negative_centroid (normalized).
  4. Word valence = cosine_similarity(word_vector, sentiment_axis).

This is based on the SemAxis framework (An, Kwak & Ahn, ACL 2018).

The resulting score is in approximately [-1, 1] but may slightly exceed these
bounds due to cosine similarity properties. We do NOT clip here; clipping
happens at the final SGW score level.
"""

import numpy as np
from functools import lru_cache
from gensim.models import KeyedVectors
from sgw.utils.config import load_config
from sgw.utils.logger import get_logger

logger = get_logger(__name__)


class ValenceScorer:
    """Computes word-level valence by projecting onto a sentiment axis."""

    def __init__(self, vectors: KeyedVectors, config: dict = None):
        """Initialize the valence scorer.

        Args:
            vectors: Pre-loaded KeyedVectors (GloVe or compatible).
            config: Configuration dict. If None, loads from default config file.
        """
        if config is None:
            config = load_config()

        self.vectors = vectors
        self.config = config
        self.fallback_valence = config["valence"]["fallback_valence"]

        # Build sentiment axis
        self.sentiment_axis = self._build_sentiment_axis(
            config["valence"]["positive_seeds"],
            config["valence"]["negative_seeds"]
        )
        
        # Compute global valence variance
        self.sigma_v_sq = self._compute_valence_variance()
        logger.info(f"Global valence variance (sigma_V^2) computed: {self.sigma_v_sq:.6f}")
        logger.info("ValenceScorer initialized.")

    def _build_sentiment_axis(self, pos_seeds: list, neg_seeds: list) -> np.ndarray:
        """Compute the sentiment axis from seed words.

        Args:
            pos_seeds: List of positive seed words.
            neg_seeds: List of negative seed words.

        Returns:
            Normalized sentiment axis vector (300-dimensional for GloVe).
        """
        pos_vecs = []
        for word in pos_seeds:
            if word in self.vectors:
                pos_vecs.append(self.vectors[word])
            else:
                logger.warning(f"Positive seed '{word}' not in vocabulary. Skipping.")

        neg_vecs = []
        for word in neg_seeds:
            if word in self.vectors:
                neg_vecs.append(self.vectors[word])
            else:
                logger.warning(f"Negative seed '{word}' not in vocabulary. Skipping.")

        assert len(pos_vecs) >= 5, f"Too few positive seeds found: {len(pos_vecs)}"
        assert len(neg_vecs) >= 5, f"Too few negative seeds found: {len(neg_vecs)}"

        pos_centroid = np.mean(pos_vecs, axis=0)
        neg_centroid = np.mean(neg_vecs, axis=0)

        axis = pos_centroid - neg_centroid
        axis_norm = np.linalg.norm(axis)
        assert axis_norm > 0, "Sentiment axis has zero norm"
        axis = axis / axis_norm

        logger.info(f"Sentiment axis built from {len(pos_vecs)} positive "
                     f"and {len(neg_vecs)} negative seed words.")
        return axis

    def _compute_valence_variance(self) -> float:
        """Compute the variance of cosine similarities over the affective support set.
        
        This implements Option A (Affective-Support Variance Normalization) to prevent
        sentiment-neutral words from collapsing the variance scale. We calculate variance
        only over the top 15% most sentiment-aligned words.
        
        Returns:
            Float variance (sigma_V,aff^2).
        """
        logger.info("Computing affective-support valence variance (Option A) across the embedding vocabulary...")
        
        # Use vectorized operations for speed
        all_vecs = self.vectors.vectors
        norms = np.linalg.norm(all_vecs, axis=1, keepdims=True)
        norms[norms == 0] = 1.0
        normalized_vecs = all_vecs / norms
        
        # The sentiment axis is already unit length, so cos_sim = dot product
        cosines = np.dot(normalized_vecs, self.sentiment_axis)
        
        # Fallback to standard 1D array if needed, np.dot with 1D axis returns 1D
        cosines = cosines.flatten()
        
        # Method 1: Percentile-based epsilon
        abs_cosines = np.abs(cosines)
        epsilon = float(np.percentile(abs_cosines, 85))  # Q_0.85
        
        affective_mask = abs_cosines > epsilon
        affective_cosines = cosines[affective_mask]
        
        variance = float(np.var(affective_cosines))
        
        logger.info(f"Affective support epsilon (85th pct): {epsilon:.4f}")
        logger.info(f"Affective support size: {np.sum(affective_mask)} out of {len(cosines)}")
        
        return variance

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
        # Cosine similarity with sentiment axis
        dot_product = np.dot(word_vec, self.sentiment_axis)
        word_norm = np.linalg.norm(word_vec)

        if word_norm == 0:
            return self.fallback_valence

        valence = dot_product / word_norm
        
        # V* = cos(w_i, s) / sqrt(sigma_V^2)
        if hasattr(self, "sigma_v_sq") and self.sigma_v_sq > 0:
            valence = valence / np.sqrt(self.sigma_v_sq)
            
        return float(valence)

    def validate_axis(self) -> dict:
        """Sanity check: verify that known positive/negative words score correctly.

        Returns:
            dict with validation results.
        """
        test_positive = ["happy", "excellent", "wonderful", "beautiful", "great"]
        test_negative = ["terrible", "horrible", "awful", "disgusting", "miserable"]
        test_neutral = ["table", "chair", "walk", "number", "Tuesday"]

        pos_scores = [self.get_valence(w) for w in test_positive]
        neg_scores = [self.get_valence(w) for w in test_negative]
        neu_scores = [self.get_valence(w) for w in test_neutral]

        results = {
            "pos_mean": float(np.mean(pos_scores)),
            "neg_mean": float(np.mean(neg_scores)),
            "neu_mean": float(np.mean(neu_scores)),
            "pos_all_positive": all(s > 0 for s in pos_scores),
            "neg_all_negative": all(s < 0 for s in neg_scores),
            "neu_near_zero": abs(np.mean(neu_scores)) < 0.3,
            "separation": float(np.mean(pos_scores) - np.mean(neg_scores)),
        }

        logger.info(f"Axis validation: pos_mean={results['pos_mean']:.3f}, "
                     f"neg_mean={results['neg_mean']:.3f}, "
                     f"neu_mean={results['neu_mean']:.3f}, "
                     f"separation={results['separation']:.3f}")

        return results
