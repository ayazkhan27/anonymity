"""Unit tests for SGW core modules."""

import pytest
import numpy as np


# --- Decay Function Tests ---

class TestDecayFunctions:
    def test_position_zero_returns_one(self):
        from sgw.core.decay.functions import exponential_decay
        assert exponential_decay(0, 100, 1.0) == 1.0

    def test_decay_decreases_with_position(self):
        from sgw.core.decay.functions import exponential_decay
        w1 = exponential_decay(10, 100, 1.0)
        w2 = exponential_decay(50, 100, 1.0)
        w3 = exponential_decay(90, 100, 1.0)
        assert w1 > w2 > w3

    def test_higher_lambda_faster_decay(self):
        from sgw.core.decay.functions import exponential_decay
        w_low = exponential_decay(50, 100, 0.5)
        w_high = exponential_decay(50, 100, 2.0)
        assert w_low > w_high

    def test_vectorized_decay_matches_scalar(self):
        from sgw.core.decay.functions import exponential_decay, compute_decay_weights
        weights = compute_decay_weights(10, 1.5)
        for i in range(10):
            expected = exponential_decay(i, 10, 1.5)
            assert abs(weights[i] - expected) < 1e-10

    def test_empty_document_decay(self):
        from sgw.core.decay.functions import compute_decay_weights
        weights = compute_decay_weights(0, 1.0)
        assert len(weights) == 0

    def test_single_token_decay(self):
        from sgw.core.decay.functions import compute_decay_weights
        weights = compute_decay_weights(1, 1.0)
        assert len(weights) == 1
        assert weights[0] == 1.0


# --- Buffer Ratio Tests ---

class TestDilution:
    def test_zero_buffers_no_dilution(self):
        from sgw.core.dilution.buffer import compute_buffer_ratio, dilution_factor
        ratio = compute_buffer_ratio(0, 100)
        assert ratio == 0.0
        assert dilution_factor(ratio, 2.0) == 1.0

    def test_all_buffers_strong_dilution(self):
        from sgw.core.dilution.buffer import compute_buffer_ratio, dilution_factor
        ratio = compute_buffer_ratio(100, 100)
        assert ratio == 1.0
        d = dilution_factor(ratio, 2.0)
        assert d < 0.2  # exp(-2) ≈ 0.135

    def test_half_buffers(self):
        from sgw.core.dilution.buffer import compute_buffer_ratio, dilution_factor
        ratio = compute_buffer_ratio(50, 100)
        assert abs(ratio - 0.5) < 1e-10
        d = dilution_factor(ratio, 2.0)
        assert abs(d - np.exp(-1.0)) < 1e-10  # exp(-2 * 0.5) = exp(-1)

    def test_empty_document(self):
        from sgw.core.dilution.buffer import compute_buffer_ratio
        assert compute_buffer_ratio(0, 0) == 0.0


# --- Valence Scorer Tests ---
# These require vectors to be loaded, so they may be slow.

class TestValenceScorer:
    @pytest.fixture(scope="class")
    def scorer(self):
        from sgw.utils.vectors import load_vectors
        from sgw.core.valence.scorer import ValenceScorer
        # Load limited vectors for speed
        vectors = load_vectors(limit=100000)
        return ValenceScorer(vectors)

    def test_positive_words_score_positive(self, scorer):
        for word in ["happy", "excellent", "wonderful"]:
            v = scorer.get_valence(word)
            assert v > 0, f"'{word}' should be positive, got {v}"

    def test_negative_words_score_negative(self, scorer):
        for word in ["terrible", "horrible", "awful"]:
            v = scorer.get_valence(word)
            assert v < 0, f"'{word}' should be negative, got {v}"

    def test_oov_returns_fallback(self, scorer):
        v = scorer.get_valence("asdfghjklzxcv")
        assert v == 0.0

    def test_axis_validation(self, scorer):
        results = scorer.validate_axis()
        assert results["pos_all_positive"], "Not all positive test words scored positive"
        assert results["neg_all_negative"], "Not all negative test words scored negative"
        assert results["separation"] > 0.2, f"Insufficient axis separation: {results['separation']}"
