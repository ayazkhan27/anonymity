"""Unit tests for domain-adaptive sentiment axes."""

import pytest
import numpy as np


class TestSentimentAxis:
    @pytest.fixture(scope="class")
    def vectors(self):
        from sgw.utils.vectors import load_vectors
        return load_vectors(limit=100000)

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

        # Domain-specific: "growth" should be positive, "loss" negative
        assert scorer.get_valence("growth") > 0, "growth should be positive in finance"
        assert scorer.get_valence("loss") < 0, "loss should be negative in finance"
        assert scorer.get_valence("outperform") > 0
        assert scorer.get_valence("recession") < 0

    def test_healthcare_axis_domain_correctness(self, vectors):
        from sgw.core.valence.axis import SentimentAxis
        from sgw.core.valence.scorer import ValenceScorer

        axis = SentimentAxis.from_preset(vectors, "healthcare")
        scorer = ValenceScorer(vectors, axis=axis)

        # "improvement" is a very stable positive anchor
        # "malignant" is a very stable negative anchor
        assert scorer.get_valence("improvement") > 0
        assert scorer.get_valence("malignant") < 0
        assert scorer.get_valence("successful") > 0
        assert scorer.get_valence("failure") < 0

    def test_cx_axis_domain_correctness(self, vectors):
        from sgw.core.valence.axis import SentimentAxis
        from sgw.core.valence.scorer import ValenceScorer

        axis = SentimentAxis.from_preset(vectors, "customer_experience")
        scorer = ValenceScorer(vectors, axis=axis)

        # "satisfied" and "impressed" are stable positive anchors
        assert scorer.get_valence("satisfied") > 0
        assert scorer.get_valence("impressed") > 0
        assert scorer.get_valence("frustrated") < 0
        assert scorer.get_valence("terrible") < 0
        assert scorer.get_valence("responsive") > 0

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
