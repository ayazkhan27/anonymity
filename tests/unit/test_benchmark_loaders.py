"""Unit tests for benchmark data loaders."""

import pytest
import pandas as pd


class TestBenchmarkLoaders:

    def test_fiqa_loads(self):
        from sgw.evaluation.benchmark_loaders import load_benchmark
        df = load_benchmark("fiqa", split="test", limit=50)
        assert len(df) > 0, "FiQA returned no data"
        assert "text" in df.columns
        assert "human_valence" in df.columns
        assert df["human_valence"].between(-1.0, 1.0).all()
        # The replacement Twitter dataset has 3 unique categorical labels mapped to [-1.0, 0.0, 1.0]
        assert df["human_valence"].nunique() <= 3, \
            f"FiQA replacement should have categorical labels, got {df['human_valence'].nunique()} unique"

    def test_phrasebank_loads(self):
        from sgw.evaluation.benchmark_loaders import load_benchmark
        df = load_benchmark("phrasebank", split="test", limit=50)
        assert len(df) > 0
        assert df["human_valence"].between(-1.0, 1.0).all()
        # PhraseBank maps to 3 values: -0.8, 0.0, 0.8
        assert df["human_valence"].nunique() <= 3

    def test_imdb_loads(self):
        from sgw.evaluation.benchmark_loaders import load_benchmark
        df = load_benchmark("imdb", split="test", limit=50)
        assert len(df) > 0
        # IMDB reviews should be long
        assert df["text"].str.len().median() > 200, \
            f"IMDB reviews should be long, median={df['text'].str.len().median()}"

    def test_unknown_dataset_raises(self):
        from sgw.evaluation.benchmark_loaders import load_benchmark
        with pytest.raises(ValueError, match="Unknown benchmark"):
            load_benchmark("nonexistent_dataset")

    def test_fiqa_continuous_labels_have_variance(self):
        """Critical: FiQA labels must have real variance for Spearman to work."""
        from sgw.evaluation.benchmark_loaders import load_benchmark
        df = load_benchmark("fiqa", split="all", limit=200)
        assert df["human_valence"].std() > 0.1, \
            f"FiQA labels have insufficient variance: std={df['human_valence'].std()}"
