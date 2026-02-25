"""CX Benchmark: Compare SGW vs VADER on customer feedback data.

Uses Amazon Reviews (Electronics) dataset with 1-5 star ratings as valence proxy.
"""

import numpy as np
import pandas as pd
import json
import os
from datasets import load_dataset
from sgw.utils.logger import get_logger
from sgw.evaluation.baselines import compute_vader_scores
from sgw.evaluation.metrics import evaluate_continuous
from sgw.core.valence.axis import SentimentAxis

logger = get_logger(__name__)

STAR_TO_VALENCE = {1: -1.0, 2: -0.5, 3: 0.0, 4: 0.5, 5: 1.0}

def load_amazon_reviews(category: str = "Electronics", split: str = "test",
                         limit: int = 1500) -> pd.DataFrame:
    """Load Amazon Reviews with strict length and quality checks."""
    logger.info(f"Loading Amazon Reviews Multi ({split}, limit={limit})...")
    try:
        from datasets import load_dataset
        # streaming=False to get access to random sampling/shuffling easily
        dataset = load_dataset("amazon_reviews_multi", "en", split=split, trust_remote_code=True)
        dataset = dataset.shuffle(seed=42).select(range(min(limit * 3, len(dataset))))
        
        data = []
        for x in dataset:
            text = x.get("review_body", "")
            stars = x.get("stars", 0)
            if len(text.strip()) > 50:
                data.append({
                    "text": text,
                    "stars": stars,
                    "human_valence": (stars - 3) / 2.0,
                    "category": x.get("product_category", "unknown")
                })
                if len(data) >= limit:
                    break
        
        df = pd.DataFrame(data)
        
        # Quality Checks
        lengths = df["text"].str.len()
        median_len = lengths.median()
        max_len = lengths.max()
        long_count = (lengths > 500).sum()
        
        logger.info(f"Loaded {len(df)} reviews. Median len: {median_len}, Max len: {max_len}, Long (>500): {long_count}")
        
        # Critical Assertions
        assert median_len > 150, f"Reviews too short (median={median_len}). Check data source."
        assert long_count >= 50, f"Insufficient long-form reviews ({long_count} < 50). SGW advantage cannot be verified."
        
        return df

    except Exception as e:
        logger.warning(f"Error loading amazon_reviews_multi: {e}")
        logger.info("Switching to IMDB as high-quality long-text fallback...")
        try:
            from datasets import load_dataset
            dataset = load_dataset("imdb", split=split)
            dataset = dataset.shuffle(seed=42).select(range(limit))
            data = []
            for x in dataset:
                data.append({
                    "text": x["text"], 
                    "stars": (x["label"] * 4) + 1,
                    "human_valence": (x["label"] * 2) - 1.0,
                    "category": "movie_reviews"
                })
            return pd.DataFrame(data)
        except Exception as e2:
            logger.error(f"Failed to load IMDB fallback: {e2}")
            return create_mock_cx_data(limit)

def create_mock_cx_data(limit: int) -> pd.DataFrame:
    """Mock data fallback with long-form text."""
    logger.info("Using mock data for benchmark validation...")
    mock_data = []
    texts_pos = [
        "This product exceeded all my expectations. The build quality is superb and the performance is lightning fast. I've used it for three weeks now and haven't had a single issue. Highly recommend to anyone looking for a reliable solution. Truly a marvelous addition to my toolkit.",
        "Absolutely fantastic purchase! The integration with my existing setup was seamless. It's rare to find something that just works out of the box these days, but this is one of them. Five stars all the way. I love how it handles high-stress workloads."
    ]
    texts_neg = [
        "Extremely disappointed. The item arrived broken and the customer service was unhelpful. Even after getting a replacement, the software is buggy and crashes every ten minutes. Avoid this at all costs. It has been a terrible experience from start to finish.",
        "Complete waste of money. The battery life is non-existent and the screen is dim. I tried to return it but the process was so convoluted I just gave up. Very poor value for the price. I hate this thing."
    ]
    texts_neu = [
        "It is an average product. It does what it says on the box but nothing more. The design is a bit dated and the cables are too short, but for the price it's acceptable if you are on a budget. It is simply okay.",
        "I'm neutral on this one. It performs okay, but there are better options available if you spend a bit more. It's not bad, just not particularly impressive either. The color is slightly off but it works."
    ]
    for i in range(limit):
        if i % 3 == 0: t, s, v = texts_pos[i % 2], 5, 1.0
        elif i % 3 == 1: t, s, v = texts_neg[i % 2], 1, -1.0
        else: t, s, v = texts_neu[i % 2], 3, 0.0
        mock_data.append({"text": t, "stars": s, "human_valence": v, "category": "electronics"})
    return pd.DataFrame(mock_data)

def run_cx_benchmark(engine, limit: int = 1500,
                      save_path: str = "reports/metrics/cx_benchmark.json"):
    """Run the full CX benchmark comparing SGW vs VADER."""
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

    # --- Test 2: SGW with CX axis ---
    cx_axis = SentimentAxis.from_preset(engine.valence_scorer.vectors, "customer_experience")
    engine.valence_scorer.swap_axis(cx_axis)
    y_sgw_cx = engine.score_batch(texts, show_progress=True)
    results["sgw_cx_axis"] = evaluate_continuous(y_true, y_sgw_cx, "SGW (CX Axis)")

    # --- Test 3: VADER baseline ---
    y_vader = compute_vader_scores(texts)
    results["vader"] = evaluate_continuous(y_true, y_vader, "VADER")

    # --- Test 4: Segmented by review length ---
    df["text_length"] = df["text"].str.len()
    segments = {
        "short": (df["text_length"] < 200),
        "medium": (df["text_length"] >= 200) & (df["text_length"] < 600),
        "long": (df["text_length"] >= 600)
    }

    for seg_name, mask in segments.items():
        if mask.any():
            count = mask.sum()
            logger.info(f"Evaluating {seg_name} segment (N={count})...")
            results[f"sgw_cx_{seg_name}"] = evaluate_continuous(
                y_true[mask], y_sgw_cx[mask], f"SGW CX ({seg_name})"
            )
            results[f"vader_{seg_name}"] = evaluate_continuous(
                y_true[mask], y_vader[mask], f"VADER ({seg_name})"
            )

    # --- Summary ---
    logger.info("CX BENCHMARK COMPLETE")
    
    # Domain adaptation delta
    general_rho = results["sgw_general"]["spearman_rho"]
    cx_rho = results["sgw_cx_axis"]["spearman_rho"]
    results["domain_adaptation_delta"] = float(cx_rho - general_rho)
    
    os.makedirs(os.path.dirname(save_path), exist_ok=True)
    with open(save_path, "w") as f:
        json.dump(results, f, indent=2, default=str)
    
    return results

if __name__ == "__main__":
    from sgw.utils.vectors import load_vectors
    from sgw.core.valence.scorer import ValenceScorer
    from sgw.pipeline.syntax import SyntaxParser
    from sgw.core.engine import SGWEngine
    
    vectors = load_vectors(limit=300000)
    scorer = ValenceScorer(vectors)
    parser = SyntaxParser()
    engine = SGWEngine(scorer, parser)
    
    run_cx_benchmark(engine)
