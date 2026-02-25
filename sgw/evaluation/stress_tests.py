"""Long-Document Stress Tests for the SGW Engine.

Validates stability under various perturbations:
1. Truncation (Short vs Long)
2. Neutral Padding (Dilution stability)
3. Section Reordering (Decay stability)
4. Vocabulary Perturbation (OOP handling)
"""

import numpy as np
import pandas as pd
import json
import os
from sgw.utils.logger import get_logger
from sgw.core.engine import SGWEngine
from sgw.pipeline.syntax import SyntaxParser
from sgw.core.valence.scorer import ValenceScorer

logger = get_logger(__name__)


def run_stress_tests(engine: SGWEngine, save_path: str = "reports/metrics/stress_tests.json"):
    """Run all long-document stress tests with raw score diagnostics."""
    results = {}

    # 1. Neutral Padding Test (Dilution Test)
    # Using multiple base texts to check if padding consistently dilutes.
    samples = [
        "I am absolutely thrilled with this amazing service. It is wonderful!",
        "The food was extremely delicious and the staff were very polite.",
        "I had a terrible experience, the product failed immediately."
    ]
    # Neutral padding (ultra-neutral 'data' which is neutralized in config)
    neutral_sent = " data data data data data data data data data data. "
    
    dilution_results = []
    for base in samples:
        expl_0 = engine.explain(base)
        expl_5 = engine.explain(base + neutral_sent * 5)
        expl_15 = engine.explain(base + neutral_sent * 15)
        
        # Ensure both padded versions pull towards zero (magnitude decreases)
        base_abs = abs(expl_0.score_raw)
        p5_abs = abs(expl_5.score_raw)
        p15_abs = abs(expl_15.score_raw)
        
        # Test 1: Does padding decrease absolute magnitude?
        magnitude_decreased = (p15_abs < p5_abs) and (p5_abs < base_abs)
        
        # Test 2: Does it maintain polarity? (Don't flip positive to negative, handle near-zero bases carefully)
        # If base is clearly positive (>0.1) padded must be >= 0
        # If base is clearly negative (<-0.1) padded must be <= 0
        polarity_maintained = True
        if expl_0.score_raw > 0.1:
            polarity_maintained = (expl_5.score_raw >= 0) and (expl_15.score_raw >= 0)
        elif expl_0.score_raw < -0.1:
            polarity_maintained = (expl_5.score_raw <= 0) and (expl_15.score_raw <= 0)
            
        test_valid = magnitude_decreased and polarity_maintained
        
        dilution_results.append({
            "text": base,
            "base": {"score": expl_0.score, "raw": expl_0.score_raw, "ratio": expl_0.effective_buffer_ratio},
            "padded_5": {"score": expl_5.score, "raw": expl_5.score_raw, "ratio": expl_5.effective_buffer_ratio},
            "padded_15": {"score": expl_15.score, "raw": expl_15.score_raw, "ratio": expl_15.effective_buffer_ratio},
            "is_diluted": bool(test_valid)
        })
    
    results["dilution_test"] = dilution_results

    # Recency Bias: pos then neg vs neg then pos
    # Use a clear pivot trigger 'but'
    pos_part = "I love this absolute masterpiece of a product, it is literally fantastic and perfect."
    neg_part = "but unfortunately the shipping was terrible and the support was awful."
    
    text_pn = pos_part + " " + neg_part
    text_np = neg_part + " " + pos_part
    
    expl_pn = engine.explain(text_pn)
    expl_np = engine.explain(text_np)
    
    # In SGW, the final sentiment should dominate (Recency Bias)
    results["reordering_test"] = {
        "pos_then_neg": {"score": expl_pn.score, "raw": expl_pn.score_raw},
        "neg_then_pos": {"score": expl_np.score, "raw": expl_np.score_raw},
        "recency_bias_valid": expl_pn.score < 0 and expl_np.score > 0,
    }

    # 3. Truncation Stability (Mixed Sentiment)
    # Saturation probe: does every prefix score +/- 1.0?
    mixed_text = (
        "The beginning was actually quite promising and I felt happy. " * 5 + 
        "But then everything went wrong and I felt miserable. " * 5
    )
    tokens = mixed_text.split()
    scores_trunc = []
    for i in range(1, 21):
        step_text = " ".join(tokens[:(i * 5)])
        expl = engine.explain(step_text)
        scores_trunc.append({
            "length": len(step_text),
            "score": expl.score,
            "raw": expl.score_raw
        })
    
    results["truncation_stability"] = {
        "samples": scores_trunc,
        "raw_variance": float(np.var([s["raw"] for s in scores_trunc])),
        "score_variance": float(np.var([s["score"] for s in scores_trunc])),
    }

    # 4. Out-of-Vocabulary Robustness
    expl_oov = engine.explain("I am absolutely thrilled with this gorgonzola-flabbergasted widget.")
    results["oov_robustness"] = {
        "score": expl_oov.score,
        "raw": expl_oov.score_raw,
        "is_nonzero": abs(expl_oov.score) > 0,
    }

    logger.info("STRESS TESTS COMPLETE")
    
    os.makedirs(os.path.dirname(save_path), exist_ok=True)
    with open(save_path, "w") as f:
        json.dump(results, f, indent=2)
    
    return results

if __name__ == "__main__":
    from sgw.utils.vectors import load_vectors
    from sgw.core.valence.scorer import ValenceScorer
    from sgw.pipeline.syntax import SyntaxParser
    
    vectors = load_vectors(limit=100000)
    scorer = ValenceScorer(vectors)
    parser = SyntaxParser()
    engine = SGWEngine(scorer, parser)
    
    run_stress_tests(engine)
