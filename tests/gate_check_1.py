"""Gate Check 1: Verify data loading, valence mapping, and VADER baseline."""

import numpy as np
from sgw.evaluation.goemotions_loader import load_goemotions, EMOTION_VALENCE_MAP
from sgw.evaluation.baselines import compute_vader_scores
from sgw.evaluation.metrics import evaluate_continuous

print("=" * 60)
print("GATE CHECK 1: Data and Baseline Validation")
print("=" * 60)

# Test 1: GoEmotions loads correctly
df = load_goemotions("test")
assert len(df) > 4000, f"GoEmotions test set too small: {len(df)}"
assert "text" in df.columns
assert "human_valence" in df.columns
assert df["human_valence"].min() >= -1.0
assert df["human_valence"].max() <= 1.0
print(f"[PASS] GoEmotions loaded: {len(df)} examples")

# Test 2: Valence mapping is reasonable
assert EMOTION_VALENCE_MAP["joy"] > 0.5, "Joy should be strongly positive"
assert EMOTION_VALENCE_MAP["sadness"] < -0.5, "Sadness should be strongly negative"
assert EMOTION_VALENCE_MAP["neutral"] == 0.0, "Neutral should be exactly 0"
assert abs(EMOTION_VALENCE_MAP["surprise"]) < 0.3, "Surprise should be near-neutral"
print("[PASS] Valence mapping sanity checks")

# Test 3: Distribution has variance (not all zeros)
assert df["human_valence"].std() > 0.1, "Valence has insufficient variance"
assert (df["human_valence"] > 0).sum() > 100, "Too few positive examples"
assert (df["human_valence"] < 0).sum() > 100, "Too few negative examples"
print(f"[PASS] Valence distribution: mean={df['human_valence'].mean():.3f}, std={df['human_valence'].std():.3f}")

# Test 4: VADER baseline runs and produces scores
vader_scores = compute_vader_scores(df["text"])
assert len(vader_scores) == len(df)
assert np.isfinite(vader_scores).all()
assert vader_scores.min() >= -1.0
assert vader_scores.max() <= 1.0
print(f"[PASS] VADER scores computed: mean={vader_scores.mean():.3f}")

# Test 5: VADER baseline metrics
vader_results = evaluate_continuous(
    df["human_valence"].values, vader_scores, method_name="VADER Baseline"
)
assert abs(vader_results["spearman_rho"]) > 0.05, "VADER should have some correlation"
print(f"[PASS] VADER Spearman ρ = {vader_results['spearman_rho']:.4f}")

# Save VADER baseline for later comparison
import json
with open("reports/metrics/vader_baseline.json", "w") as f:
    json.dump(vader_results, f, indent=2)
print(f"[PASS] VADER baseline saved to reports/metrics/vader_baseline.json")

print("=" * 60)
print("GATE CHECK 1: ALL TESTS PASSED")
print("=" * 60)
