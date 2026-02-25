"""Gate Check 2: Verify all core modules work independently."""

import numpy as np

print("=" * 60)
print("GATE CHECK 2: Core Module Validation")
print("=" * 60)

# Test 1: Valence scorer axis quality
from sgw.utils.vectors import load_vectors
from sgw.core.valence.scorer import ValenceScorer

vectors = load_vectors(limit=200000)
scorer = ValenceScorer(vectors)
validation = scorer.validate_axis()
assert validation["pos_all_positive"], "Axis fails: positive words not all positive"
assert validation["neg_all_negative"], "Axis fails: negative words not all negative"
assert validation["separation"] > 0.2, f"Axis separation too low: {validation['separation']}"
print(f"[PASS] Valence axis: separation={validation['separation']:.3f}")

# Test 2: Decay function mathematical properties
from sgw.core.decay.functions import compute_decay_weights
weights = compute_decay_weights(100, 1.0)
assert weights[0] == 1.0, "First position weight must be 1.0"
assert all(weights[i] >= weights[i+1] for i in range(len(weights)-1)), "Weights must be monotonically decreasing"
assert weights[-1] > 0, "Weights must never reach exactly 0"
print(f"[PASS] Decay: monotonically decreasing, first={weights[0]:.3f}, last={weights[-1]:.3f}")

# Test 3: Dilution mathematical properties
from sgw.core.dilution.buffer import dilution_factor
assert dilution_factor(0.0, 2.0) == 1.0, "Zero buffer ratio must produce no dilution"
assert dilution_factor(1.0, 2.0) < 0.2, "Full buffer ratio must produce strong dilution"
d_half = dilution_factor(0.5, 2.0)
assert 0.3 < d_half < 0.5, f"Half buffer dilution out of range: {d_half}"
print(f"[PASS] Dilution: D(0)=1.0, D(0.5)={d_half:.3f}, D(1.0)={dilution_factor(1.0, 2.0):.3f}")

# Test 4: SGW Engine produces scores in correct range
from sgw.pipeline.syntax import SyntaxParser
from sgw.core.engine import SGWEngine

parser = SyntaxParser()
engine = SGWEngine(scorer, parser)

test_cases = [
    ("I am extremely happy today!", "positive"),
    ("This is absolutely terrible and disgusting.", "negative"),
    ("The table is in the room.", "neutral"),
    ("", "zero"),
    ("I am not happy.", "should be negative due to negation"),
]

for text, expected_direction in test_cases:
    score = engine.score(text)
    assert -1.0 <= score <= 1.0, f"Score out of range for '{text}': {score}"
    print(f"  '{text[:50]}...' -> {score:.4f} ({expected_direction})")

# Verify extreme positive scores positive
pos_score = engine.score("I am extremely happy today!")
assert pos_score > 0, f"Happy sentence should be positive, got {pos_score}"

# Verify extreme negative scores negative
neg_score = engine.score("This is absolutely terrible and disgusting.")
assert neg_score < 0, f"Terrible sentence should be negative, got {neg_score}"

# Verify empty returns 0
assert engine.score("") == 0.0
assert engine.score("   ") == 0.0

print("[PASS] SGW Engine: scores in range, directional correctness verified")

# Test 5: Unit tests all pass (decay + dilution only — skip valence tests that need full vectors)
import subprocess
result = subprocess.run(["python", "-m", "pytest", "tests/unit/test_core.py", "-v", "--tb=short",
                         "-k", "not TestValenceScorer"],
                       capture_output=True, text=True,
                       env={"PATH": subprocess.os.environ["PATH"],
                            "PYTHONPATH": subprocess.os.environ.get("PYTHONPATH", ""),
                            "HOME": subprocess.os.environ.get("HOME", ""),
                            "VIRTUAL_ENV": subprocess.os.environ.get("VIRTUAL_ENV", "")})
print(result.stdout)
if result.returncode != 0:
    print(result.stderr)
    raise AssertionError("Unit tests failed. Fix before proceeding.")
print("[PASS] All unit tests pass")

print("=" * 60)
print("GATE CHECK 2: ALL TESTS PASSED")
print("=" * 60)
