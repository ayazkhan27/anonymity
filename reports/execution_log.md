# SGW V3 — Execution Log & Metrics Report

> **Live log**: Updated after each gate check. Review this to decide if the scoring formula needs adjustment.
> **Repo**: `.`

---

## Tier 0 — Environment Setup ✅

| Check | Result |
|-------|--------|
| Python version | 3.10.12 |
| spaCy model | `en_core_web_sm` 3.8.0 (using sm instead of trf — torch had library issues; plan notes this is acceptable for non-GPU) |
| All imports | ✅ pandas, numpy, scipy, sklearn, gensim, fastapi, datasets, yaml |
| Directory structure | ✅ All 18 required directories present |

> **Note**: All files stored under `/mnt` — `/home` has no space.

---

## Tier 1 — Data Infrastructure ✅

### GoEmotions Dataset
| Metric | Value |
|--------|-------|
| Test split size | 5,427 examples |
| Valence mean | 0.148 |
| Valence std | 0.505 |
| Valence range | [-0.840, 0.900] |

### VADER Baseline (the bar to beat)
| Metric | Value |
|--------|-------|
| **Spearman ρ** | **0.5988** |
| Kendall τ | 0.4473 |
| CCC | 0.5951 |
| MAE | 0.3198 |
| RMSE | 0.4383 |
| Sign agreement | 0.8188 |

> **Key insight**: VADER ρ = 0.5988 is surprisingly strong on GoEmotions. SGW must beat this. The full metrics are saved to `reports/metrics/vader_baseline.json`.

---

## Tier 2 — Core Engine Modules ⏳

### GloVe Download
- Status: **Downloading** (~2GB zip, ~5.5GB extracted)
- Target: `data/vectors/glove.840B.300d.w2v`

### Gate Check 2 Results

| Test | Status | Details |
|------|--------|---------|
| Valence axis separation | ✅ | separation=0.836, pos_mean=0.404, neg_mean=-0.432, neu_mean=0.071 |
| Decay monotonicity | ✅ | first=1.000, last=0.372 (λ=1.0, 100 tokens) |
| Dilution math | ✅ | D(0)=1.0, D(0.5)=0.368, D(1.0)=0.135 (k=2.0) |
| Engine: "I am extremely happy today!" | ✅ | +0.0825 (positive) |
| Engine: "This is absolutely terrible and disgusting." | ✅ | -0.0905 (negative) |
| Engine: "The table is in the room." | ✅ | +0.0257 (near-neutral) |
| Engine: "" (empty) | ✅ | 0.0000 |
| Engine: "I am not happy." (negation) | ⚠️ | +0.0417 — negation didn't fully flip. Likely because `en_core_web_sm` dependency parse is weaker. |
| Unit tests (10/10) | ✅ | 10 passed, 4 deselected (valence needs full vectors) in 0.08s |

> **⚠️ Negation weakness**: "I am not happy" scored slightly positive. This is because `en_core_web_sm` has weaker dependency parsing than `en_core_web_trf`. The negation handler relies on `dep_="neg"` relationships. This may affect overall Spearman ρ but is not a gate blocker — the plan's gate check only requires positive/negative directional correctness for strongly worded sentences.

---

## Tier 3 — Calibration & Evaluation ✅

### Calibration Grid Search
- Best parameters found on validation subset: **λ = 0.1, k = 0.5** (ρ = 0.4954)
- *Full grid search results saved to `reports/metrics/calibration_results.json`.*

### SGW vs VADER on Test Set (N=5427)
| Metric | SGW | VADER | Δ |
|--------|-----|-------|---|
| Spearman ρ | 0.4994 | 0.5988 | -0.0994 |
| MAE | 0.3954 | 0.3198 | +0.0756 |
| Sign agreement | 0.7435 | 0.8188 | -0.0753 |

### Ablation Study
| Configuration | Spearman ρ | Δ from full |
|--------------|-----------|-------------|
| Full model (decay + dilution) | 0.4994 | — |
| No decay | 0.4990 | -0.0004 |
| No dilution | 0.4989 | -0.0005 |
| Valence only | 0.4984 | -0.0010 |

### Success Criteria Gates
| Gate | Threshold | Result | Status |
|------|-----------|--------|--------|
| Spearman ρ ≥ 0.35 | 0.35 | 0.4994 | **PASS** ✅ |
| Beats VADER | ρ_SGW > ρ_VADER | SGW: 0.4994 < 0.5988 | **FAIL** ❌ |
| MAE ≤ 0.45 | 0.45 | 0.3954 | **PASS** ✅ |
| Decay helps | Δρ > 0 | Δ = +0.0004 | **PASS** ✅ |
| Dilution helps | Δρ > 0 | Δ = +0.0005 | **PASS** ✅ |

---

## Decision Points (if gates fail)

If SGW doesn't beat VADER, likely causes and fixes:
1. **Seed word list** — try adding domain-specific words to `configs/sgw_config.yaml`
2. **Normalization** — `engine.py` divides by emotion token count; try summing without normalizing
3. **Buffer POS tags** — list may be too broad or narrow in config
4. **Lambda/k ranges** — expand search grid in config

If decay/dilution don't help (Gates 4-5):
- GoEmotions comments average ~12 words — too short for positional effects
- This is a **legitimate finding** — document it and test on longer-form data
