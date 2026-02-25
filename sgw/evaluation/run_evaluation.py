"""Master evaluation script. Runs calibration, ablation, and final benchmarks.

This is the script that determines whether SGW meets its success criteria.
"""

import json
import sys
import numpy as np
from sgw.utils.vectors import load_vectors
from sgw.utils.config import load_config
from sgw.core.valence.scorer import ValenceScorer
from sgw.pipeline.syntax import SyntaxParser
from sgw.core.engine import SGWEngine
from sgw.evaluation.goemotions_loader import load_goemotions
from sgw.evaluation.baselines import compute_vader_scores
from sgw.evaluation.metrics import evaluate_continuous
from sgw.evaluation.calibrate import calibrate
from sgw.evaluation.ablation import run_ablation
from sgw.utils.logger import get_logger

logger = get_logger(__name__)


def main():
    print("=" * 70)
    print("SGW V3 FULL EVALUATION PIPELINE")
    print("=" * 70)

    # --- Setup ---
    logger.info("Loading vectors...")
    vectors = load_vectors(limit=500000)
    scorer = ValenceScorer(vectors)
    parser = SyntaxParser()
    engine = SGWEngine(scorer, parser)

    # --- Phase 1: Calibrate on validation set ---
    logger.info("\n" + "=" * 50)
    logger.info("PHASE 1: CALIBRATION (validation set)")
    logger.info("=" * 50)
    best_lambda, best_k = calibrate(engine)
    logger.info(f"Optimal parameters: λ={best_lambda}, k={best_k}")

    # --- Phase 1b: Variance-Matching Calibration ---
    logger.info("\n" + "=" * 50)
    logger.info("PHASE 1b: VARIANCE-MATCHING SCALE CALIBRATION")
    logger.info("=" * 50)
    df_val = load_goemotions("validation")
    y_true_val = df_val["human_valence"].values
    
    # Temporarily remove affine scaler to measure native variance
    engine.set_calibration_scale(0.0, 1.0, 1.0)
    y_sgw_val = engine.score_batch(df_val["text"].tolist(), show_progress=False)
    
    mu_s = float(np.mean(y_sgw_val))
    sigma_s = float(np.std(y_sgw_val))
    sigma_y = float(np.std(y_true_val))
    
    logger.info(f"Native metrics: mu_S={mu_s:.4f}, sigma_S={sigma_s:.4f}, true sigma_Y={sigma_y:.4f}")
    engine.set_calibration_scale(mu_s, sigma_s, sigma_y)
    logger.info("Variance-Matching applied.")

    # --- Phase 2: Evaluate on test set ---
    logger.info("\n" + "=" * 50)
    logger.info("PHASE 2: FINAL EVALUATION (test set)")
    logger.info("=" * 50)

    df_test = load_goemotions("test")
    texts = df_test["text"].tolist()
    y_true = df_test["human_valence"].values

    # SGW scores
    y_sgw = engine.score_batch(texts, show_progress=True)
    sgw_results = evaluate_continuous(y_true, y_sgw, "SGW (Calibrated)")

    # VADER scores
    y_vader = compute_vader_scores(df_test["text"])
    vader_results = evaluate_continuous(y_true, y_vader, "VADER")

    # --- Phase 3: Ablation studies ---
    logger.info("\n" + "=" * 50)
    logger.info("PHASE 3: ABLATION STUDIES")
    logger.info("=" * 50)
    ablation = run_ablation(engine)

    # --- Phase 4: Gate checks ---
    logger.info("\n" + "=" * 50)
    logger.info("PHASE 4: SUCCESS CRITERIA VERIFICATION")
    logger.info("=" * 50)

    gates = {}

    # Gate 1: Spearman rho >= 0.35
    gates["spearman_ge_035"] = sgw_results["spearman_rho"] >= 0.35
    logger.info(f"Gate 1 - Spearman ρ ≥ 0.35: {sgw_results['spearman_rho']:.4f} -> "
                f"{'PASS' if gates['spearman_ge_035'] else 'FAIL'}")

    # Gate 2: Beat VADER
    gates["beats_vader"] = sgw_results["spearman_rho"] > vader_results["spearman_rho"]
    logger.info(f"Gate 2 - Beats VADER: SGW ρ={sgw_results['spearman_rho']:.4f} vs "
                f"VADER ρ={vader_results['spearman_rho']:.4f} -> "
                f"{'PASS' if gates['beats_vader'] else 'FAIL'}")

    # Gate 3: MAE <= 0.45
    gates["mae_le_045"] = sgw_results["mae"] <= 0.45
    logger.info(f"Gate 3 - MAE ≤ 0.45: {sgw_results['mae']:.4f} -> "
                f"{'PASS' if gates['mae_le_045'] else 'FAIL'}")

    # Gate 4: Decay helps
    gates["decay_helps"] = ablation["decay_contribution"] > 0
    logger.info(f"Gate 4 - Decay helps: Δρ={ablation['decay_contribution']:.4f} -> "
                f"{'PASS' if gates['decay_helps'] else 'FAIL'}")

    # Gate 5: Dilution helps
    gates["dilution_helps"] = ablation["dilution_contribution"] > 0
    logger.info(f"Gate 5 - Dilution helps: Δρ={ablation['dilution_contribution']:.4f} -> "
                f"{'PASS' if gates['dilution_helps'] else 'FAIL'}")

    # --- Phase 5: Structure Validation Benchmarks ---
    logger.info("\n" + "=" * 50)
    logger.info("PHASE 5: ADDITIONAL BENCHMARKS")
    logger.info("=" * 50)
    
    from sgw.evaluation.benchmark_loaders import load_benchmark
    benchmark_results = {}
    
    for b_name in ["sst5", "yelp", "imdb"]:
        try:
            df_b = load_benchmark(b_name, split="test", limit=5000)
            b_texts = df_b["text"].tolist()
            b_y_true = df_b["human_valence"].values
            
            b_y_sgw = engine.score_batch(b_texts, show_progress=True)
            b_res_sgw = evaluate_continuous(b_y_true, b_y_sgw, f"SGW ({b_name})")
            
            b_y_vader = compute_vader_scores(b_texts)
            b_res_vader = evaluate_continuous(b_y_true, b_y_vader, f"VADER ({b_name})")
            
            benchmark_results[b_name] = {"sgw": b_res_sgw, "vader": b_res_vader}
            logger.info(f"{b_name.upper()} -> SGW ρ={b_res_sgw['spearman_rho']:.4f} vs VADER ρ={b_res_vader['spearman_rho']:.4f}")
        except Exception as e:
            logger.error(f"Failed to run benchmark {b_name}: {e}")

    # --- Save all results ---
    final_report = {
        "calibrated_lambda": best_lambda,
        "calibrated_k": best_k,
        "variance_matching": {"mu_s": mu_s, "sigma_s": sigma_s, "sigma_y": sigma_y},
        "sgw_results": sgw_results,
        "vader_results": vader_results,
        "ablation": {
            "full_rho": ablation["full_model"]["spearman_rho"],
            "no_decay_rho": ablation["no_decay"]["spearman_rho"],
            "no_dilution_rho": ablation["no_dilution"]["spearman_rho"],
            "valence_only_rho": ablation["valence_only"]["spearman_rho"],
            "decay_contribution": ablation["decay_contribution"],
            "dilution_contribution": ablation["dilution_contribution"],
        },
        "benchmarks": benchmark_results,
        "gates": gates,
        "all_gates_passed": all(gates.values()),
    }

    with open("reports/metrics/final_evaluation.json", "w") as f:
        json.dump(final_report, f, indent=2, default=str)

    # --- Final verdict ---
    print("\n" + "=" * 70)
    if all(gates.values()):
        print("ALL GATES PASSED. Proceed to Tier 4 (deployment and polish).")
    else:
        failed = [k for k, v in gates.items() if not v]
        print(f"GATES FAILED: {failed}")
        print("DO NOT PROCEED TO TIER 4.")
        print("Debug the failing components and re-run this evaluation.")
        print("If decay or dilution gates fail, the mathematical contributions")
        print("may not be significant on this dataset. Consider:")
        print("  1. Reviewing the seed word list for the valence axis")
        print("  2. Expanding the lambda/k search ranges")
        print("  3. Checking if the GoEmotions valence mapping is appropriate")
        sys.exit(1)
    print("=" * 70)


if __name__ == "__main__":
    main()
