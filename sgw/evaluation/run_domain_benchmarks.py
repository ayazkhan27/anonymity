"""Domain benchmark runner for SGW.

Evaluates SGW against VADER across domain-specific datasets,
demonstrating domain adaptation and long-form text advantages.
"""

import json
import sys
import numpy as np
import pandas as pd
from pathlib import Path
from sgw.utils.vectors import load_vectors
from sgw.utils.config import load_config
from sgw.core.valence.scorer import ValenceScorer
from sgw.core.valence.axis import SentimentAxis
from sgw.pipeline.syntax import SyntaxParser
from sgw.core.engine import SGWEngine
from sgw.evaluation.benchmark_loaders import load_benchmark
from sgw.evaluation.baselines import compute_vader_scores
from sgw.evaluation.metrics import evaluate_continuous
from sgw.utils.logger import get_logger

logger = get_logger(__name__)

FINANCIAL_BENCHMARKS = ["fiqa", "phrasebank", "aiera"]
GENERAL_BENCHMARKS = ["imdb"]
ALL_BENCHMARKS = FINANCIAL_BENCHMARKS + GENERAL_BENCHMARKS

MINIMUM_SAMPLES_FOR_SEGMENT = 30  # Don't report metrics on fewer than this


def run_single_benchmark(engine, df, dataset_name, axis_name, vader_scores=None):
    """Run SGW and VADER evaluation on a single dataset.

    Returns dict with sgw metrics, vader metrics, and length-segmented results.
    """
    texts = df["text"].tolist()
    y_true = df["human_valence"].values

    # SGW scores
    y_sgw = engine.score_batch(texts, show_progress=True)
    sgw_metrics = evaluate_continuous(y_true, y_sgw, f"SGW ({dataset_name}/{axis_name})")

    # VADER scores (compute once, reuse across axes)
    if vader_scores is None:
        vader_scores = compute_vader_scores(df["text"])
    vader_metrics = evaluate_continuous(y_true, vader_scores, f"VADER ({dataset_name})")

    result = {
        "sgw": sgw_metrics,
        "vader": vader_metrics,
        "axis_used": axis_name,
        "n_samples": len(df),
        "text_length_median": float(df["text"].str.len().median()),
        "text_length_mean": float(df["text"].str.len().mean()),
        "sgw_beats_vader": sgw_metrics["spearman_rho"] > vader_metrics["spearman_rho"],
    }

    # Length-segmented analysis
    df_eval = df.copy()
    df_eval["sgw_score"] = y_sgw
    df_eval["vader_score"] = vader_scores
    df_eval["text_length"] = df_eval["text"].str.len()

    segments = {
        "short": df_eval[df_eval["text_length"] < 100],
        "medium": df_eval[(df_eval["text_length"] >= 100) & (df_eval["text_length"] < 500)],
        "long": df_eval[df_eval["text_length"] >= 500],
    }

    for seg_name, seg_df in segments.items():
        if len(seg_df) >= MINIMUM_SAMPLES_FOR_SEGMENT:
            seg_y_true = seg_df["human_valence"].values
            seg_y_sgw = seg_df["sgw_score"].values
            seg_y_vader = seg_df["vader_score"].values

            # Check for zero variance (causes NaN in correlation)
            if np.std(seg_y_true) < 1e-6 or np.std(seg_y_sgw) < 1e-6:
                logger.warning(f"Skipping {seg_name} segment for {dataset_name}: "
                              f"zero variance detected")
                continue

            seg_sgw = evaluate_continuous(seg_y_true, seg_y_sgw,
                                          f"SGW ({dataset_name}/{seg_name})")
            seg_vader = evaluate_continuous(seg_y_true, seg_y_vader,
                                            f"VADER ({dataset_name}/{seg_name})")
            result[f"sgw_{seg_name}"] = seg_sgw
            result[f"vader_{seg_name}"] = seg_vader
            result[f"n_{seg_name}"] = len(seg_df)

    return result, vader_scores


def main():
    print("=" * 70)
    print("SGW DOMAIN BENCHMARK SUITE")
    print("=" * 70)

    # --- Setup ---
    vectors = load_vectors(limit=500000)
    config = load_config()
    scorer = ValenceScorer(vectors, config)
    parser = SyntaxParser(config)
    engine = SGWEngine(scorer, parser, config)

    # Load calibration if available
    cal_path = Path("reports/metrics/calibration_results.json")
    if cal_path.exists():
        with open(cal_path) as f:
            cal = json.load(f)
        engine.set_lambda(cal["best_lambda"])
        engine.set_k(cal["best_k"])
        logger.info(f"Loaded calibration: lambda={cal['best_lambda']}, k={cal['best_k']}")

    final_path = Path("reports/metrics/final_evaluation.json")
    if final_path.exists():
        with open(final_path) as f:
            final = json.load(f)
        vm = final.get("variance_matching", {})
        if vm and vm.get("sigma_s", 1.0) != 1.0:
            engine.set_calibration_scale(vm["mu_s"], vm["sigma_s"], vm["sigma_y"])
            logger.info("Loaded variance-matching calibration.")

    # Build axes
    general_axis = SentimentAxis.from_preset(vectors, "general")
    finance_axis = SentimentAxis.from_preset(vectors, "finance")

    all_results = {}

    # ================================================================
    # FINANCIAL BENCHMARKS: Test with both general and finance axes
    # ================================================================
    for bench_name in FINANCIAL_BENCHMARKS:
        print(f"\n{'=' * 50}")
        print(f"BENCHMARK: {bench_name.upper()}")
        print(f"{'=' * 50}")

        try:
            # Use split='all' for FiQA since it's small (1,174 total)
            # Use split='test' for others
            use_split = "all" if bench_name == "fiqa" else "test"
            df = load_benchmark(bench_name, split=use_split, limit=5000)
        except Exception as e:
            logger.error(f"Failed to load {bench_name}: {e}")
            all_results[bench_name] = {"status": "load_failed", "error": str(e)}
            continue

        if len(df) == 0:
            logger.warning(f"No data for {bench_name}. Skipping.")
            all_results[bench_name] = {"status": "empty"}
            continue

        # Compute VADER once (same regardless of axis)
        vader_scores = compute_vader_scores(df["text"])

        # Test 1: General axis
        logger.info(f"\n--- {bench_name} with GENERAL axis ---")
        scorer.swap_axis(general_axis)
        result_general, _ = run_single_benchmark(
            engine, df, bench_name, "general", vader_scores
        )

        # Test 2: Finance axis
        logger.info(f"\n--- {bench_name} with FINANCE axis ---")
        scorer.swap_axis(finance_axis)
        result_finance, _ = run_single_benchmark(
            engine, df, bench_name, "finance", vader_scores
        )

        # Domain adaptation delta
        delta = (result_finance["sgw"]["spearman_rho"] -
                 result_general["sgw"]["spearman_rho"])

        all_results[bench_name] = {
            "status": "ok",
            "general_axis": result_general,
            "finance_axis": result_finance,
            "domain_adaptation_delta": delta,
            "best_axis": "finance" if delta > 0 else "general",
            "best_sgw_rho": max(
                result_general["sgw"]["spearman_rho"],
                result_finance["sgw"]["spearman_rho"]
            ),
            "vader_rho": result_general["vader"]["spearman_rho"],
            "sgw_beats_vader": max(
                result_general["sgw"]["spearman_rho"],
                result_finance["sgw"]["spearman_rho"]
            ) > result_general["vader"]["spearman_rho"],
        }

        logger.info(f"\n{bench_name} SUMMARY:")
        logger.info(f"  General axis ρ = {result_general['sgw']['spearman_rho']:.4f}")
        logger.info(f"  Finance axis ρ = {result_finance['sgw']['spearman_rho']:.4f}")
        logger.info(f"  VADER ρ        = {result_general['vader']['spearman_rho']:.4f}")
        logger.info(f"  Adaptation Δ   = {delta:+.4f}")

    # ================================================================
    # GENERAL BENCHMARKS: Test with general axis only
    # ================================================================
    scorer.swap_axis(general_axis)

    for bench_name in GENERAL_BENCHMARKS:
        print(f"\n{'=' * 50}")
        print(f"BENCHMARK: {bench_name.upper()}")
        print(f"{'=' * 50}")

        try:
            df = load_benchmark(bench_name, split="test", limit=5000)
        except Exception as e:
            logger.error(f"Failed to load {bench_name}: {e}")
            all_results[bench_name] = {"status": "load_failed", "error": str(e)}
            continue

        result, _ = run_single_benchmark(engine, df, bench_name, "general")
        all_results[bench_name] = {
            "status": "ok",
            "general_axis": result,
            "sgw_rho": result["sgw"]["spearman_rho"],
            "vader_rho": result["vader"]["spearman_rho"],
            "sgw_beats_vader": result["sgw_beats_vader"],
        }

    # ================================================================
    # CROSS-DOMAIN SUMMARY
    # ================================================================
    print("\n" + "=" * 70)
    print("CROSS-DOMAIN SUMMARY")
    print("=" * 70)

    summary_rows = []
    wins = 0
    total = 0

    for bench_name, res in all_results.items():
        if res.get("status") != "ok":
            continue

        if bench_name in FINANCIAL_BENCHMARKS:
            best_rho = res["best_sgw_rho"]
            vader_rho = res["vader_rho"]
            axis = res["best_axis"]
            delta = res["domain_adaptation_delta"]
        else:
            best_rho = res["sgw_rho"]
            vader_rho = res["vader_rho"]
            axis = "general"
            delta = 0.0

        beats = best_rho > vader_rho
        if beats:
            wins += 1
        total += 1

        summary_rows.append({
            "dataset": bench_name,
            "sgw_rho": best_rho,
            "vader_rho": vader_rho,
            "winner": "SGW" if beats else "VADER",
            "margin": best_rho - vader_rho,
            "best_axis": axis,
            "adaptation_delta": delta,
        })

        status = "WIN" if beats else "LOSS"
        print(f"  {bench_name:15s} | SGW ρ={best_rho:.4f} vs VADER ρ={vader_rho:.4f} "
              f"| {status} by {abs(best_rho - vader_rho):.4f} | axis={axis}")

    print(f"\n  SGW wins {wins}/{total} benchmarks")

    all_results["summary"] = {
        "datasets_evaluated": total,
        "sgw_wins": wins,
        "details": summary_rows,
    }

    # ================================================================
    # GATE CHECK
    # ================================================================
    gate_pass = wins >= 2
    all_results["gate_beats_vader_2of4"] = gate_pass
    print(f"\n  GATE (beats VADER on ≥2 datasets): {'PASS' if gate_pass else 'FAIL'}")

    # Save
    save_path = "reports/metrics/domain_benchmarks.json"
    with open(save_path, "w") as f:
        json.dump(all_results, f, indent=2, default=str)
    print(f"\n  Results saved to {save_path}")
    print("=" * 70)

    return all_results


if __name__ == "__main__":
    main()
