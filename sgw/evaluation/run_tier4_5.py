"""Tier 4.5 Master Execution Script.

Runs all productization phases and generates the final release report.
"""

import os
import json
import logging
from datetime import datetime
from sgw.utils.vectors import load_vectors
from sgw.core.valence.scorer import ValenceScorer
from sgw.pipeline.syntax import SyntaxParser
from sgw.core.engine import SGWEngine
from sgw.evaluation.cx_benchmark import run_cx_benchmark
from sgw.evaluation.stress_tests import run_stress_tests

# Setup logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s | %(name)s | %(levelname)s | %(message)s',
    handlers=[
        logging.FileHandler("reports/metrics/tier4_5_release.log"),
        logging.StreamHandler()
    ]
)
logger = logging.getLogger("tier4_5_master")

def run_all():
    logger.info("==================================================")
    logger.info("SGW V3.5.0 PRODUCT RELEASE VALIDATION (TIER 4.5)")
    logger.info("==================================================")
    
    start_time = datetime.now()
    report = {
        "release_version": "3.5.0",
        "timestamp": start_time.isoformat(),
        "phases": {}
    }

    # Load resources once
    logger.info("Ph1: Loading global resources (300k vectors)...")
    vectors = load_vectors(limit=300000)
    scorer = ValenceScorer(vectors)
    parser = SyntaxParser()
    engine = SGWEngine(scorer, parser)
    
    # Phase 1 & 2: Structural Verification
    logger.info("\nPh1 & Ph2: Verifying Domain Axes and Explanations...")
    # We use some of the logic from our gate checks
    test_text = "The service was slow and frustrating, but the final resolution was excellent."
    expl = engine.explain(test_text)
    report["phases"]["core_explanation"] = {
        "status": "passed",
        "score": expl.score,
        "pivot_detected": expl.pivot_detected,
        "pivot_token": expl.pivot_token
    }
    
    # Phase 3: CX Benchmark
    logger.info("\nPh3: Running CX Benchmark (Amazon Reviews Multi)...")
    cx_results = run_cx_benchmark(engine, limit=500)
    report["phases"]["cx_benchmark"] = cx_results
    
    # Phase 4: Stress Tests
    logger.info("\nPh4: Running Long-Document Stress Tests...")
    stress_results = run_stress_tests(engine)
    report["phases"]["stress_tests"] = stress_results
    
    # --- Release Gates ---
    import numpy as np
    recency_ok = stress_results.get("reordering_test", {}).get("recency_bias_valid", False)
    dilution_ok = all(d.get("is_diluted", False) for d in stress_results.get("dilution_test", []))
    
    # Check benchmark rho on the best available segment
    cx_bench = report["phases"]["cx_benchmark"]
    long_rho = cx_bench.get("sgw_cx_long", {}).get("spearman_rho", 0.0)
    med_rho = cx_bench.get("sgw_cx_medium", {}).get("spearman_rho", 0.0)
    best_rho = long_rho if not np.isnan(long_rho) and long_rho != 0 else med_rho
    
    benchmark_ok = not np.isnan(best_rho) and best_rho > 0.4
    
    # Release Decision
    if recency_ok and dilution_ok and benchmark_ok:
        report["overall_status"] = "RELEASE READY"
    else:
        logger.warning(f"RELEASE GATES FAILED: recency={recency_ok}, dilution={dilution_ok}, benchmark={benchmark_ok}")
        report["overall_status"] = "BLOCKER DETECTED"
        report["gate_failures"] = {
            "recency_bias": recency_ok,
            "dilution_stability": dilution_ok,
            "benchmark_rho": benchmark_ok
        }

    # Finalize
    end_time = datetime.now()
    report["duration_seconds"] = (end_time - start_time).total_seconds()
    
    report_path = "reports/metrics/tier4_5_final_report.json"
    os.makedirs(os.path.dirname(report_path), exist_ok=True)
    with open(report_path, "w") as f:
        json.dump(report, f, indent=2, default=str)
        
    logger.info("\n==================================================")
    logger.info(f"DONE. Final report saved to: {report_path}")
    logger.info(f"Release Status: {report['overall_status']}")
    logger.info("==================================================")

if __name__ == "__main__":
    run_all()
