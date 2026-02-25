"""Calibrate SGW hyperparameters (lambda, k) against GoEmotions."""

import json
import itertools
import numpy as np
import pandas as pd
from sgw.core.engine import SGWEngine
from sgw.evaluation.metrics import evaluate_continuous
from sgw.evaluation.goemotions_loader import load_goemotions
from sgw.utils.config import load_config
from sgw.utils.logger import get_logger

logger = get_logger(__name__)


def calibrate(engine: SGWEngine, save_path: str = "reports/metrics/calibration_results.json"):
    """Grid search over lambda and k to find optimal hyperparameters.

    Uses GoEmotions VALIDATION split for calibration (not test).
    Final evaluation uses test split.
    """
    config = load_config()
    lambda_range = config["decay"]["lambda_search_range"]
    k_range = config["dilution"]["k_search_range"]

    df_val = load_goemotions("validation")
    texts = df_val["text"].tolist()
    y_true = df_val["human_valence"].values

    logger.info(f"Calibrating on {len(texts)} validation examples.")
    logger.info(f"Lambda range: {lambda_range}")
    logger.info(f"K range: {k_range}")
    
    logger.info("Pre-parsing validation texts to speed up grid search...")
    docs = engine.syntax_parser.parse_batch(texts, show_progress=True)

    best_rho = -1.0
    best_lambda = None
    best_k = None
    all_results = []

    for lam, k in itertools.product(lambda_range, k_range):
        engine.set_lambda(lam)
        engine.set_k(k)
        engine.enable_decay()
        engine.enable_dilution()

        y_pred = engine.score_docs(docs, show_progress=False)
        metrics = evaluate_continuous(y_true, y_pred, method_name=f"λ={lam}, k={k}")

        result = {"lambda": lam, "k": k, **metrics}
        all_results.append(result)

        if metrics["spearman_rho"] > best_rho:
            best_rho = metrics["spearman_rho"]
            best_lambda = lam
            best_k = k

        logger.info(f"λ={lam:.1f}, k={k:.1f} -> ρ={metrics['spearman_rho']:.4f}, "
                     f"MAE={metrics['mae']:.4f}")

    logger.info(f"\nBest: λ={best_lambda}, k={best_k}, ρ={best_rho:.4f}")

    output = {
        "best_lambda": best_lambda,
        "best_k": best_k,
        "best_spearman_rho": best_rho,
        "all_results": all_results,
    }

    with open(save_path, "w") as f:
        json.dump(output, f, indent=2)
    logger.info(f"Calibration results saved to {save_path}")

    # Apply best parameters
    engine.set_lambda(best_lambda)
    engine.set_k(best_k)

    return best_lambda, best_k
