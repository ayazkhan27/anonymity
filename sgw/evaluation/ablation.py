"""Ablation studies: measure the contribution of each SGW component."""

import json
import numpy as np
from sgw.core.engine import SGWEngine
from sgw.evaluation.metrics import evaluate_continuous
from sgw.evaluation.goemotions_loader import load_goemotions
from sgw.utils.logger import get_logger

logger = get_logger(__name__)


def run_ablation(engine: SGWEngine, save_path: str = "reports/metrics/ablation_results.json"):
    """Run ablation studies on the SGW engine.

    Tests four configurations:
    1. Full model (decay + dilution)
    2. No decay (dilution only)
    3. No dilution (decay only)
    4. No decay, no dilution (valence only — raw SemAxis projection)
    """
    df_test = load_goemotions("test")
    texts = df_test["text"].tolist()
    y_true = df_test["human_valence"].values

    results = {}

    # Configuration 1: Full model
    engine.enable_decay()
    engine.enable_dilution()
    y_pred = engine.score_batch(texts, show_progress=True)
    results["full_model"] = evaluate_continuous(y_true, y_pred, "Full SGW")

    # Configuration 2: No decay
    engine.disable_decay()
    engine.enable_dilution()
    y_pred = engine.score_batch(texts, show_progress=True)
    results["no_decay"] = evaluate_continuous(y_true, y_pred, "No Decay")

    # Configuration 3: No dilution
    engine.enable_decay()
    engine.disable_dilution()
    y_pred = engine.score_batch(texts, show_progress=True)
    results["no_dilution"] = evaluate_continuous(y_true, y_pred, "No Dilution")

    # Configuration 4: Valence only
    engine.disable_decay()
    engine.disable_dilution()
    y_pred = engine.score_batch(texts, show_progress=True)
    results["valence_only"] = evaluate_continuous(y_true, y_pred, "Valence Only")

    # Re-enable for future use
    engine.enable_decay()
    engine.enable_dilution()

    # Compute deltas
    full_rho = results["full_model"]["spearman_rho"]
    results["decay_contribution"] = full_rho - results["no_decay"]["spearman_rho"]
    results["dilution_contribution"] = full_rho - results["no_dilution"]["spearman_rho"]

    logger.info(f"\n--- Ablation Summary ---")
    logger.info(f"Full model ρ:     {results['full_model']['spearman_rho']:.4f}")
    logger.info(f"No decay ρ:       {results['no_decay']['spearman_rho']:.4f} (Δ={results['decay_contribution']:.4f})")
    logger.info(f"No dilution ρ:    {results['no_dilution']['spearman_rho']:.4f} (Δ={results['dilution_contribution']:.4f})")
    logger.info(f"Valence only ρ:   {results['valence_only']['spearman_rho']:.4f}")

    with open(save_path, "w") as f:
        json.dump(results, f, indent=2, default=str)
    logger.info(f"Ablation results saved to {save_path}")

    return results
