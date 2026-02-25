"""Evaluation metrics for SGW. All benchmark comparisons use these functions."""

import numpy as np
from scipy import stats
from sklearn.metrics import mean_absolute_error, mean_squared_error
from sgw.utils.logger import get_logger

logger = get_logger(__name__)


def concordance_correlation_coefficient(y_true: np.ndarray, y_pred: np.ndarray) -> float:
    """Compute Lin's Concordance Correlation Coefficient (CCC).

    CCC measures both the correlation and the agreement between two variables.
    It is the standard metric for dimensional emotion prediction (AVEC challenges).
    Range: [-1, 1]. Values > 0.4 are moderate agreement.

    Reference: Lin (1989). A concordance correlation coefficient to evaluate
    reproducibility. Biometrics, 45(1), 255-268.
    """
    mean_true = np.mean(y_true)
    mean_pred = np.mean(y_pred)
    var_true = np.var(y_true)
    var_pred = np.var(y_pred)
    covariance = np.mean((y_true - mean_true) * (y_pred - mean_pred))

    denominator = var_true + var_pred + (mean_true - mean_pred) ** 2
    if denominator == 0:
        return 0.0

    ccc = (2 * covariance) / denominator
    return float(ccc)


def evaluate_continuous(y_true: np.ndarray, y_pred: np.ndarray, method_name: str = "Model") -> dict:
    """Compute all continuous evaluation metrics.

    Args:
        y_true: Ground truth valence scores.
        y_pred: Predicted valence scores.
        method_name: Name for logging.

    Returns:
        dict with keys: spearman_rho, spearman_p, kendall_tau, kendall_p,
                        ccc, mae, rmse, sign_agreement
    """
    # Remove NaN/Inf
    mask = np.isfinite(y_true) & np.isfinite(y_pred)
    y_true = y_true[mask]
    y_pred = y_pred[mask]

    spearman_rho, spearman_p = stats.spearmanr(y_true, y_pred)
    kendall_tau, kendall_p = stats.kendalltau(y_true, y_pred)
    ccc = concordance_correlation_coefficient(y_true, y_pred)
    mae = mean_absolute_error(y_true, y_pred)
    rmse = np.sqrt(mean_squared_error(y_true, y_pred))

    # Sign agreement: what fraction have the same sign (pos/neg)?
    # Exclude exact zeros from both
    nonzero_mask = (y_true != 0) & (y_pred != 0)
    if nonzero_mask.sum() > 0:
        sign_agree = (np.sign(y_true[nonzero_mask]) == np.sign(y_pred[nonzero_mask])).mean()
    else:
        sign_agree = 0.0

    results = {
        "spearman_rho": float(spearman_rho),
        "spearman_p": float(spearman_p),
        "kendall_tau": float(kendall_tau),
        "kendall_p": float(kendall_p),
        "ccc": float(ccc),
        "mae": float(mae),
        "rmse": float(rmse),
        "sign_agreement": float(sign_agree),
        "n_samples": int(len(y_true)),
    }

    logger.info(f"--- {method_name} Evaluation ---")
    logger.info(f"  Spearman ρ = {results['spearman_rho']:.4f} (p={results['spearman_p']:.2e})")
    logger.info(f"  Kendall τ  = {results['kendall_tau']:.4f} (p={results['kendall_p']:.2e})")
    logger.info(f"  CCC        = {results['ccc']:.4f}")
    logger.info(f"  MAE        = {results['mae']:.4f}")
    logger.info(f"  RMSE       = {results['rmse']:.4f}")
    logger.info(f"  Sign agree = {results['sign_agreement']:.4f}")
    logger.info(f"  N samples  = {results['n_samples']}")

    return results
