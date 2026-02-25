"""The SGW Sentiment Engine. Core scoring algorithm.

This module implements the full SGW scoring formula:
  SGW(doc) = clip( [Σ V_i · mod_i · neg_i · decay(i, L, λ)] · dilution(B, k) , -1, 1)
"""

import numpy as np
from sgw.core.valence.scorer import ValenceScorer
from sgw.core.decay.functions import compute_decay_weights, no_decay
from sgw.core.dilution.buffer import compute_buffer_ratio, dilution_factor, no_dilution
from sgw.pipeline.syntax import SyntaxParser
from sgw.utils.config import load_config
from sgw.utils.logger import get_logger

logger = get_logger(__name__)


class SGWEngine:
    """The Sentiment Gummy Worm scoring engine."""

    def __init__(self, valence_scorer: ValenceScorer, syntax_parser: SyntaxParser,
                 config: dict = None):
        """Initialize the SGW engine.

        Args:
            valence_scorer: Initialized ValenceScorer instance.
            syntax_parser: Initialized SyntaxParser instance.
            config: Configuration dict. Defaults to loading from file.
        """
        if config is None:
            config = load_config()

        self.valence_scorer = valence_scorer
        self.syntax_parser = syntax_parser
        self.config = config
        self.lambda_c = config["decay"]["lambda_default"]
        self.k = config["dilution"]["k_default"]

        # Ablation flags
        self._use_decay = True
        self._use_dilution = True

    def set_lambda(self, lambda_c: float):
        """Set the decay constant. Used during calibration."""
        self.lambda_c = lambda_c

    def set_k(self, k: float):
        """Set the dilution constant. Used during calibration."""
        self.k = k

    def set_calibration_scale(self, mu_s: float, sigma_s: float, sigma_y: float):
        """Set affine calibration parameters to match corpus variance.
        Formula: S' = ((S - mu_s) / sigma_s) * sigma_y
        """
        self.mu_s = mu_s
        self.sigma_s = sigma_s
        self.sigma_y = sigma_y

    def disable_decay(self):
        """Disable positional decay (for ablation study)."""
        self._use_decay = False

    def enable_decay(self):
        """Enable positional decay."""
        self._use_decay = True

    def disable_dilution(self):
        """Disable buffer dilution (for ablation study)."""
        self._use_dilution = False

    def enable_dilution(self):
        """Enable buffer dilution."""
        self._use_dilution = True

    def score(self, text: str) -> float:
        """Compute the SGW sentiment score for a text.

        Args:
            text: Input text string.

        Returns:
            Float score in [-1.0, 1.0].
        """
        min_len = self.config["processing"]["min_text_length"]
        if not isinstance(text, str) or len(text.strip()) < min_len:
            return 0.0

        doc = self.syntax_parser.parse(text)
        return self.score_doc(doc)

    def score_doc(self, doc) -> float:
        """Compute the SGW sentiment score for a pre-parsed spaCy doc.

        Args:
            doc: spaCy Doc object.

        Returns:
            Float score in [-1.0, 1.0].
        """
        total_tokens = self.syntax_parser.count_tokens(doc)

        if total_tokens == 0:
            return 0.0

        # Detect contrastive pivot
        pivot_idx = self.syntax_parser.detect_pivot(doc)

        # Compute decay weights
        if self._use_decay:
            decay_weights = compute_decay_weights(total_tokens, self.lambda_c, pivot_idx=pivot_idx, eta=0.4)
        else:
            decay_weights = no_decay(total_tokens)

        # 1st Pass: Gather emotion tokens, valences, and compute Affective Mass (A)
        emotion_tokens = []
        token_idx = 0
        A = 0.0

        for token in doc:
            if token.is_space:
                continue

            if self.syntax_parser.is_emotion_carrier(token):
                v_star = self.valence_scorer.get_valence(token.lemma_)
                A += abs(v_star)
                emotion_tokens.append((token, token_idx, v_star))

            token_idx += 1

        # Compute Buffer Ratio B* 
        buffer_count = self.syntax_parser.count_buffers(doc)
        buffer_ratio_star = compute_buffer_ratio(buffer_count, total_tokens, A)

        # Compute dilution context h(D)
        if self._use_dilution:
            dilution = dilution_factor(buffer_ratio_star, self.k)
        else:
            dilution = no_dilution()

        # 2nd Pass: Final authoritative formula integration
        E_count = len(emotion_tokens)
        if E_count == 0:
            return 0.0

        sum_energy = 0.0
        gamma = 0.6
        delta = 0.4

        for token, idx, v_star in emotion_tokens:
            # Modifiers (Log-space intensity to exponential)
            alpha_i = self.syntax_parser.get_modifier_alpha(token)
            M_i = np.exp(alpha_i)

            # Magnitude-Aware Negation N_i
            if self.syntax_parser.is_negated(token):
                N_i = -gamma - delta * abs(v_star)
            else:
                N_i = 1.0

            # Spatial Decay w_i
            w_i = decay_weights[idx]

            term = v_star * M_i * N_i * w_i
            sum_energy += term

        # Energy-Preserving Normalization
        normalized = sum_energy / np.sqrt(1.0 + E_count)

        # Apply Global Dilution
        final_val = normalized * dilution

        # Clip to bounds [-1, 1]
        score = float(np.clip(final_val, -1.0, 1.0))

        # Variance-Matching Calibration (Post-hoc Affine Transform)
        if hasattr(self, 'sigma_s') and self.sigma_s > 0:
            score = ((score - self.mu_s) / self.sigma_s) * self.sigma_y
            score = float(np.clip(score, -1.0, 1.0))

        return score

    def score_docs(self, docs: list, show_progress: bool = False) -> np.ndarray:
        """Score a batch of pre-parsed spaCy docs.

        Args:
            docs: List of spaCy Doc objects.
            show_progress: Whether to show progress bar.

        Returns:
            numpy array of scores.
        """
        scores = []
        iterator = enumerate(docs)
        if show_progress:
            from tqdm import tqdm
            iterator = tqdm(enumerate(docs), total=len(docs), desc="SGW Scoring (Docs)")

        for i, doc in iterator:
            scores.append(self.score_doc(doc))

        return np.array(scores)

    def score_batch(self, texts: list, show_progress: bool = True) -> np.ndarray:
        """Score a batch of texts by parsing then scoring.

        Args:
            texts: List of text strings.
            show_progress: Whether to show progress bar.

        Returns:
            numpy array of scores.
        """
        docs = self.syntax_parser.parse_batch(texts, show_progress=show_progress)
        return self.score_docs(docs, show_progress=False)
