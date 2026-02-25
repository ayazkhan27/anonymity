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

        # Valence thresholding for noise reduction
        self.valence_threshold = float(config.get("valence", {}).get("valence_threshold_factor", 0.0))

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
                if abs(v_star) < self.valence_threshold:
                    v_star = 0.0
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

        # Energy-Preserving Normalization (Configurable Power)
        # Power=0.5 is sqrt (classic), 1.0 is average. 
        # Using 0.7 to balance intensity build-up with length stability.
        norm_power = self.config.get("normalization", {}).get("power", 0.7)
        normalized = sum_energy / np.power(1.0 + E_count, norm_power)

        # Apply Global Dilution
        final_val = normalized * dilution

        # Clip to bounds [-1, 1]
        score = float(np.clip(final_val, -1.0, 1.0))

        # Variance-Matching Calibration (Post-hoc Affine Transform)
        if hasattr(self, 'sigma_s') and self.sigma_s > 0:
            score = ((score - self.mu_s) / self.sigma_s) * self.sigma_y
            score = float(np.clip(score, -1.0, 1.0))

        return score

    def explain_doc(self, doc) -> "SGWExplanation":
        """Compute SGW score with full token-level explanation.

        Args:
            doc: spaCy Doc object.

        Returns:
            SGWExplanation with complete decomposition.
        """
        from sgw.core.explanation import SGWExplanation, TokenContribution

        total_tokens = self.syntax_parser.count_tokens(doc)
        if total_tokens == 0:
            return SGWExplanation(
                text=doc.text, score=0.0, score_before_calibration=0.0,
                token_contributions=[], sum_energy=0.0,
                emotion_token_count=0, normalization_divisor=1.0,
                normalized_energy=0.0, dilution_factor=1.0,
                diluted_energy=0.0, total_tokens=0, buffer_count=0,
                buffer_ratio=0.0, affective_mass=0.0,
                effective_buffer_ratio=0.0, pivot_detected=False,
                lambda_c=self.lambda_c, k=self.k,
                axis_name=self.valence_scorer.axis.name,
            )

        pivot_idx = self.syntax_parser.detect_pivot(doc)

        if self._use_decay:
            decay_weights = compute_decay_weights(total_tokens, self.lambda_c, pivot_idx=pivot_idx, eta=0.4)
        else:
            decay_weights = no_decay(total_tokens)

        # 1st pass: gather all token info
        all_contributions = []
        emotion_tokens_data = []
        token_idx = 0
        A = 0.0

        for token in doc:
            if token.is_space:
                continue

            is_emotion = self.syntax_parser.is_emotion_carrier(token)
            is_buf = self.syntax_parser.is_buffer(token)
            v_star = self.valence_scorer.get_valence(token) if is_emotion else 0.0

            if abs(v_star) < self.valence_threshold:
                v_star = 0.0

            if is_emotion:
                A += abs(v_star)

            contrib = TokenContribution(
                token=token.text,
                lemma=token.lemma_,
                position=token_idx,
                pos_tag=token.pos_,
                is_emotion_carrier=is_emotion,
                base_valence=v_star,
                modifier_weight=1.0,
                negation_factor=1.0,
                decay_weight=float(decay_weights[token_idx]),
                raw_contribution=0.0,
                is_negated=False,
                is_buffer=is_buf,
            )

            if is_emotion:
                emotion_tokens_data.append((contrib, token, token_idx, v_star))

            all_contributions.append(contrib)
            token_idx += 1

        buffer_count = self.syntax_parser.count_buffers(doc)
        buffer_ratio = buffer_count / total_tokens if total_tokens > 0 else 0.0
        effective_buffer_ratio = compute_buffer_ratio(buffer_count, total_tokens, A)

        if self._use_dilution:
            dilution = dilution_factor(effective_buffer_ratio, self.k)
        else:
            dilution = no_dilution()

        # 2nd pass: compute contributions for emotion tokens
        E_count = len(emotion_tokens_data)
        gamma = 0.6
        delta = 0.4
        sum_energy = 0.0

        for contrib, token, idx, v_star in emotion_tokens_data:
            alpha_i = self.syntax_parser.get_modifier_alpha(token)
            M_i = np.exp(alpha_i)
            contrib.modifier_weight = float(M_i)

            # Check for modifier trigger
            for child in token.children:
                if child.dep_ == "advmod" and child.lower_ in {**self.syntax_parser.intensifiers, **self.syntax_parser.mitigators}:
                    contrib.modifier_trigger = child.text
                    break

            is_neg = self.syntax_parser.is_negated(token)
            if is_neg:
                N_i = -gamma - delta * abs(v_star)
                contrib.is_negated = True
                # Find negation trigger
                for child in token.children:
                    if child.dep_ == "neg" or child.lower_ in {"never", "hardly", "barely", "scarcely", "seldom"}:
                        contrib.negation_trigger = child.text
                        break
            else:
                N_i = 1.0

            contrib.negation_factor = float(N_i)
            w_i = float(decay_weights[idx])
            contrib.decay_weight = w_i

            raw = v_star * M_i * N_i * w_i
            contrib.raw_contribution = float(raw)
            sum_energy += raw

        norm_power = self.config.get("normalization", {}).get("power", 0.7)
        norm_divisor = np.power(1.0 + E_count, norm_power)
        normalized = sum_energy / norm_divisor if E_count > 0 else 0.0
        diluted = normalized * dilution
        score_pre_cal = float(np.clip(diluted, -1.0, 1.0))

        # Calibration
        score = score_pre_cal
        mu_s = getattr(self, 'mu_s', 0.0)
        sigma_s = getattr(self, 'sigma_s', 1.0)
        sigma_y = getattr(self, 'sigma_y', 1.0)
        if hasattr(self, 'sigma_s') and sigma_s > 0 and sigma_s != 1.0:
            score = ((score - mu_s) / sigma_s) * sigma_y
            score = float(np.clip(score, -1.0, 1.0))

        # Pivot info
        pivot_detected = pivot_idx is not None
        pivot_token_text = None
        pivot_position = None
        if pivot_detected:
            non_space_idx = 0
            for token in doc:
                if token.is_space:
                    continue
                if non_space_idx == pivot_idx:
                    pivot_token_text = token.text
                    pivot_position = pivot_idx
                    break
                non_space_idx += 1

        return SGWExplanation(
            text=doc.text,
            score=score,
            score_before_calibration=score_pre_cal,
            score_raw=float(diluted),
            token_contributions=all_contributions,
            sum_energy=float(sum_energy),
            emotion_token_count=E_count,
            normalization_divisor=float(norm_divisor),
            normalized_energy=float(normalized),
            dilution_factor=float(dilution),
            diluted_energy=float(diluted),
            total_tokens=total_tokens,
            buffer_count=buffer_count,
            buffer_ratio=float(buffer_ratio),
            affective_mass=float(A),
            effective_buffer_ratio=float(effective_buffer_ratio),
            pivot_detected=pivot_detected,
            pivot_token=pivot_token_text,
            pivot_position=pivot_position,
            lambda_c=self.lambda_c,
            k=self.k,
            axis_name=self.valence_scorer.axis.name,
            mu_s=mu_s,
            sigma_s=sigma_s,
            sigma_y=sigma_y,
        )

    def explain(self, text: str) -> "SGWExplanation":
        """Compute SGW score with full explanation for a raw text string."""
        doc = self.syntax_parser.parse(text)
        return self.explain_doc(doc)

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
