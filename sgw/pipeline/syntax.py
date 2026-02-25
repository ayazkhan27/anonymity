"""Syntactic parsing utilities using spaCy.

Extracts dependency-aware features: negation, modifiers, POS tags, buffer counts.
"""

import spacy
import math
from sgw.utils.config import load_config
from sgw.utils.logger import get_logger

logger = get_logger(__name__)


class SyntaxParser:
    """Wraps spaCy for dependency-aware feature extraction."""

    def __init__(self, config: dict = None):
        if config is None:
            config = load_config()
        self.config = config

        model_name = config["processing"]["spacy_model"]
        logger.info(f"Loading spaCy model: {model_name}")
        spacy.prefer_gpu()
        self.nlp = spacy.load(model_name)
        logger.info(f"spaCy model loaded: {model_name}")

        self.buffer_pos = set(config["syntax"]["buffer_pos_tags"])
        self.emotion_pos = set(config["syntax"]["emotion_pos_tags"])
        self.intensifiers = config["modifiers"]["intensifiers"]
        self.mitigators = config["modifiers"]["mitigators"]

    def parse(self, text: str):
        """Parse text into a spaCy Doc object.

        Args:
            text: Input text string.

        Returns:
            spacy.tokens.Doc object.
        """
        max_len = self.config["processing"]["max_text_length"]
        if len(text) > max_len:
            text = text[:max_len]
        return self.nlp(text)

    def parse_batch(self, texts: list, show_progress: bool = False):
        """Parse a batch of texts into spaCy Doc objects efficiently using nlp.pipe.

        Args:
            texts: List of input text strings.
            show_progress: Whether to show progress bar.

        Returns:
            List of spacy.tokens.Doc objects.
        """
        max_len = self.config["processing"]["max_text_length"]
        processed = [t[:max_len] if len(t) > max_len else t for t in texts]
        
        if show_progress:
            from tqdm import tqdm
            return list(tqdm(self.nlp.pipe(processed), total=len(texts), desc="spaCy Parsing"))
        else:
            return list(self.nlp.pipe(processed))

    def is_buffer(self, token) -> bool:
        """Check if a token is a buffer (function) word."""
        return token.pos_ in self.buffer_pos

    def is_emotion_carrier(self, token) -> bool:
        """Check if a token is an emotion-carrying word."""
        return token.pos_ in self.emotion_pos

    def is_negated(self, token) -> bool:
        """Check if a token is syntactically negated.

        Checks for:
        1. Direct negation dependency (child with dep_ == 'neg')
        2. Adverbial negators ('never', 'hardly', 'barely', 'scarcely', 'seldom')
        """
        for child in token.children:
            if child.dep_ == "neg":
                return True
            if child.lower_ in {"never", "hardly", "barely", "scarcely", "seldom"}:
                return True
        return False

    def get_modifier_alpha(self, token) -> float:
        """Get the sum of log-space intensity modifiers (alpha) for a token.

        Scans children with advmod dependency for known intensifiers/mitigators.

        Returns:
            Alpha sum in log-space.
        """
        alpha_sum = 0.0
        for child in token.children:
            if child.dep_ == "advmod":
                child_lower = child.lower_
                if child_lower in self.intensifiers:
                    val = max(1.001, self.intensifiers[child_lower])
                    alpha_sum += math.log(val)
                if child_lower in self.mitigators:
                    val = max(0.001, self.mitigators[child_lower])
                    alpha_sum += math.log(val)
        return alpha_sum

    def detect_pivot(self, doc) -> int:
        """Detect the contrastive pivot index.
        
        p = max {i : t_i in {but, however, although, though}}
        
        Returns:
            Token index of the pivot, or -1 if no pivot exists.
        """
        pivots = {"but", "however", "although", "though"}
        max_idx = -1
        for i, token in enumerate(doc):
            if token.lower_ in pivots:
                max_idx = i
        return max_idx

    def count_buffers(self, doc) -> int:
        """Count buffer tokens in a document."""
        return sum(1 for token in doc if self.is_buffer(token) and not token.is_space)

    def count_tokens(self, doc) -> int:
        """Count non-space tokens in a document."""
        return sum(1 for token in doc if not token.is_space)
