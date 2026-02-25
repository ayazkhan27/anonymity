# Sentiment Gummy Worm (SGW) V3

**Unsupervised mathematical sentiment scoring via spatial decay, vector valence projection, and syntactic dilution.**

SGW produces continuous sentiment scores from -1.0 to +1.0 for arbitrary English text without requiring any labeled training data.

## Quick Start

```python
from sgw.utils.vectors import load_vectors
from sgw.core.valence.scorer import ValenceScorer
from sgw.pipeline.syntax import SyntaxParser
from sgw.core.engine import SGWEngine

vectors = load_vectors(limit=500000)
scorer = ValenceScorer(vectors)
parser = SyntaxParser()
engine = SGWEngine(scorer, parser)

score = engine.score("I am incredibly happy today!")
print(f"Sentiment: {score:.3f}")  # Positive value
```

## How It Works

SGW computes sentiment via three novel components:

1. **Vector Valence**: Projects word embeddings onto a sentiment axis (SemAxis method).
2. **Spatial Decay**: Weights earlier words more heavily (primacy effect).
3. **Syntactic Dilution**: Dampens intensity based on function-word ratio.

See `docs/` for the full methodology paper.

## Evaluation Results

Run `python sgw/evaluation/run_evaluation.py` to reproduce all benchmarks.

## License

MIT
