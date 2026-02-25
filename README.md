# SGW: Sentiment Gummy Worm V3.5.0 🐛

**Unsupervised Mathematical Sentiment Scoring for High-Stakes Natural Language.**

SGW is a robust, interpretable sentiment engine designed for complex, long-form documents where traditional lexicon-based models (like VADER) fail and black-box LLMs are too expensive or opaque for auditability.

## 🚀 Key Features (V3.5.0 PRODUCT RELEASE)

- **Domain-Adaptive Sentiment Axes**: Change the engine's worldview (General, CX, Finance, Healthcare) by hot-swapping polar seed words without retraining.
- **Decomposable Explanations**: Every score includes a token-level breakdown of valence, intensity modifiers, negation triggers, and spatial decay.
- **Structural Sentiment Formula**:
  - **Spatial Decay**: Recency bias for final-judgment dominance.
  - **Buffer Dilution**: Emotional concentration awareness (handles "fluff").
  - **Magnitude-Aware Negation**: Sentiment magnitude affects negation strength.
- **Enterprise-Ready API**: FastAPI endpoints for `/analyze`, `/explain`, and `/axis/swap`.

## 📦 Installation

```bash
git clone https://github.com/ayazkhan27/anonymity.git
cd anonymity
pip install -e .
python -m spacy download en_core_web_sm
```

## 🛠 Usage

### Basic Scoring
```python
from sgw.core.engine import SGWEngine
from sgw.core.valence.scorer import ValenceScorer
from sgw.pipeline.syntax import SyntaxParser
from sgw.utils.vectors import load_vectors

# Load GloVe vectors
vectors = load_vectors(limit=300000)

# Initialize engine
scorer = ValenceScorer(vectors)
parser = SyntaxParser()
engine = SGWEngine(scorer, parser)

# Score text
score = engine.score("The service was terrible but the food was excellent.")
print(f"SGW Score: {score}")
```

### Explanatory Output
```python
explanation = engine.explain("The service was terrible but the food was excellent.")
print(explanation.summary())
```

### Domain Adaptation
```python
from sgw.core.valence.axis import SentimentAxis
finance_axis = SentimentAxis.from_preset(vectors, "finance")
engine.valence_scorer.swap_axis(finance_axis)
```

## 📊 Benchmarking

Run the product validation suite (Tier 4.5):
```bash
python sgw/evaluation/run_tier4_5.py
```

## ⚖️ License
Proprietary / Enterprise SDK.
