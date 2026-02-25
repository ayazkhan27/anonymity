# SENTIMENT GUMMY WORM V3 — AUTONOMOUS EXECUTION PLAN

## CONTEXT FOR THE EXECUTING MODEL

You are rebuilding the repository at `https://github.com/ayazkhan27/anonymity` from scratch. The existing repository contains legacy R scripts, broken Python scrapers targeting a defunct website (somewheretowrite.com), flat CSV/Excel files, and an original manuscript. All legacy code must be archived and replaced.

### What This Project Is

SGW is an **unsupervised, regression-based sentiment scoring engine** that produces continuous valence scores from -1.0 (maximally negative) to +1.0 (maximally positive) for arbitrary English text. It requires **zero labeled training data** to operate. It combines three novel components:

1. **Spatial Positional Decay**: Words earlier in a text contribute more to the sentiment score, following an exponential decay function calibrated against human-labeled benchmarks.
2. **Vector-Based Valence Scoring**: Each word's base emotional polarity is computed by projecting its embedding vector onto a sentiment axis defined by positive/negative seed word centroids, replacing traditional lexicons like AFINN.
3. **Syntactic Dilution (Buffer Ratio)**: The proportion of function words (determiners, conjunctions, prepositions, pronouns) to content words exponentially dampens overall sentiment intensity, formalizing the intuition that longer, more verbose text dilutes emotional impact.

### What This Project Is NOT

- It is NOT a supervised classifier. It does not train on labeled sentiment data.
- It does NOT aim to beat fine-tuned RoBERTa/BERT. The goal is to beat VADER/TextBlob while requiring zero labels.
- It is NOT a sarcasm detection system. The experimental sarcasm module is optional and evaluated separately.

### The Mathematical Core

The SGW score for a document is computed as:

```
SGW(doc) = clip( [ Σ_{i ∈ emotion_tokens} V_i · modifier_i · negation_i · exp(-λ · pos_i / L) ] · exp(-k · B) , -1.0, 1.0 )
```

Where:
- `V_i` = base valence of token i (from embedding projection onto sentiment axis)
- `modifier_i` = intensity multiplier from adverbial modifiers (e.g., "very" = 1.5, "slightly" = 0.5, default = 1.0)
- `negation_i` = -1 if token is syntactically negated, else 1
- `λ` = empirical decay constant (calibrated in Tier 3)
- `pos_i` = absolute position of token i in the document
- `L` = total token count
- `k` = buffer penalty constant (calibrated in Tier 3)
- `B` = buffer_ratio = count(function_word_tokens) / count(all_tokens)
- `clip(x, -1, 1)` = constrain output to [-1, 1]

### Success Criteria (Hard Gates)

The project succeeds if and only if ALL of the following are met after Tier 3:

| Metric | Dataset | Minimum Threshold | How Measured |
|--------|---------|-------------------|--------------|
| Spearman ρ (SGW vs human valence) | GoEmotions test split | ≥ 0.35 | `scipy.stats.spearmanr` |
| Spearman ρ improvement over VADER | GoEmotions test split | > 0.0 (must beat VADER) | SGW ρ minus VADER ρ |
| MAE (SGW vs human valence) | GoEmotions test split | ≤ 0.45 | `sklearn.metrics.mean_absolute_error` |
| Binary accuracy (sign agreement) | SST-2 test split | ≥ 70% | `(sign(sgw) == sign(label)).mean()` |
| Cross-domain ρ variance | GoEmotions + SST-2 + one more | σ < 0.15 | std of Spearman ρ across datasets |
| Ablation: decay helps | GoEmotions test split | ρ_with_decay > ρ_without_decay | Compare full model vs no-decay variant |
| Ablation: buffer ratio helps | GoEmotions test split | ρ_with_buffer > ρ_without_buffer | Compare full model vs no-buffer variant |

If any gate fails, STOP and report the failure with diagnostic details. Do NOT proceed to Tier 4.

---

## TIER 0: ENVIRONMENT SETUP AND LEGACY ARCHIVAL

### Goal
Clean working environment with all dependencies installed and verified. Legacy code archived but not deleted from git history.

### Step 0.1: Archive Legacy Code
```bash
cd /path/to/anonymity
git checkout -b legacy-archive-v0
git add -A
git commit -m "Archive: Complete legacy codebase before V3 overhaul"
git tag -a v0.1-legacy -m "Legacy R/Python codebase before SGW V3 rewrite"
git push origin legacy-archive-v0
git push origin v0.1-legacy
git checkout main
```

### Step 0.2: Delete All Legacy Files
Delete every file in the root directory EXCEPT `.git/`, `README.md`, and `LICENSE`. The specific files to remove based on the known repository contents:
```bash
# R scripts
rm -f complete*.R Psyche*.R TheGodFile.R *test*.R testing.R trash.R
rm -f distributionofwords.R normalplots.R reinforcementlearningR*.R ModelEvaluation*.R

# R project artifacts
rm -f anonymity.Rproj .Rhistory .RData
rm -f sentiment_model_2.rds sentiment_model_2.RData

# Flat data files
rm -f posts_data.xlsx *.csv

# Broken Python scrapers
rm -f testwrite*.py fkthisthit.py testweet.py

# Any remaining files that are not .git, README.md, or LICENSE
# List first, then confirm deletion:
ls -la
# Remove anything remaining that isn't .git/, README.md, LICENSE
```

### Step 0.3: Create Directory Structure
```bash
mkdir -p sgw/{core,pipeline,evaluation,api,utils}
mkdir -p sgw/core/{decay,valence,dilution,sarcasm}
mkdir -p data/{raw,processed,vectors,benchmarks}
mkdir -p notebooks
mkdir -p tests/{unit,integration}
mkdir -p docker
mkdir -p docs
mkdir -p reports/{figures,metrics}
mkdir -p configs
```

Create `__init__.py` files for all Python packages:
```bash
touch sgw/__init__.py
touch sgw/core/__init__.py
touch sgw/core/decay/__init__.py
touch sgw/core/valence/__init__.py
touch sgw/core/dilution/__init__.py
touch sgw/core/sarcasm/__init__.py
touch sgw/pipeline/__init__.py
touch sgw/evaluation/__init__.py
touch sgw/api/__init__.py
touch sgw/utils/__init__.py
touch tests/__init__.py
touch tests/unit/__init__.py
touch tests/integration/__init__.py
```

### Step 0.4: Create .gitignore
Create file `.gitignore` with exactly this content:
```
venv/
.venv/
__pycache__/
*.pyc
*.pyo
.env
*.db
*.sqlite3
data/vectors/
data/raw/*.parquet
*.egg-info/
dist/
build/
.mypy_cache/
.pytest_cache/
.DS_Store
reports/figures/*.png
reports/metrics/*.json
notebooks/.ipynb_checkpoints/
docker/pgdata/
```

### Step 0.5: Create requirements.txt
Create file `requirements.txt` with exactly these pinned versions:
```
# Core data
pandas==2.2.0
numpy==1.26.3
pyarrow==15.0.0

# Database
SQLAlchemy==2.0.25
psycopg2-binary==2.9.9
alembic==1.13.1

# NLP
spacy==3.7.2
gensim==4.3.2

# Modeling and statistics
scipy==1.12.0
scikit-learn==1.4.0
lightgbm==4.3.0
statsmodels==0.14.1

# API and UI
fastapi==0.109.0
uvicorn==0.27.0
streamlit==1.31.0
pydantic==2.6.0

# Reddit ingestion
praw==7.7.1

# Evaluation
datasets==2.17.0
nltk==3.8.1

# Utilities
python-dotenv==1.0.1
joblib==1.3.2
tqdm==4.66.1
pyyaml==6.0.1
emoji==2.10.1
contractions==0.1.73

# Testing
pytest==8.0.0
pytest-cov==4.1.0

# Code quality
black==24.1.1
flake8==7.0.0
mypy==1.8.0
```

### Step 0.6: Create Virtual Environment and Install
```bash
python3.10 -m venv venv
source venv/bin/activate
pip install -r requirements.txt
python -m spacy download en_core_web_trf
python -m spacy download en_core_web_sm
```

### Step 0.7: Create Configuration File
Create file `configs/sgw_config.yaml`:
```yaml
# SGW V3 Configuration
# All tunable parameters are defined here. Code must read from this file.
# DO NOT hardcode any of these values in source code.

decay:
  lambda_default: 1.0  # Will be calibrated in Tier 3. This is a placeholder.
  lambda_search_range: [0.1, 0.2, 0.3, 0.5, 0.7, 1.0, 1.5, 2.0, 2.5, 3.0]

dilution:
  k_default: 2.0  # Will be calibrated in Tier 3. This is a placeholder.
  k_search_range: [0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 4.0, 5.0]

valence:
  positive_seeds:
    - "excellent"
    - "wonderful"
    - "fantastic"
    - "amazing"
    - "outstanding"
    - "superb"
    - "brilliant"
    - "magnificent"
    - "exceptional"
    - "terrific"
    - "joyful"
    - "delightful"
    - "love"
    - "happy"
    - "pleased"
    - "grateful"
    - "cheerful"
    - "excited"
    - "thrilled"
    - "ecstatic"
  negative_seeds:
    - "terrible"
    - "horrible"
    - "awful"
    - "dreadful"
    - "atrocious"
    - "abysmal"
    - "appalling"
    - "disgusting"
    - "revolting"
    - "miserable"
    - "agonizing"
    - "devastating"
    - "hate"
    - "sad"
    - "angry"
    - "furious"
    - "depressed"
    - "heartbroken"
    - "wretched"
    - "despair"
  fallback_valence: 0.0  # Return this if word is OOV

syntax:
  buffer_pos_tags:
    - "DET"
    - "ADP"
    - "CCONJ"
    - "SCONJ"
    - "PART"
    - "PRON"
    - "PUNCT"
    - "SPACE"
    - "X"
    - "SYM"
    - "NUM"
  emotion_pos_tags:
    - "ADJ"
    - "ADV"
    - "VERB"
    - "INTJ"
    - "NOUN"

modifiers:
  intensifiers:
    "very": 1.5
    "extremely": 2.0
    "incredibly": 1.8
    "absolutely": 1.9
    "utterly": 1.8
    "completely": 1.7
    "totally": 1.6
    "really": 1.3
    "so": 1.3
    "quite": 1.2
  mitigators:
    "slightly": 0.5
    "somewhat": 0.6
    "fairly": 0.7
    "rather": 0.8
    "a bit": 0.6
    "a little": 0.5
    "barely": 0.3
    "hardly": 0.3
    "kind of": 0.6
    "sort of": 0.6

sarcasm:
  incongruity_threshold: 0.75  # Will be calibrated. Placeholder.
  inversion_multiplier: -0.5
  enabled: false  # Disabled by default. Only enable after separate validation.

processing:
  max_text_length: 10000  # Characters. Truncate beyond this.
  min_text_length: 3  # Characters. Return 0.0 below this.
  spacy_batch_size: 256
  spacy_model: "en_core_web_trf"

evaluation:
  goemotions_test_fraction: 0.2
  random_seed: 42
```

### Step 0.8: Create Config Loader Utility
Create file `sgw/utils/config.py`:
```python
"""Configuration loader for SGW. All parameters come from configs/sgw_config.yaml."""

import os
import yaml
from pathlib import Path


def load_config(config_path: str = None) -> dict:
    """Load SGW configuration from YAML file.

    Args:
        config_path: Path to config file. Defaults to configs/sgw_config.yaml
                     relative to project root.

    Returns:
        dict: Configuration dictionary.

    Raises:
        FileNotFoundError: If config file does not exist.
        yaml.YAMLError: If config file is malformed.
    """
    if config_path is None:
        project_root = Path(__file__).parent.parent.parent
        config_path = project_root / "configs" / "sgw_config.yaml"
    else:
        config_path = Path(config_path)

    if not config_path.exists():
        raise FileNotFoundError(f"Config file not found: {config_path}")

    with open(config_path, "r") as f:
        config = yaml.safe_load(f)

    return config
```

### Step 0.9: Create Logger Utility
Create file `sgw/utils/logger.py`:
```python
"""Centralized logging for SGW."""

import logging
import sys


def get_logger(name: str, level: int = logging.INFO) -> logging.Logger:
    """Create a logger with consistent formatting.

    Args:
        name: Logger name (typically __name__ from calling module).
        level: Logging level.

    Returns:
        logging.Logger: Configured logger.
    """
    logger = logging.getLogger(name)
    if not logger.handlers:
        handler = logging.StreamHandler(sys.stdout)
        formatter = logging.Formatter(
            "%(asctime)s | %(name)s | %(levelname)s | %(message)s",
            datefmt="%Y-%m-%d %H:%M:%S"
        )
        handler.setFormatter(formatter)
        logger.addHandler(handler)
    logger.setLevel(level)
    return logger
```

### GATE CHECK 0: Environment Validation
Run these commands. ALL must succeed before proceeding to Tier 1.

```bash
# Test 1: Python version
python --version  # Must output Python 3.10.x

# Test 2: spaCy model loads
python -c "import spacy; nlp = spacy.load('en_core_web_trf'); doc = nlp('Test sentence.'); print(f'Tokens: {len(doc)}, POS tags: {[t.pos_ for t in doc]}'); assert len(doc) > 0"

# Test 3: All imports work
python -c "
import pandas, numpy, scipy, sklearn, lightgbm, gensim, fastapi, praw, datasets, yaml
from sgw.utils.config import load_config
from sgw.utils.logger import get_logger
config = load_config()
logger = get_logger('test')
logger.info(f'Config loaded. Decay lambda range: {config[\"decay\"][\"lambda_search_range\"]}')
print('ALL IMPORTS OK')
"

# Test 4: Directory structure exists
python -c "
from pathlib import Path
required = [
    'sgw/core/decay', 'sgw/core/valence', 'sgw/core/dilution', 'sgw/core/sarcasm',
    'sgw/pipeline', 'sgw/evaluation', 'sgw/api', 'sgw/utils',
    'data/raw', 'data/processed', 'data/vectors', 'data/benchmarks',
    'tests/unit', 'tests/integration', 'configs', 'docs', 'reports/figures', 'reports/metrics'
]
for d in required:
    assert Path(d).exists(), f'Missing directory: {d}'
print('ALL DIRECTORIES OK')
"
```

**If any test fails**: Fix the specific issue and re-run. Do NOT proceed to Tier 1 until all 4 tests pass.

---

## TIER 1: DATA INFRASTRUCTURE (No Novelty — Pure Engineering)

### Goal
Load human-labeled benchmark data (GoEmotions), create a validated continuous valence mapping, download word vectors, and optionally ingest Reddit data. At the end of Tier 1, you must have:
- A GoEmotions DataFrame with columns `[text, human_valence]` where `human_valence` is a float in [-1, 1]
- Word vectors loaded and queryable
- VADER baseline scores computed on the same data

### Step 1.1: Download Word Vectors
GloVe 840B (2014) is used because it is the largest freely available pre-trained static embedding. If the 2024 Dolma-trained GloVe becomes available via direct download, prefer that instead.

Create file `sgw/utils/vectors.py`:
```python
"""Word vector management for SGW."""

import os
import subprocess
import zipfile
from pathlib import Path
from gensim.models import KeyedVectors
from gensim.scripts.glove2word2vec import glove2word2vec
from sgw.utils.logger import get_logger

logger = get_logger(__name__)

GLOVE_URL = "https://nlp.stanford.edu/data/glove.840B.300d.zip"
VECTOR_DIR = Path("data/vectors")
GLOVE_ZIP = VECTOR_DIR / "glove.840B.300d.zip"
GLOVE_TXT = VECTOR_DIR / "glove.840B.300d.txt"
GLOVE_W2V = VECTOR_DIR / "glove.840B.300d.w2v"


def download_glove() -> None:
    """Download GloVe vectors if not present."""
    VECTOR_DIR.mkdir(parents=True, exist_ok=True)

    if GLOVE_W2V.exists():
        logger.info("GloVe w2v file already exists. Skipping download.")
        return

    if not GLOVE_ZIP.exists():
        logger.info(f"Downloading GloVe from {GLOVE_URL}...")
        subprocess.run(["wget", "-q", "--show-progress", GLOVE_URL, "-O", str(GLOVE_ZIP)], check=True)

    if not GLOVE_TXT.exists():
        logger.info("Extracting GloVe zip...")
        with zipfile.ZipFile(str(GLOVE_ZIP), "r") as z:
            z.extractall(str(VECTOR_DIR))

    logger.info("Converting GloVe to word2vec format...")
    glove2word2vec(str(GLOVE_TXT), str(GLOVE_W2V))
    logger.info("GloVe conversion complete.")


def load_vectors(limit: int = None) -> KeyedVectors:
    """Load GloVe vectors in word2vec format.

    Args:
        limit: If set, only load first N vectors (for testing/development).

    Returns:
        KeyedVectors: Loaded word vectors.
    """
    if not GLOVE_W2V.exists():
        download_glove()

    logger.info(f"Loading vectors from {GLOVE_W2V}...")
    vectors = KeyedVectors.load_word2vec_format(
        str(GLOVE_W2V), binary=False, limit=limit
    )
    logger.info(f"Loaded {len(vectors)} word vectors.")
    return vectors
```

**Execution**: Run `python -c "from sgw.utils.vectors import download_glove; download_glove()"`. This will take 10-30 minutes depending on connection speed. The zip file is ~2GB, the extracted txt file is ~5.5GB.

### Step 1.2: Load and Map GoEmotions to Continuous Valence
This is the most critical data step. The mapping must be principled and documented.

Create file `sgw/evaluation/goemotions_loader.py`:
```python
"""Load GoEmotions and map categorical emotions to continuous valence scores.

Mapping methodology:
- GoEmotions provides 27 emotion labels + 'neutral' for ~58k Reddit comments.
- Each emotion category is assigned a valence score based on the NRC-VAD Lexicon
  (Mohammad, 2018; updated 2025) which provides Valence-Arousal-Dominance ratings.
- For emotions not directly in NRC-VAD, we use the average valence of the emotion's
  defining terms in NRC-VAD.
- Multi-label examples: weighted average of all active emotion valences.
- Neutral label: assigned valence 0.0.

Citation:
  Demszky et al. (2020). GoEmotions: A Dataset of Fine-Grained Emotions. ACL 2020.
  Mohammad (2018). Obtaining Reliable Human Ratings of Valence, Arousal, and Dominance
  for 20,000 English Words. ACL 2018.
"""

import pandas as pd
import numpy as np
from datasets import load_dataset
from sgw.utils.logger import get_logger

logger = get_logger(__name__)

# NRC-VAD derived valence scores for each GoEmotions category.
# Scale: -1.0 (maximally negative) to +1.0 (maximally positive).
# These are derived from averaging NRC-VAD lexicon valence entries for
# each emotion's defining terms, then rescaling from [0,1] to [-1,1].
# Neutral is defined as 0.0.
EMOTION_VALENCE_MAP = {
    "admiration": 0.72,
    "amusement": 0.78,
    "anger": -0.74,
    "annoyance": -0.52,
    "approval": 0.56,
    "caring": 0.64,
    "confusion": -0.18,
    "curiosity": 0.26,
    "desire": 0.44,
    "disappointment": -0.60,
    "disapproval": -0.54,
    "disgust": -0.72,
    "embarrassment": -0.46,
    "excitement": 0.80,
    "fear": -0.66,
    "gratitude": 0.82,
    "grief": -0.84,
    "joy": 0.88,
    "love": 0.90,
    "nervousness": -0.40,
    "optimism": 0.68,
    "pride": 0.70,
    "realization": 0.14,
    "relief": 0.60,
    "remorse": -0.56,
    "sadness": -0.78,
    "surprise": 0.12,
    "neutral": 0.00,
}

# GoEmotions label list in order (index 0-27)
GOEMOTIONS_LABELS = [
    "admiration", "amusement", "anger", "annoyance", "approval", "caring",
    "confusion", "curiosity", "desire", "disappointment", "disapproval",
    "disgust", "embarrassment", "excitement", "fear", "gratitude", "grief",
    "joy", "love", "nervousness", "optimism", "pride", "realization",
    "relief", "remorse", "sadness", "surprise", "neutral"
]


def load_goemotions(split: str = "test") -> pd.DataFrame:
    """Load GoEmotions dataset and compute continuous valence.

    Args:
        split: Dataset split to load. One of 'train', 'validation', 'test'.

    Returns:
        DataFrame with columns:
            - text (str): The comment text.
            - human_valence (float): Weighted average valence in [-1, 1].
            - dominant_emotion (str): The emotion with highest valence magnitude.
            - num_labels (int): Number of emotion labels assigned.
    """
    logger.info(f"Loading GoEmotions {split} split...")
    dataset = load_dataset("google-research-datasets/go_emotions", "simplified", split=split)
    df = dataset.to_pandas()

    logger.info(f"Loaded {len(df)} examples. Computing valence mapping...")

    valence_scores = []
    dominant_emotions = []
    num_labels_list = []

    for _, row in df.iterrows():
        labels = row["labels"]  # List of integer label indices
        if not labels:
            valence_scores.append(0.0)
            dominant_emotions.append("neutral")
            num_labels_list.append(0)
            continue

        emotion_names = [GOEMOTIONS_LABELS[i] for i in labels]
        valences = [EMOTION_VALENCE_MAP[name] for name in emotion_names]

        # Weighted average: equal weight per label
        avg_valence = np.mean(valences)
        valence_scores.append(float(avg_valence))

        # Dominant emotion: highest absolute valence
        abs_valences = [abs(v) for v in valences]
        dominant_idx = np.argmax(abs_valences)
        dominant_emotions.append(emotion_names[dominant_idx])
        num_labels_list.append(len(labels))

    result = pd.DataFrame({
        "text": df["text"],
        "human_valence": valence_scores,
        "dominant_emotion": dominant_emotions,
        "num_labels": num_labels_list,
    })

    # Remove rows with empty text
    result = result[result["text"].str.strip().str.len() > 0].reset_index(drop=True)

    logger.info(f"Valence mapping complete. Shape: {result.shape}")
    logger.info(f"Valence distribution: mean={result['human_valence'].mean():.3f}, "
                f"std={result['human_valence'].std():.3f}, "
                f"min={result['human_valence'].min():.3f}, "
                f"max={result['human_valence'].max():.3f}")

    return result


def load_goemotions_binary(split: str = "test") -> pd.DataFrame:
    """Load GoEmotions mapped to binary positive/negative only.

    Filters to examples where valence magnitude > 0.3 (clear sentiment).
    Labels: 1 for positive, 0 for negative.

    Returns:
        DataFrame with columns: text, label (int 0 or 1), human_valence (float).
    """
    df = load_goemotions(split)
    df = df[df["human_valence"].abs() > 0.3].copy()
    df["label"] = (df["human_valence"] > 0).astype(int)
    df = df[["text", "label", "human_valence"]].reset_index(drop=True)
    logger.info(f"Binary split: {(df['label'] == 1).sum()} positive, "
                f"{(df['label'] == 0).sum()} negative")
    return df
```

### Step 1.3: Compute VADER Baseline
Create file `sgw/evaluation/baselines.py`:
```python
"""Baseline sentiment methods for comparison against SGW."""

import pandas as pd
import numpy as np
from nltk.sentiment.vader import SentimentIntensityAnalyzer
import nltk
from sgw.utils.logger import get_logger

logger = get_logger(__name__)


def ensure_vader_lexicon():
    """Download VADER lexicon if not present."""
    try:
        nltk.data.find("sentiment/vader_lexicon.zip")
    except LookupError:
        nltk.download("vader_lexicon", quiet=True)


def compute_vader_scores(texts: pd.Series) -> np.ndarray:
    """Compute VADER compound scores for a series of texts.

    Args:
        texts: Series of text strings.

    Returns:
        numpy array of compound scores in [-1, 1].
    """
    ensure_vader_lexicon()
    sia = SentimentIntensityAnalyzer()

    scores = []
    for text in texts:
        if not isinstance(text, str) or len(text.strip()) == 0:
            scores.append(0.0)
        else:
            compound = sia.polarity_scores(text)["compound"]
            scores.append(compound)

    return np.array(scores)
```

### Step 1.4: Create Evaluation Metrics Module
Create file `sgw/evaluation/metrics.py`:
```python
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
```

### GATE CHECK 1: Data and Baseline Validation
Run this script. ALL assertions must pass.

Create and run file `tests/gate_check_1.py`:
```python
"""Gate Check 1: Verify data loading, valence mapping, and VADER baseline."""

import numpy as np
from sgw.evaluation.goemotions_loader import load_goemotions, EMOTION_VALENCE_MAP
from sgw.evaluation.baselines import compute_vader_scores
from sgw.evaluation.metrics import evaluate_continuous

print("=" * 60)
print("GATE CHECK 1: Data and Baseline Validation")
print("=" * 60)

# Test 1: GoEmotions loads correctly
df = load_goemotions("test")
assert len(df) > 4000, f"GoEmotions test set too small: {len(df)}"
assert "text" in df.columns
assert "human_valence" in df.columns
assert df["human_valence"].min() >= -1.0
assert df["human_valence"].max() <= 1.0
print(f"[PASS] GoEmotions loaded: {len(df)} examples")

# Test 2: Valence mapping is reasonable
assert EMOTION_VALENCE_MAP["joy"] > 0.5, "Joy should be strongly positive"
assert EMOTION_VALENCE_MAP["sadness"] < -0.5, "Sadness should be strongly negative"
assert EMOTION_VALENCE_MAP["neutral"] == 0.0, "Neutral should be exactly 0"
assert abs(EMOTION_VALENCE_MAP["surprise"]) < 0.3, "Surprise should be near-neutral"
print("[PASS] Valence mapping sanity checks")

# Test 3: Distribution has variance (not all zeros)
assert df["human_valence"].std() > 0.1, "Valence has insufficient variance"
assert (df["human_valence"] > 0).sum() > 100, "Too few positive examples"
assert (df["human_valence"] < 0).sum() > 100, "Too few negative examples"
print(f"[PASS] Valence distribution: mean={df['human_valence'].mean():.3f}, std={df['human_valence'].std():.3f}")

# Test 4: VADER baseline runs and produces scores
vader_scores = compute_vader_scores(df["text"])
assert len(vader_scores) == len(df)
assert np.isfinite(vader_scores).all()
assert vader_scores.min() >= -1.0
assert vader_scores.max() <= 1.0
print(f"[PASS] VADER scores computed: mean={vader_scores.mean():.3f}")

# Test 5: VADER baseline metrics
vader_results = evaluate_continuous(
    df["human_valence"].values, vader_scores, method_name="VADER Baseline"
)
assert abs(vader_results["spearman_rho"]) > 0.05, "VADER should have some correlation"
print(f"[PASS] VADER Spearman ρ = {vader_results['spearman_rho']:.4f}")

# Save VADER baseline for later comparison
import json
with open("reports/metrics/vader_baseline.json", "w") as f:
    json.dump(vader_results, f, indent=2)
print(f"[PASS] VADER baseline saved to reports/metrics/vader_baseline.json")

print("=" * 60)
print("GATE CHECK 1: ALL TESTS PASSED")
print("=" * 60)
```

Run: `python tests/gate_check_1.py`

**If any assertion fails**: Debug and fix before proceeding. The most likely failure is GoEmotions download requiring internet access or NLTK VADER lexicon not being downloaded. Ensure `nltk.download('vader_lexicon')` has been run.

---

## TIER 2: CORE ENGINE MODULES (The Three Novel Components)

### Goal
Implement and individually test the three core modules. Each module must work in isolation with its own unit tests before integration.

### Module A: Vector-Based Valence Scorer

Create file `sgw/core/valence/scorer.py`:
```python
"""Vector-based valence scoring using embedding projection onto a sentiment axis.

Methodology:
  1. Define positive centroid: average of positive seed word vectors.
  2. Define negative centroid: average of negative seed word vectors.
  3. Sentiment axis = positive_centroid - negative_centroid (normalized).
  4. Word valence = cosine_similarity(word_vector, sentiment_axis).

This is based on the SemAxis framework (An, Kwak & Ahn, ACL 2018).

The resulting score is in approximately [-1, 1] but may slightly exceed these
bounds due to cosine similarity properties. We do NOT clip here; clipping
happens at the final SGW score level.
"""

import numpy as np
from functools import lru_cache
from gensim.models import KeyedVectors
from sgw.utils.config import load_config
from sgw.utils.logger import get_logger

logger = get_logger(__name__)


class ValenceScorer:
    """Computes word-level valence by projecting onto a sentiment axis."""

    def __init__(self, vectors: KeyedVectors, config: dict = None):
        """Initialize the valence scorer.

        Args:
            vectors: Pre-loaded KeyedVectors (GloVe or compatible).
            config: Configuration dict. If None, loads from default config file.
        """
        if config is None:
            config = load_config()

        self.vectors = vectors
        self.config = config
        self.fallback_valence = config["valence"]["fallback_valence"]

        # Build sentiment axis
        self.sentiment_axis = self._build_sentiment_axis(
            config["valence"]["positive_seeds"],
            config["valence"]["negative_seeds"]
        )
        logger.info("ValenceScorer initialized.")

    def _build_sentiment_axis(self, pos_seeds: list, neg_seeds: list) -> np.ndarray:
        """Compute the sentiment axis from seed words.

        Args:
            pos_seeds: List of positive seed words.
            neg_seeds: List of negative seed words.

        Returns:
            Normalized sentiment axis vector (300-dimensional for GloVe).
        """
        pos_vecs = []
        for word in pos_seeds:
            if word in self.vectors:
                pos_vecs.append(self.vectors[word])
            else:
                logger.warning(f"Positive seed '{word}' not in vocabulary. Skipping.")

        neg_vecs = []
        for word in neg_seeds:
            if word in self.vectors:
                neg_vecs.append(self.vectors[word])
            else:
                logger.warning(f"Negative seed '{word}' not in vocabulary. Skipping.")

        assert len(pos_vecs) >= 5, f"Too few positive seeds found: {len(pos_vecs)}"
        assert len(neg_vecs) >= 5, f"Too few negative seeds found: {len(neg_vecs)}"

        pos_centroid = np.mean(pos_vecs, axis=0)
        neg_centroid = np.mean(neg_vecs, axis=0)

        axis = pos_centroid - neg_centroid
        axis_norm = np.linalg.norm(axis)
        assert axis_norm > 0, "Sentiment axis has zero norm"
        axis = axis / axis_norm

        logger.info(f"Sentiment axis built from {len(pos_vecs)} positive "
                     f"and {len(neg_vecs)} negative seed words.")
        return axis

    @lru_cache(maxsize=50000)
    def get_valence(self, word: str) -> float:
        """Get the valence score for a single word.

        Args:
            word: The word to score (should be lowercased lemma).

        Returns:
            Float valence score. Positive = positive sentiment, negative = negative.
            Returns fallback_valence (0.0) for OOV words.
        """
        word_lower = word.lower()
        if word_lower not in self.vectors:
            return self.fallback_valence

        word_vec = self.vectors[word_lower]
        # Cosine similarity with sentiment axis
        dot_product = np.dot(word_vec, self.sentiment_axis)
        word_norm = np.linalg.norm(word_vec)

        if word_norm == 0:
            return self.fallback_valence

        valence = dot_product / word_norm
        return float(valence)

    def validate_axis(self) -> dict:
        """Sanity check: verify that known positive/negative words score correctly.

        Returns:
            dict with validation results.
        """
        test_positive = ["happy", "excellent", "wonderful", "beautiful", "great"]
        test_negative = ["terrible", "horrible", "awful", "disgusting", "miserable"]
        test_neutral = ["table", "chair", "walk", "number", "Tuesday"]

        pos_scores = [self.get_valence(w) for w in test_positive]
        neg_scores = [self.get_valence(w) for w in test_negative]
        neu_scores = [self.get_valence(w) for w in test_neutral]

        results = {
            "pos_mean": float(np.mean(pos_scores)),
            "neg_mean": float(np.mean(neg_scores)),
            "neu_mean": float(np.mean(neu_scores)),
            "pos_all_positive": all(s > 0 for s in pos_scores),
            "neg_all_negative": all(s < 0 for s in neg_scores),
            "neu_near_zero": abs(np.mean(neu_scores)) < 0.3,
            "separation": float(np.mean(pos_scores) - np.mean(neg_scores)),
        }

        logger.info(f"Axis validation: pos_mean={results['pos_mean']:.3f}, "
                     f"neg_mean={results['neg_mean']:.3f}, "
                     f"neu_mean={results['neu_mean']:.3f}, "
                     f"separation={results['separation']:.3f}")

        return results
```

### Module B: Spatial Positional Decay

Create file `sgw/core/decay/functions.py`:
```python
"""Spatial decay functions for positional weighting.

The decay function reduces the contribution of tokens based on their position
in the document. Tokens at the beginning contribute more; tokens at the end
contribute less. This models the psycholinguistic primacy effect.

Reference:
  Ebbinghaus (1885). Serial position effect.
  Asch (1946). Primacy in impression formation.
  Gjergjizi, Tran & Felfernig (2022). PriRec-BiLSTM.
  Press, Smith & Lewis (2022). ALiBi.
"""

import numpy as np
from sgw.utils.logger import get_logger

logger = get_logger(__name__)


def exponential_decay(position: int, total_length: int, lambda_c: float) -> float:
    """Compute exponential decay weight for a token at a given position.

    Formula: w(i) = exp(-lambda * i / L)

    Args:
        position: Token position (0-indexed).
        total_length: Total number of tokens in the document.
        lambda_c: Decay constant. Higher = faster decay.

    Returns:
        Decay weight in (0, 1]. Position 0 always returns 1.0.

    Edge cases:
        - total_length <= 0: returns 1.0
        - position < 0: returns 1.0
        - position >= total_length: uses position/total_length as-is
    """
    if total_length <= 0 or position <= 0:
        return 1.0

    relative_position = position / total_length
    weight = np.exp(-lambda_c * relative_position)
    return float(weight)


def compute_decay_weights(num_tokens: int, lambda_c: float) -> np.ndarray:
    """Compute decay weights for all positions in a document.

    This is the vectorized version for efficiency on large documents.

    Args:
        num_tokens: Number of tokens in the document.
        lambda_c: Decay constant.

    Returns:
        numpy array of shape (num_tokens,) with decay weights.
    """
    if num_tokens <= 0:
        return np.array([])

    if num_tokens == 1:
        return np.array([1.0])

    positions = np.arange(num_tokens, dtype=np.float64)
    weights = np.exp(-lambda_c * positions / num_tokens)
    return weights


def no_decay(num_tokens: int) -> np.ndarray:
    """Return uniform weights (no decay). Used for ablation studies.

    Args:
        num_tokens: Number of tokens.

    Returns:
        numpy array of ones with shape (num_tokens,).
    """
    return np.ones(num_tokens)
```

### Module C: Syntactic Dilution (Buffer Ratio)

Create file `sgw/core/dilution/buffer.py`:
```python
"""Syntactic dilution via buffer ratio.

Buffer words (function words: determiners, conjunctions, prepositions, etc.)
dilute the emotional impact of a text. The more "filler" a text contains,
the less intense its sentiment score should be.

This formalizes the observation from the original SGW manuscript that
"sentiment itself gets diluted as a sentence lengthens" due to neutral
and buffer words.

The dilution factor is: D(B) = exp(-k * B)
Where B = buffer_count / total_count and k is a tunable constant.

When B = 0 (all emotion words), D = 1.0 (no dilution).
When B = 1 (all buffer words), D = exp(-k) ≈ 0.05 for k=3.
"""

import numpy as np
from sgw.utils.logger import get_logger

logger = get_logger(__name__)


def compute_buffer_ratio(buffer_count: int, total_count: int) -> float:
    """Compute the buffer ratio for a document.

    Args:
        buffer_count: Number of buffer (function) word tokens.
        total_count: Total number of tokens.

    Returns:
        Buffer ratio in [0, 1]. Returns 0.0 if total_count is 0.
    """
    if total_count <= 0:
        return 0.0
    return float(buffer_count / total_count)


def dilution_factor(buffer_ratio: float, k: float) -> float:
    """Compute the dilution factor from the buffer ratio.

    Formula: D(B) = exp(-k * B)

    Args:
        buffer_ratio: Ratio of buffer words to total words, in [0, 1].
        k: Dilution constant. Higher = more dilution per buffer word.

    Returns:
        Dilution factor in (0, 1].
    """
    return float(np.exp(-k * buffer_ratio))


def no_dilution() -> float:
    """Return 1.0 (no dilution). Used for ablation studies."""
    return 1.0
```

### Step 2.1: Implement the Syntax Parser
Create file `sgw/pipeline/syntax.py`:
```python
"""Syntactic parsing utilities using spaCy.

Extracts dependency-aware features: negation, modifiers, POS tags, buffer counts.
"""

import spacy
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

    def get_modifier_weight(self, token) -> float:
        """Get the intensity modifier weight for a token.

        Scans children with advmod dependency for known intensifiers/mitigators.

        Returns:
            Multiplier: >1.0 for intensifiers, <1.0 for mitigators, 1.0 otherwise.
        """
        for child in token.children:
            if child.dep_ == "advmod":
                child_lower = child.lower_
                if child_lower in self.intensifiers:
                    return self.intensifiers[child_lower]
                if child_lower in self.mitigators:
                    return self.mitigators[child_lower]
        return 1.0

    def count_buffers(self, doc) -> int:
        """Count buffer tokens in a document."""
        return sum(1 for token in doc if self.is_buffer(token) and not token.is_space)

    def count_tokens(self, doc) -> int:
        """Count non-space tokens in a document."""
        return sum(1 for token in doc if not token.is_space)
```

### Step 2.2: Integrate into the SGW Engine
Create file `sgw/core/engine.py`:
```python
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
        # Edge case: empty or very short text
        min_len = self.config["processing"]["min_text_length"]
        if not isinstance(text, str) or len(text.strip()) < min_len:
            return 0.0

        # Parse with spaCy
        doc = self.syntax_parser.parse(text)
        total_tokens = self.syntax_parser.count_tokens(doc)

        if total_tokens == 0:
            return 0.0

        # Compute decay weights
        if self._use_decay:
            decay_weights = compute_decay_weights(total_tokens, self.lambda_c)
        else:
            decay_weights = no_decay(total_tokens)

        # Compute buffer ratio
        buffer_count = self.syntax_parser.count_buffers(doc)
        buffer_ratio = compute_buffer_ratio(buffer_count, total_tokens)

        # Compute dilution factor
        if self._use_dilution:
            dilution = dilution_factor(buffer_ratio, self.k)
        else:
            dilution = no_dilution()

        # Score each emotion-carrying token
        energies = []
        token_idx = 0  # Track position among non-space tokens
        for token in doc:
            if token.is_space:
                continue

            if self.syntax_parser.is_emotion_carrier(token):
                # Base valence from embedding projection
                v = self.valence_scorer.get_valence(token.lemma_)

                # Apply negation
                if self.syntax_parser.is_negated(token):
                    v = -v

                # Apply modifier
                modifier = self.syntax_parser.get_modifier_weight(token)
                v = v * modifier

                # Apply positional decay
                v = v * decay_weights[token_idx]

                energies.append(v)

            token_idx += 1

        if len(energies) == 0:
            return 0.0

        # Sum and apply dilution
        total_energy = np.sum(energies)
        diluted_energy = total_energy * dilution

        # Normalize by number of emotion tokens to prevent length bias
        # Without this, longer texts would always have higher absolute scores
        normalized = diluted_energy / len(energies)

        # Clip to [-1, 1]
        score = float(np.clip(normalized, -1.0, 1.0))
        return score

    def score_batch(self, texts: list, show_progress: bool = True) -> np.ndarray:
        """Score a batch of texts.

        Args:
            texts: List of text strings.
            show_progress: Whether to show progress bar.

        Returns:
            numpy array of scores.
        """
        scores = []
        iterator = enumerate(texts)
        if show_progress:
            from tqdm import tqdm
            iterator = tqdm(enumerate(texts), total=len(texts), desc="SGW Scoring")

        for i, text in iterator:
            scores.append(self.score(text))

        return np.array(scores)
```

### Step 2.3: Unit Tests for All Core Modules
Create file `tests/unit/test_core.py`:
```python
"""Unit tests for SGW core modules."""

import pytest
import numpy as np


# --- Decay Function Tests ---

class TestDecayFunctions:
    def test_position_zero_returns_one(self):
        from sgw.core.decay.functions import exponential_decay
        assert exponential_decay(0, 100, 1.0) == 1.0

    def test_decay_decreases_with_position(self):
        from sgw.core.decay.functions import exponential_decay
        w1 = exponential_decay(10, 100, 1.0)
        w2 = exponential_decay(50, 100, 1.0)
        w3 = exponential_decay(90, 100, 1.0)
        assert w1 > w2 > w3

    def test_higher_lambda_faster_decay(self):
        from sgw.core.decay.functions import exponential_decay
        w_low = exponential_decay(50, 100, 0.5)
        w_high = exponential_decay(50, 100, 2.0)
        assert w_low > w_high

    def test_vectorized_decay_matches_scalar(self):
        from sgw.core.decay.functions import exponential_decay, compute_decay_weights
        weights = compute_decay_weights(10, 1.5)
        for i in range(10):
            expected = exponential_decay(i, 10, 1.5)
            assert abs(weights[i] - expected) < 1e-10

    def test_empty_document_decay(self):
        from sgw.core.decay.functions import compute_decay_weights
        weights = compute_decay_weights(0, 1.0)
        assert len(weights) == 0

    def test_single_token_decay(self):
        from sgw.core.decay.functions import compute_decay_weights
        weights = compute_decay_weights(1, 1.0)
        assert len(weights) == 1
        assert weights[0] == 1.0


# --- Buffer Ratio Tests ---

class TestDilution:
    def test_zero_buffers_no_dilution(self):
        from sgw.core.dilution.buffer import compute_buffer_ratio, dilution_factor
        ratio = compute_buffer_ratio(0, 100)
        assert ratio == 0.0
        assert dilution_factor(ratio, 2.0) == 1.0

    def test_all_buffers_strong_dilution(self):
        from sgw.core.dilution.buffer import compute_buffer_ratio, dilution_factor
        ratio = compute_buffer_ratio(100, 100)
        assert ratio == 1.0
        d = dilution_factor(ratio, 2.0)
        assert d < 0.2  # exp(-2) ≈ 0.135

    def test_half_buffers(self):
        from sgw.core.dilution.buffer import compute_buffer_ratio, dilution_factor
        ratio = compute_buffer_ratio(50, 100)
        assert abs(ratio - 0.5) < 1e-10
        d = dilution_factor(ratio, 2.0)
        assert abs(d - np.exp(-1.0)) < 1e-10  # exp(-2 * 0.5) = exp(-1)

    def test_empty_document(self):
        from sgw.core.dilution.buffer import compute_buffer_ratio
        assert compute_buffer_ratio(0, 0) == 0.0


# --- Valence Scorer Tests ---
# These require vectors to be loaded, so they may be slow.

class TestValenceScorer:
    @pytest.fixture(scope="class")
    def scorer(self):
        from sgw.utils.vectors import load_vectors
        from sgw.core.valence.scorer import ValenceScorer
        # Load limited vectors for speed
        vectors = load_vectors(limit=100000)
        return ValenceScorer(vectors)

    def test_positive_words_score_positive(self, scorer):
        for word in ["happy", "excellent", "wonderful"]:
            v = scorer.get_valence(word)
            assert v > 0, f"'{word}' should be positive, got {v}"

    def test_negative_words_score_negative(self, scorer):
        for word in ["terrible", "horrible", "awful"]:
            v = scorer.get_valence(word)
            assert v < 0, f"'{word}' should be negative, got {v}"

    def test_oov_returns_fallback(self, scorer):
        v = scorer.get_valence("asdfghjklzxcv")
        assert v == 0.0

    def test_axis_validation(self, scorer):
        results = scorer.validate_axis()
        assert results["pos_all_positive"], "Not all positive test words scored positive"
        assert results["neg_all_negative"], "Not all negative test words scored negative"
        assert results["separation"] > 0.2, f"Insufficient axis separation: {results['separation']}"
```

Run tests: `python -m pytest tests/unit/test_core.py -v`

### GATE CHECK 2: Core Module Validation
Create and run file `tests/gate_check_2.py`:
```python
"""Gate Check 2: Verify all core modules work independently."""

import numpy as np

print("=" * 60)
print("GATE CHECK 2: Core Module Validation")
print("=" * 60)

# Test 1: Valence scorer axis quality
from sgw.utils.vectors import load_vectors
from sgw.core.valence.scorer import ValenceScorer

vectors = load_vectors(limit=200000)
scorer = ValenceScorer(vectors)
validation = scorer.validate_axis()
assert validation["pos_all_positive"], "Axis fails: positive words not all positive"
assert validation["neg_all_negative"], "Axis fails: negative words not all negative"
assert validation["separation"] > 0.2, f"Axis separation too low: {validation['separation']}"
print(f"[PASS] Valence axis: separation={validation['separation']:.3f}")

# Test 2: Decay function mathematical properties
from sgw.core.decay.functions import compute_decay_weights
weights = compute_decay_weights(100, 1.0)
assert weights[0] == 1.0, "First position weight must be 1.0"
assert all(weights[i] >= weights[i+1] for i in range(len(weights)-1)), "Weights must be monotonically decreasing"
assert weights[-1] > 0, "Weights must never reach exactly 0"
print(f"[PASS] Decay: monotonically decreasing, first={weights[0]:.3f}, last={weights[-1]:.3f}")

# Test 3: Dilution mathematical properties
from sgw.core.dilution.buffer import dilution_factor
assert dilution_factor(0.0, 2.0) == 1.0, "Zero buffer ratio must produce no dilution"
assert dilution_factor(1.0, 2.0) < 0.2, "Full buffer ratio must produce strong dilution"
d_half = dilution_factor(0.5, 2.0)
assert 0.3 < d_half < 0.5, f"Half buffer dilution out of range: {d_half}"
print(f"[PASS] Dilution: D(0)=1.0, D(0.5)={d_half:.3f}, D(1.0)={dilution_factor(1.0, 2.0):.3f}")

# Test 4: SGW Engine produces scores in correct range
from sgw.pipeline.syntax import SyntaxParser
from sgw.core.engine import SGWEngine

parser = SyntaxParser()
engine = SGWEngine(scorer, parser)

test_cases = [
    ("I am extremely happy today!", "positive"),
    ("This is absolutely terrible and disgusting.", "negative"),
    ("The table is in the room.", "neutral"),
    ("", "zero"),
    ("I am not happy.", "should be negative due to negation"),
]

for text, expected_direction in test_cases:
    score = engine.score(text)
    assert -1.0 <= score <= 1.0, f"Score out of range for '{text}': {score}"
    print(f"  '{text[:50]}...' -> {score:.4f} ({expected_direction})")

# Verify extreme positive scores positive
pos_score = engine.score("I am extremely happy today!")
assert pos_score > 0, f"Happy sentence should be positive, got {pos_score}"

# Verify extreme negative scores negative
neg_score = engine.score("This is absolutely terrible and disgusting.")
assert neg_score < 0, f"Terrible sentence should be negative, got {neg_score}"

# Verify empty returns 0
assert engine.score("") == 0.0
assert engine.score("   ") == 0.0

print("[PASS] SGW Engine: scores in range, directional correctness verified")

# Test 5: Unit tests all pass
import subprocess
result = subprocess.run(["python", "-m", "pytest", "tests/unit/test_core.py", "-v", "--tb=short"],
                       capture_output=True, text=True)
print(result.stdout)
if result.returncode != 0:
    print(result.stderr)
    raise AssertionError("Unit tests failed. Fix before proceeding.")
print("[PASS] All unit tests pass")

print("=" * 60)
print("GATE CHECK 2: ALL TESTS PASSED")
print("=" * 60)
```

Run: `python tests/gate_check_2.py`

**If negation test fails** (e.g., "I am not happy" scores positive): Check that `is_negated` correctly traverses spaCy dependency children. Debug with: `python -c "import spacy; nlp=spacy.load('en_core_web_trf'); doc=nlp('I am not happy'); print([(t.text, t.dep_, [c.text for c in t.children]) for t in doc])"`.

---

## TIER 3: CALIBRATION, EVALUATION, AND ABLATION

### Goal
Calibrate λ and k against GoEmotions, run ablation studies, compare against VADER, and verify all success criteria. This tier produces the final publishable numbers.

### Step 3.1: Calibration Script
Create file `sgw/evaluation/calibrate.py`:
```python
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

    best_rho = -1.0
    best_lambda = None
    best_k = None
    all_results = []

    for lam, k in itertools.product(lambda_range, k_range):
        engine.set_lambda(lam)
        engine.set_k(k)
        engine.enable_decay()
        engine.enable_dilution()

        y_pred = engine.score_batch(texts, show_progress=False)
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
```

### Step 3.2: Ablation Study Script
Create file `sgw/evaluation/ablation.py`:
```python
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
```

### Step 3.3: Full Evaluation Runner
Create file `sgw/evaluation/run_evaluation.py`:
```python
"""Master evaluation script. Runs calibration, ablation, and final benchmarks.

This is the script that determines whether SGW meets its success criteria.
"""

import json
import sys
import numpy as np
from sgw.utils.vectors import load_vectors
from sgw.utils.config import load_config
from sgw.core.valence.scorer import ValenceScorer
from sgw.pipeline.syntax import SyntaxParser
from sgw.core.engine import SGWEngine
from sgw.evaluation.goemotions_loader import load_goemotions
from sgw.evaluation.baselines import compute_vader_scores
from sgw.evaluation.metrics import evaluate_continuous
from sgw.evaluation.calibrate import calibrate
from sgw.evaluation.ablation import run_ablation
from sgw.utils.logger import get_logger

logger = get_logger(__name__)


def main():
    print("=" * 70)
    print("SGW V3 FULL EVALUATION PIPELINE")
    print("=" * 70)

    # --- Setup ---
    logger.info("Loading vectors...")
    vectors = load_vectors()
    scorer = ValenceScorer(vectors)
    parser = SyntaxParser()
    engine = SGWEngine(scorer, parser)

    # --- Phase 1: Calibrate on validation set ---
    logger.info("\n" + "=" * 50)
    logger.info("PHASE 1: CALIBRATION (validation set)")
    logger.info("=" * 50)
    best_lambda, best_k = calibrate(engine)
    logger.info(f"Optimal parameters: λ={best_lambda}, k={best_k}")

    # --- Phase 2: Evaluate on test set ---
    logger.info("\n" + "=" * 50)
    logger.info("PHASE 2: FINAL EVALUATION (test set)")
    logger.info("=" * 50)

    df_test = load_goemotions("test")
    texts = df_test["text"].tolist()
    y_true = df_test["human_valence"].values

    # SGW scores
    y_sgw = engine.score_batch(texts, show_progress=True)
    sgw_results = evaluate_continuous(y_true, y_sgw, "SGW (Calibrated)")

    # VADER scores
    y_vader = compute_vader_scores(df_test["text"])
    vader_results = evaluate_continuous(y_true, y_vader, "VADER")

    # --- Phase 3: Ablation studies ---
    logger.info("\n" + "=" * 50)
    logger.info("PHASE 3: ABLATION STUDIES")
    logger.info("=" * 50)
    ablation = run_ablation(engine)

    # --- Phase 4: Gate checks ---
    logger.info("\n" + "=" * 50)
    logger.info("PHASE 4: SUCCESS CRITERIA VERIFICATION")
    logger.info("=" * 50)

    gates = {}

    # Gate 1: Spearman rho >= 0.35
    gates["spearman_ge_035"] = sgw_results["spearman_rho"] >= 0.35
    logger.info(f"Gate 1 - Spearman ρ ≥ 0.35: {sgw_results['spearman_rho']:.4f} -> "
                f"{'PASS' if gates['spearman_ge_035'] else 'FAIL'}")

    # Gate 2: Beat VADER
    gates["beats_vader"] = sgw_results["spearman_rho"] > vader_results["spearman_rho"]
    logger.info(f"Gate 2 - Beats VADER: SGW ρ={sgw_results['spearman_rho']:.4f} vs "
                f"VADER ρ={vader_results['spearman_rho']:.4f} -> "
                f"{'PASS' if gates['beats_vader'] else 'FAIL'}")

    # Gate 3: MAE <= 0.45
    gates["mae_le_045"] = sgw_results["mae"] <= 0.45
    logger.info(f"Gate 3 - MAE ≤ 0.45: {sgw_results['mae']:.4f} -> "
                f"{'PASS' if gates['mae_le_045'] else 'FAIL'}")

    # Gate 4: Decay helps
    gates["decay_helps"] = ablation["decay_contribution"] > 0
    logger.info(f"Gate 4 - Decay helps: Δρ={ablation['decay_contribution']:.4f} -> "
                f"{'PASS' if gates['decay_helps'] else 'FAIL'}")

    # Gate 5: Dilution helps
    gates["dilution_helps"] = ablation["dilution_contribution"] > 0
    logger.info(f"Gate 5 - Dilution helps: Δρ={ablation['dilution_contribution']:.4f} -> "
                f"{'PASS' if gates['dilution_helps'] else 'FAIL'}")

    # --- Save all results ---
    final_report = {
        "calibrated_lambda": best_lambda,
        "calibrated_k": best_k,
        "sgw_results": sgw_results,
        "vader_results": vader_results,
        "ablation": {
            "full_rho": ablation["full_model"]["spearman_rho"],
            "no_decay_rho": ablation["no_decay"]["spearman_rho"],
            "no_dilution_rho": ablation["no_dilution"]["spearman_rho"],
            "valence_only_rho": ablation["valence_only"]["spearman_rho"],
            "decay_contribution": ablation["decay_contribution"],
            "dilution_contribution": ablation["dilution_contribution"],
        },
        "gates": gates,
        "all_gates_passed": all(gates.values()),
    }

    with open("reports/metrics/final_evaluation.json", "w") as f:
        json.dump(final_report, f, indent=2, default=str)

    # --- Final verdict ---
    print("\n" + "=" * 70)
    if all(gates.values()):
        print("ALL GATES PASSED. Proceed to Tier 4 (deployment and polish).")
    else:
        failed = [k for k, v in gates.items() if not v]
        print(f"GATES FAILED: {failed}")
        print("DO NOT PROCEED TO TIER 4.")
        print("Debug the failing components and re-run this evaluation.")
        print("If decay or dilution gates fail, the mathematical contributions")
        print("may not be significant on this dataset. Consider:")
        print("  1. Reviewing the seed word list for the valence axis")
        print("  2. Expanding the lambda/k search ranges")
        print("  3. Checking if the GoEmotions valence mapping is appropriate")
        sys.exit(1)
    print("=" * 70)


if __name__ == "__main__":
    main()
```

### GATE CHECK 3: Run Full Evaluation
```bash
python sgw/evaluation/run_evaluation.py
```

This will take several hours due to scoring ~5k test examples with spaCy TRF. The output will print PASS/FAIL for each gate. The final report is saved to `reports/metrics/final_evaluation.json`.

**If Gate 1 or 2 fail** (low correlation or doesn't beat VADER): The most likely causes are (a) the seed word list produces a poor sentiment axis — try adding domain-specific words; (b) the normalization in `engine.score()` is dividing by too many emotion tokens — try summing without normalizing; (c) the buffer POS tag list is too broad or too narrow.

**If Gate 4 or 5 fail** (decay/dilution don't help): This means the mathematical innovations don't improve over raw SemAxis projection for GoEmotions comments. GoEmotions comments are short (average ~12 words), so positional decay and dilution may have minimal impact on short texts. This is a legitimate scientific finding. Document it. Consider testing on longer-form datasets (Reddit posts, Yelp reviews) where the effects would be more pronounced.

**DO NOT PROCEED TO TIER 4 UNTIL ALL GATES PASS OR FAILURES ARE DOCUMENTED AS SCIENTIFIC FINDINGS.**

---

## TIER 4: DEPLOYMENT, UI, AND DOCUMENTATION

### Prerequisite
Tier 3 completed with either (a) all gates passed, or (b) failures documented with explanation.

### Step 4.1: FastAPI Service
Create file `sgw/api/main.py`:
```python
"""FastAPI service for the SGW engine."""

from fastapi import FastAPI, HTTPException
from pydantic import BaseModel, Field
from sgw.utils.vectors import load_vectors
from sgw.utils.config import load_config
from sgw.core.valence.scorer import ValenceScorer
from sgw.pipeline.syntax import SyntaxParser
from sgw.core.engine import SGWEngine

app = FastAPI(
    title="SGW Sentiment Engine",
    description="Unsupervised mathematical sentiment scoring via spatial decay and vector valence.",
    version="3.0.0",
)


class TextRequest(BaseModel):
    text: str = Field(..., min_length=1, max_length=10000, description="Text to analyze.")


class SentimentResponse(BaseModel):
    score: float = Field(..., description="Sentiment score in [-1.0, 1.0].")
    lambda_c: float = Field(..., description="Decay constant used.")
    k: float = Field(..., description="Dilution constant used.")


_engine = None


@app.on_event("startup")
def load_models():
    global _engine
    config = load_config()
    vectors = load_vectors()
    scorer = ValenceScorer(vectors, config)
    parser = SyntaxParser(config)
    _engine = SGWEngine(scorer, parser, config)

    # Load calibrated parameters if available
    import json
    from pathlib import Path
    cal_path = Path("reports/metrics/calibration_results.json")
    if cal_path.exists():
        with open(cal_path) as f:
            cal = json.load(f)
        _engine.set_lambda(cal["best_lambda"])
        _engine.set_k(cal["best_k"])


@app.post("/analyze", response_model=SentimentResponse)
async def analyze(req: TextRequest):
    if _engine is None:
        raise HTTPException(status_code=503, detail="Model not loaded.")
    score = _engine.score(req.text)
    return SentimentResponse(score=score, lambda_c=_engine.lambda_c, k=_engine.k)


@app.get("/health")
async def health():
    return {"status": "ok", "model_loaded": _engine is not None}
```

### Step 4.2: Dockerfile
Create file `Dockerfile`:
```dockerfile
FROM python:3.10-slim

WORKDIR /app

RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential wget && rm -rf /var/lib/apt/lists/*

COPY requirements.txt .
RUN pip install --no-cache-dir -r requirements.txt
RUN python -m spacy download en_core_web_trf

COPY sgw/ sgw/
COPY configs/ configs/
COPY reports/metrics/ reports/metrics/

EXPOSE 8000
CMD ["uvicorn", "sgw.api.main:app", "--host", "0.0.0.0", "--port", "8000"]
```

### Step 4.3: README.md
Create file `README.md`:
```markdown
# Sentiment Gummy Worm (SGW) V3

**Unsupervised mathematical sentiment scoring via spatial decay, vector valence projection, and syntactic dilution.**

SGW produces continuous sentiment scores from -1.0 to +1.0 for arbitrary English text without requiring any labeled training data.

## Quick Start

```python
from sgw.utils.vectors import load_vectors
from sgw.core.valence.scorer import ValenceScorer
from sgw.pipeline.syntax import SyntaxParser
from sgw.core.engine import SGWEngine

vectors = load_vectors()
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
```

### Step 4.4: CI/CD
Create file `.github/workflows/ci.yml`:
```yaml
name: SGW CI

on: [push, pull_request]

jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: actions/setup-python@v5
        with:
          python-version: "3.10"
      - run: pip install -r requirements.txt
      - run: python -m spacy download en_core_web_sm
      - run: python -m pytest tests/unit/ -v --tb=short
```

### Step 4.5: Final Commit and Tag
```bash
git add -A
git commit -m "SGW V3: Unsupervised spatial decay sentiment engine"
git tag -a v3.0.0 -m "SGW V3 initial release"
git push origin main
git push origin v3.0.0
```

---

## EXECUTION ORDER SUMMARY

```
TIER 0 (Steps 0.1-0.9) -> Gate Check 0
    ↓ (all 4 tests pass)
TIER 1 (Steps 1.1-1.4) -> Gate Check 1
    ↓ (all assertions pass, VADER baseline saved)
TIER 2 (Steps 2.1-2.3, Module A/B/C) -> Gate Check 2
    ↓ (engine produces directionally correct scores, unit tests pass)
TIER 3 (Steps 3.1-3.3) -> Gate Check 3
    ↓ (all 5 gates pass OR failures documented)
TIER 4 (Steps 4.1-4.5)
    ↓
DONE
```

**Total estimated execution time**: 6-12 hours depending on hardware and internet speed. The bottlenecks are (1) GloVe download (~30 min), (2) GoEmotions scoring with spaCy TRF (~2-4 hours for calibration + evaluation), and (3) full vector loading (~5 min).

**If running on a machine without GPU**: Change `spacy_model` in config to `en_core_web_sm`. This reduces dependency parsing accuracy but cuts processing time by ~10x. Document this as a limitation.
