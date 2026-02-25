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

    import time
    limit_str = f" (limit={limit})" if limit else " (ALL ~2.2M)"
    logger.info(f"Loading vectors from {GLOVE_W2V}{limit_str}...")
    print(f"[VECTORS] Starting load{limit_str}... this may take a few minutes.", flush=True)
    t0 = time.time()
    vectors = KeyedVectors.load_word2vec_format(
        str(GLOVE_W2V), binary=False, limit=limit, unicode_errors='ignore'
    )
    elapsed = time.time() - t0
    logger.info(f"Loaded {len(vectors)} word vectors in {elapsed:.1f}s.")
    print(f"[VECTORS] Loaded {len(vectors):,} vectors in {elapsed:.1f}s.", flush=True)
    return vectors
