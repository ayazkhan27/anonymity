import pandas as pd
from datasets import load_dataset
from sgw.utils.logger import get_logger

logger = get_logger(__name__)

def map_sst5_to_valence(label: int) -> float:
    # 0 = very negative, 1 = negative, 2 = neutral, 3 = positive, 4 = very positive
    # map to [-1, 1]
    return (label - 2.0) / 2.0

def map_yelp_to_valence(label: int) -> float:
    # labels are 0-4 for 1-5 stars
    return (label - 2.0) / 2.0

def map_imdb_to_valence(label: int) -> float:
    # 0 = negative, 1 = positive
    # map to -1, 1
    return 1.0 if label == 1 else -1.0

def load_benchmark(name: str, split: str = "test", limit: int = None) -> pd.DataFrame:
    """Load benchmark datasets and map their labels to continuous valence in [-1, 1].
    
    Args:
        name: Name of benchmark ("sst5", "yelp", "imdb")
        split: Split to load (usually "test")
        limit: Max samples to load (for speed during development/debugging)
    """
    logger.info(f"Loading {name} benchmark (split={split})...")
    
    if name == "sst5":
        # Using a reliable SST-5 repo on HF
        dataset = load_dataset("SetFit/sst5", split=split)
        df = dataset.to_pandas()
        df["human_valence"] = df["label"].apply(map_sst5_to_valence)
        
    elif name == "yelp":
        dataset = load_dataset("yelp_review_full", split=split)
        df = dataset.to_pandas()
        df["human_valence"] = df["label"].apply(map_yelp_to_valence)
        
    elif name == "imdb":
        dataset = load_dataset("imdb", split=split)
        df = dataset.to_pandas()
        df["human_valence"] = df["label"].apply(map_imdb_to_valence)
        
    else:
        raise ValueError(f"Unknown benchmark: {name}")
        
    if limit and limit < len(df):
        df = df.sample(n=limit, random_state=42).reset_index(drop=True)
        
    logger.info(f"Loaded {len(df)} samples from {name} ({split}).")
    return df
