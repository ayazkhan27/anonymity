import os
import json
import logging
import pandas as pd
import numpy as np
from scipy import stats
from tqdm import tqdm
import spacy

from sgw.core.engine import SGWEngine
from sgw.utils.config import load_config
from sgw.evaluation.benchmark_loaders import load_yelp_benchmark, load_imdb_benchmark
from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer

logger = logging.getLogger(__name__)

def split_doc(text, nlp):
    doc = nlp(text)
    if len(doc) < 12:  # too short to reasonably split
        return text, text, text
    
    q1 = int(len(doc) * 0.25)
    q3 = int(len(doc) * 0.75)
    
    first = doc[:q1].text
    mid = doc[q1:q3].text
    last = doc[q3:].text
    return first, mid, last

def run_segmentation_benchmark(df, engine, analyzer, nlp, dataset_name):
    logger.info(f"Running segmentation benchmark on {dataset_name} (N={len(df)})")
    
    sgw_full, sgw_first, sgw_mid, sgw_last = [], [], [], []
    vader_full, vader_first, vader_mid, vader_last = [], [], [], []
    true_labels = []
    
    for _, row in tqdm(df.iterrows(), total=len(df), desc=f"Segmenting & Scoring {dataset_name}"):
        text = row['text']
        true_val = row['true_valence']
        true_labels.append(true_val)
        
        # Split text
        first, mid, last = split_doc(text, nlp)
        
        # SGW Parsing
        doc_full = engine.syntax_parser.parse_batch([text])[0]
        doc_first = engine.syntax_parser.parse_batch([first])[0]
        doc_mid = engine.syntax_parser.parse_batch([mid])[0]
        doc_last = engine.syntax_parser.parse_batch([last])[0]
        
        # SGW Scoring
        sgw_full.append(engine.score(doc_full))
        sgw_first.append(engine.score(doc_first))
        sgw_mid.append(engine.score(doc_mid))
        sgw_last.append(engine.score(doc_last))
        
        # VADER Scoring
        vader_full.append(analyzer.polarity_scores(text)['compound'])
        vader_first.append(analyzer.polarity_scores(first)['compound'])
        vader_mid.append(analyzer.polarity_scores(mid)['compound'])
        vader_last.append(analyzer.polarity_scores(last)['compound'])

    results = {}
    
    # Calculate Spearman
    for name, array in [
        ('sgw_full', sgw_full), ('sgw_first', sgw_first), 
        ('sgw_mid', sgw_mid), ('sgw_last', sgw_last),
        ('vader_full', vader_full), ('vader_first', vader_first), 
        ('vader_mid', vader_mid), ('vader_last', vader_last)
    ]:
        rho, _ = stats.spearmanr(true_labels, array)
        results[f"{name}_spearman"] = rho
        
    logger.info(f"--- {dataset_name} Segmentation Correlation ---")
    logger.info(f"SGW Full:   {results['sgw_full_spearman']:.4f}")
    logger.info(f"SGW First:  {results['sgw_first_spearman']:.4f}")
    logger.info(f"SGW Mid:    {results['sgw_mid_spearman']:.4f}")
    logger.info(f"SGW Last:   {results['sgw_last_spearman']:.4f}")
    logger.info(f"VADER Full: {results['vader_full_spearman']:.4f}")
    logger.info(f"VADER Last: {results['vader_last_spearman']:.4f}")
    
    # Correlation between the last segment and the full document score
    rho_inner_sgw, _ = stats.spearmanr(sgw_last, sgw_full)
    rho_inner_vader, _ = stats.spearmanr(vader_last, vader_full)
    
    logger.info(f"SGW Last <-> Full Agreement:   {rho_inner_sgw:.4f}")
    logger.info(f"VADER Last <-> Full Agreement: {rho_inner_vader:.4f}")
    
    results['sgw_last_to_full_rho'] = rho_inner_sgw
    results['vader_last_to_full_rho'] = rho_inner_vader
    
    return results

def main():
    logging.basicConfig(level=logging.INFO, format='%(asctime)s | %(name)s | %(levelname)s | %(message)s')
    
    config = load_config("configs/sgw_config.yaml")
    
    # Load optimal calib params
    calib_path = "reports/metrics/calibration_results.json"
    if os.path.exists(calib_path):
        with open(calib_path, 'r') as f:
            calib = json.load(f)
            lambda_val = calib.get("variance_calibrated_lambda", calib.get("calibrated_lambda", 3.0))
            k_val = calib.get("variance_calibrated_k", calib.get("calibrated_k", 2.0))
            scale_mu_s = calib.get("scale_mu_s", 0.0)
            scale_sigma_s = calib.get("scale_sigma_s", 1.0)
            scale_sigma_y = calib.get("scale_sigma_y", 1.0)
    else:
        lambda_val, k_val = 3.0, 2.0
        scale_mu_s, scale_sigma_s, scale_sigma_y = 0.0, 1.0, 1.0
        
    
    engine = SGWEngine(
        lambda_val=lambda_val, 
        k_val=k_val,
        config_path="configs/sgw_config.yaml",
        vectors_path="data/vectors/glove.840B.300d.txt"
    )
    # Apply variance-matching transformation
    engine.output_clip = lambda x: (x - scale_mu_s) / scale_sigma_s * scale_sigma_y
    
    analyzer = SentimentIntensityAnalyzer()
    
    # fast parser just for splitting quickly
    nlp = spacy.load("en_core_web_sm", disable=["parser", "ner", "lemmatizer"])
    
    # Load subsets for faster benchmarking (1500 limit typically)
    yelp_df = load_yelp_benchmark(split="test", limit=2000)
    imdb_df = load_imdb_benchmark(split="test", limit=2000)
    
    yelp_results = run_segmentation_benchmark(yelp_df, engine, analyzer, nlp, "YELP")
    imdb_results = run_segmentation_benchmark(imdb_df, engine, analyzer, nlp, "IMDB")
    
    final_report = {
        "YELP": yelp_results,
        "IMDB": imdb_results
    }
    
    os.makedirs("reports/metrics", exist_ok=True)
    with open("reports/metrics/segmentation_results.json", 'w') as f:
        json.dump(final_report, f, indent=4)
        
    logger.info("Segmentation analysis complete. Results saved to reports/metrics/segmentation_results.json")

if __name__ == "__main__":
    main()
