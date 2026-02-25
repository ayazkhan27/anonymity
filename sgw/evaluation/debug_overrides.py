from sgw.core.engine import SGWEngine
from sgw.core.valence.scorer import ValenceScorer
from sgw.pipeline.syntax import SyntaxParser
from sgw.utils.vectors import load_vectors

def debug_overrides():
    vectors = load_vectors(limit=100000)
    scorer = ValenceScorer(vectors)
    overrides = scorer.config.get("valence", {}).get("valence_overrides", {})
    print("\n--- Loaded Overrides ---")
    print(overrides)
    print(f"\n'data' in overrides: {'data' in overrides}")
    if "data" in overrides:
        print(f"Value for 'data': {overrides['data']} (type: {type(overrides['data'])})")
    
    print(f"\nScorer.get_valence('data'): {scorer.get_valence('data')}")

if __name__ == "__main__":
    debug_overrides()
