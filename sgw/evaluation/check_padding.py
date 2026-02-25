from sgw.core.engine import SGWEngine
from sgw.core.valence.scorer import ValenceScorer
from sgw.pipeline.syntax import SyntaxParser
from sgw.utils.vectors import load_vectors

def check_padding_valence():
    vectors = load_vectors(limit=100000)
    scorer = ValenceScorer(vectors)
    padding_words = ["object", "located", "coordinates", "material", "inert", "substance", "process", "active", "entity", "exists"]
    print("\n--- Padding Word Valences ---")
    for w in padding_words:
        print(f"{w:15} | {scorer.get_valence(w):.4f}")

if __name__ == "__main__":
    check_padding_valence()
