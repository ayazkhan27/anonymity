from sgw.core.engine import SGWEngine
from sgw.core.valence.scorer import ValenceScorer
from sgw.pipeline.syntax import SyntaxParser
from sgw.utils.vectors import load_vectors

import logging
logging.basicConfig(level=logging.INFO)

def debug_dilution_energy():
    vectors = load_vectors(limit=300000)
    scorer = ValenceScorer(vectors)
    parser = SyntaxParser()
    engine = SGWEngine(scorer, parser)

    base = "I am absolutely thrilled with this amazing service. It is wonderful!"
    padding = " data data data. "
    
    padded_5 = base + padding
    
    expl = engine.explain(padded_5)
    
    print("\nToken/Lemma Trace:")
    for t in expl.token_contributions:
        if "data" in t.token.lower():
            v = scorer.get_valence(t.lemma)
            print(f"  Pos: {t.position:3} | Token: {repr(t.token):15} | Lemma: {repr(t.lemma):15} | ScorerVal: {v:.4f}")

if __name__ == "__main__":
    debug_dilution_energy()
