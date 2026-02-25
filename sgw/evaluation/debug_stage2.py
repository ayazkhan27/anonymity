from sgw.core.engine import SGWEngine
from sgw.core.valence.scorer import ValenceScorer
from sgw.pipeline.syntax import SyntaxParser
from sgw.utils.vectors import load_vectors
from sgw.utils.logger import get_logger

logger = get_logger(__name__)

def debug_logic():
    # Load resources
    vectors = load_vectors(limit=100000)
    scorer = ValenceScorer(vectors)
    parser = SyntaxParser()
    engine = SGWEngine(scorer, parser)

    # 1. Debug "terrible experience"
    text_neg = "I had a terrible experience, the service was awful"
    expl_neg = engine.explain(text_neg)
    print(f"\n--- Debugging: '{text_neg}' ---")
    print(f"Score: {expl_neg.score:.4f}")
    print(f"Raw Score: {expl_neg.score_raw:.4f}")
    print(f"Emotion Tokens: {expl_neg.emotion_token_count}")
    for t in expl_neg.token_contributions:
        if t.is_emotion_carrier:
            print(f"  Token: {t.token:15} | Base: {t.base_valence:.4f} | Raw: {t.raw_contribution:.4f}")

    # 2. Debug Dilution Case 3
    neutral_sent = " The table is brown. The sun is in the sky. It is three o'clock."
    expl_padded = engine.explain(text_neg + neutral_sent * 5)
    print(f"\n--- Debugging Dilution: '{text_neg}' + 5 neutral ---")
    print(f"Score: {expl_padded.score:.4f}")
    print(f"Raw Score: {expl_padded.score_raw:.4f}")
    print(f"Buffer Ratio: {expl_padded.effective_buffer_ratio:.4f}")
    print(f"Dilution Factor: {expl_padded.dilution_factor:.4f}")

    # 3. Debug Recency Bias
    pos_part = "I love this product, it is fantastic and perfect."
    neg_part = "However, the shipping was terrible and the support was awful."
    
    text_pn = pos_part + " " + neg_part
    text_np = neg_part + " " + pos_part
    
    expl_pn = engine.explain(text_pn)
    expl_np = engine.explain(text_np)
    
    print(f"\n--- Debugging Recency Bias ---")
    print(f"POS then NEG: Score={expl_pn.score:.4f}, Raw={expl_pn.score_raw:.4f}, Pivot={expl_pn.pivot_token}")
    print(f"NEG then POS: Score={expl_np.score:.4f}, Raw={expl_np.score_raw:.4f}, Pivot={expl_np.pivot_token}")

if __name__ == "__main__":
    debug_logic()
