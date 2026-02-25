from sgw.utils.vectors import load_vectors
from sgw.core.valence.axis import SentimentAxis
from sgw.core.valence.scorer import ValenceScorer
from sgw.utils.config import load_config

config = load_config()
vectors = load_vectors(limit=300000)
scorer = ValenceScorer(vectors, config=config)
threshold = scorer.axis.sigma_v_aff * config["valence"].get("valence_threshold_factor", 0.5)

print(f"Sigma_V_aff = {scorer.axis.sigma_v_aff:.4f}, Threshold = {threshold:.4f}")

print("\n# These should be ABOVE threshold (kept)")
for word in ["happy", "terrible", "excellent", "awful", "thrilled"]:
    v = scorer.get_valence(word)
    print(f"  {word:15s} v={v:+.4f}  |v|={abs(v):.4f}  threshold={threshold:.4f}  {'KEEP' if abs(v) >= threshold else 'ZERO'}")

print("\n# These should be BELOW threshold (zeroed)")
for word in ["room", "table", "wall", "painted", "corner", "rectangular", "chair", "floor", "wood", "hung"]:
    v = scorer.get_valence(word)
    print(f"  {word:15s} v={v:+.4f}  |v|={abs(v):.4f}  threshold={threshold:.4f}  {'KEEP' if abs(v) >= threshold else 'ZERO'}")
