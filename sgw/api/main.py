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
    vectors = load_vectors(limit=500000)
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
