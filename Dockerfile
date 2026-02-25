FROM python:3.10-slim

WORKDIR /app

RUN apt-get update && apt-get install -y --no-install-recommends \
    build-essential wget && rm -rf /var/lib/apt/lists/*

COPY requirements.txt .
RUN pip install --no-cache-dir -r requirements.txt
RUN python -m spacy download en_core_web_sm

COPY sgw/ sgw/
COPY configs/ configs/
COPY reports/metrics/ reports/metrics/

EXPOSE 8000
CMD ["uvicorn", "sgw.api.main:app", "--host", "0.0.0.0", "--port", "8000"]
