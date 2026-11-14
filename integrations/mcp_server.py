"""MCP server: gives a language model the scorer and the tone check as tools.

    pip install mcp
    python3 integrations/mcp_server.py                                   # stdio transport
    claude mcp add sgw -- python3 /absolute/path/to/integrations/mcp_server.py
"""
import os, sys

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), ".."))
from mcp.server.fastmcp import FastMCP

import sgw
from sgw import tone

mcp = FastMCP("sgw")


@mcp.tool()
def score_text(text: str) -> dict:
    """Score a text of any length. Returns words, hedons (positive charge), dolors (negative charge),
    density (net charge per 1,000 words), level with its standard error, a calibrated valence in
    standard-deviation units, and the arc of net charge at the start, quarter points and end."""
    return sgw.read(text)


@mcp.tool()
def tone_check(text: str, genre: str = "essay") -> str:
    """Compare a draft with the range found in human writing of one kind: "essay" (student essays),
    "answer" (answers to questions), "review" or "fiction". Returns a short report that names each
    tone measure outside the middle 80% of human texts, with the human range and a suggestion."""
    if genre not in tone.reference():
        return f"Unknown genre {genre!r}. Choose one of: {', '.join(tone.reference())}."
    return tone.report(text, genre)


@mcp.tool()
def tone_numbers(text: str, genre: str = "essay") -> dict:
    """The tone check as numbers: each measure of the draft, the human median and range, and a z-score."""
    return tone.compare(text, genre)


if __name__ == "__main__":
    mcp.run()
