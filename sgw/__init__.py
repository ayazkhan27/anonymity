"""Sentiment Gummy Worm. Everything lives in sgw.core; names are looked up there on access,
so tables rebuilt by load_lexicon() are never stale."""
from . import core


def __getattr__(name):
    return getattr(core, name)
