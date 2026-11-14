"""Command line.

    python3 -m sgw "some text"                    score a text (or a file path)
    python3 -m sgw tone draft.txt --genre essay   compare a draft with human writing of one kind
    python3 -m sgw tone draft.txt --json          the same as numbers
"""
import json, os, sys

from . import core, tone


def _text(arg):
    return open(arg, encoding="utf-8", errors="ignore").read() if os.path.isfile(arg) else arg


def main(argv):
    if argv and argv[0] == "tone":
        rest = [a for a in argv[1:] if not a.startswith("--")]
        genre = argv[argv.index("--genre") + 1] if "--genre" in argv else "essay"
        rest = [a for a in rest if a != genre] or [sys.stdin.read()]
        text = _text(" ".join(rest))
        print(json.dumps(tone.compare(text, genre), indent=1) if "--json" in argv else tone.report(text, genre))
    else:
        print(json.dumps(core.read(_text(" ".join(argv)) if argv else sys.stdin.read()), indent=1))


if __name__ == "__main__":
    main(sys.argv[1:])
