"""Command line.

    python3 -m sgw "some text"                    score a text (or a file path)
    python3 -m sgw tone draft.txt --genre essay   compare a draft with human writing of one kind
    python3 -m sgw tone draft.txt --json          the same as numbers
"""
import json, os, re, sys

from . import core, tone


def _text(arg):
    if os.path.isfile(arg):
        return open(arg, encoding="utf-8", errors="ignore").read()
    if re.fullmatch(r"\S+\.(txt|md|text)", arg):
        sys.exit(f"sgw: no such file: {arg}")
    return arg


def main(argv):
    if argv[:1] in (["-h"], ["--help"]):
        print(__doc__.strip())
        return
    if argv and argv[0] == "tone":
        import argparse
        p = argparse.ArgumentParser(prog="python3 -m sgw tone", description="Compare a draft with human writing of one kind.")
        p.add_argument("text", nargs="*", help="a file path or the text itself; standard input if omitted")
        p.add_argument("--genre", default="essay", choices=sorted(tone.reference(None)))
        p.add_argument("--json", action="store_true", help="print the numbers as JSON")
        a = p.parse_args(argv[1:])
        text = _text(" ".join(a.text)) if a.text else sys.stdin.read()
        print(json.dumps(tone.compare(text, a.genre), indent=1) if a.json else tone.report(text, a.genre))
    else:
        print(json.dumps(core.read(_text(" ".join(argv)) if argv else sys.stdin.read()), indent=1))


if __name__ == "__main__":
    main(sys.argv[1:])
