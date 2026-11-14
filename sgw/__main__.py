"""python3 -m sgw "some text"   or   python3 -m sgw path/to/file.txt"""
import json, os, sys

from . import core

arg = " ".join(sys.argv[1:])
print(json.dumps(core.read(open(arg, encoding="utf-8", errors="ignore").read() if os.path.isfile(arg) else arg), indent=1))
