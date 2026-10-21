import importlib.util
import shutil
import sys

import idna

assert sys.prefix == "/opt/venv"
assert idna.encode("kiln").decode() == "kiln"
assert importlib.util.find_spec("pytest") is None
assert shutil.which("poetry") is None
assert shutil.which("pdm") is None
print("kiln smoke")
