import sys

import idna

assert sys.prefix == "/opt/venv"
assert idna.encode("kiln").decode() == "kiln"
print("kiln smoke")
