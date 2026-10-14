import _sha3
import sys


print(_sha3.sha3_256(b"x" * int(sys.argv[1])).hexdigest())
