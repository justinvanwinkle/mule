"""Program that calls sys.exit with a string message: exit code 1, message on stderr."""
import sys

print("before exit")
try:
    sys.exit("fatal: configuration missing")
finally:
    print("finally before exit")
print("unreachable")
