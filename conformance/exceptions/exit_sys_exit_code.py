"""Program that calls sys.exit(3) inside nested try/finally blocks whose finally clauses print."""
import sys


def inner():
    try:
        print("inner body")
        sys.exit(3)
    finally:
        print("inner finally")


print("start")
try:
    try:
        try:
            inner()
        except Exception:
            print("wrong: SystemExit caught by except Exception")
        finally:
            print("middle finally")
    finally:
        print("outer finally")
finally:
    print("top-level finally")
print("unreachable")
