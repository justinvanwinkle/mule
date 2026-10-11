"""input() with empty stdin raises EOFError, which a program can catch and continue past."""

print("before input")
try:
    value = input()
    print("unexpected value", repr(value))
except EOFError as e:
    print("caught", type(e).__name__)
print("after input")
