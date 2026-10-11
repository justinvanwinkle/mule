"""The args tuple and str()/repr() of exceptions with zero, one, and many arguments."""

cases = [
    ValueError(),
    ValueError("one"),
    ValueError("a", "b"),
    ValueError(1, 2, 3),
    ValueError(42),
    ValueError(None),
    ValueError(("t", 1)),
    ValueError(""),
    ValueError([1, "x"]),
    Exception("nested", ("x",)),
]
for e in cases:
    print("args", e.args, "| str", repr(str(e)), "| repr", repr(e))

# KeyError str uses repr of its single argument.
for k in ["k", 5, ("a", 1), "", None]:
    e = KeyError(k)
    print("KeyError", repr(str(e)), repr(e))
print("KeyError no args", repr(str(KeyError())))
print("KeyError two args", repr(str(KeyError("a", "b"))))

# args can be reassigned; str follows it.
e = RuntimeError("orig")
e.args = ("changed", 2)
print("reassigned args", e.args, str(e))
e.args = ["listy"]
print("list coerced to tuple", e.args, type(e.args).__name__, str(e))
e.args = ()
print("emptied", repr(str(e)))

# OSError special argument handling.
o = OSError(2, "No such file")
print("OSError errno", o.errno, "strerror", o.strerror, "args", o.args)
print("OSError str", str(o))
o1 = OSError("just message")
print("OSError one arg", o1.errno, o1.args, str(o1))
o3 = OSError(2, "No such file", "name.txt")
print("OSError filename", o3.filename, str(o3), type(o3).__name__)

# StopIteration value is args[0].
s = StopIteration("v", "w")
print("StopIteration value", s.value, s.args)
print("StopIteration empty value", StopIteration().value)

# SystemExit code
print("SystemExit codes", SystemExit().code, SystemExit(3).code, SystemExit("m").code,
      SystemExit(1, 2).code)

# Format specs on exceptions go through str.
e = ValueError("fmt")
print(f"f-string {e} / {e!r} / {e!s:>6}|")
print("%s and %r" % (e, e))

# Equality is identity-based.
a, b = ValueError("x"), ValueError("x")
print("equal instances", a == b, a == a)

# UnicodeError attributes
u = UnicodeDecodeError("utf-8", b"ab\xff", 2, 3, "invalid start byte")
print("unicode", u.encoding, u.object, u.start, u.end, u.reason)
print("unicode str", str(u))
