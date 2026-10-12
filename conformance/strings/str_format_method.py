"""str.format and format(): positional, named, indexed, attribute access, nesting, and conversions."""


class Point:
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def __repr__(self):
        return f"Point({self.x!r}, {self.y!r})"

    def __str__(self):
        return f"<{self.x},{self.y}>"

    def __format__(self, spec):
        if spec == "polar":
            return "polar!"
        return format(str(self), spec)


p = Point(3, "é")
print("auto positional", f"{1} + {2} = {3}")
print("explicit positional", "{1} {0} {1}".format("a", "b"))
print("named", "{name} is {age}".format(name="Ann", age=30))
print("mixed", "{0} {name} {1}".format("x", "y", name="N"))
print("index", "{0[1]} {d[key]} {d[0]}".format([10, 20, 30], d={"key": "v", 0: "int0", "0": "str0"}))
try:
    print("negative index", f"{[10, 20]['-1']}")
except TypeError as e:
    print("negative index", type(e).__name__)
print("str key not int", "{d[k1]}".format(d={"k1": 1}))
print("attribute", f"{p.x} {p.y} {p.x}")
print("chained", f"{p.x.real} {[[1, 2]][0][1]} {p.y.__class__.__name__}")
print("conversions", f"{p!s} {p!r} {p!a}")
print("conversions str", "{0!s:>8}|{0!r:>8}|{1!a}".format("hi", "ñ"))
print("custom __format__", f"{p:polar} {p:>10} {p:*^9}")
print("nested spec", "{0:{1}{2}}".format("x", "^", 7), "{:{fill}>{width}}".format(5, fill="0", width=4))
print("nested spec auto", "{:{}.{}f}".format(3.14159, 10, 2))
print("braces", "{{}} {{{0}}} {{literal}}".format("in"))
print("format builtin", format(255, "x"), format("s", "<3") + "|", format(p), format(3.5), format(None))
print("format_map", "{a}-{b}".format_map({"a": 1, "b": 2}))


class Default(dict):
    def __missing__(self, key):
        return f"<{key}?>"


print("format_map missing", "{a} {zz}".format_map(Default(a=1)))
print("unpack kwargs", f"{1}/{2}", "{}-{}".format(*"ab"))
for fmt, args, kwargs in [
    ("{} {}", (1,), {}),
    ("{0} {}", (1, 2), {}),
    ("{missing}", (), {}),
    ("{0[5]}", ([1],), {}),
    ("{0.nope}", (p,), {}),
    ("{", (), {}),
    ("}", (), {}),
    ("{0!x}", (1,), {}),
    ("{:d}", ("s",), {}),
]:
    try:
        print("error case", repr(fmt), fmt.format(*args, **kwargs))
    except Exception as e:
        print("error case", repr(fmt), type(e).__name__)
print("extra args ignored", f"{1}", f"{1}")
print("same arg reused", "{0}{0}{0}".format("ab"))
print("integer-like names", "{00}".format("zero"), "{01}".format("a", "b"))
print("method of str", str.format("{}!", "ok"))
