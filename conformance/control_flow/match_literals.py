"""match literal patterns: numbers, strings, bytes, None/True/False, negative and complex."""


def describe(subject):
    match subject:
        case None:
            return "None"
        case True:
            return "True"
        case False:
            return "False"
        case 0:
            return "zero"
        case -1:
            return "minus one"
        case 1.5:
            return "one and a half"
        case -2.5:
            return "minus two and a half"
        case 3j:
            return "imag 3j"
        case 1 + 2j:
            return "complex 1+2j"
        case -1 - 1j:
            return "complex -1-1j"
        case "hello":
            return "str hello"
        case "con" "cat":
            return "implicitly concatenated"
        case b"bytes":
            return "bytes literal"
        case "":
            return "empty string"
        case _:
            return "other " + type(subject).__name__


subjects = [None, True, False, 0, 0.0, -0.0, -1, -1.0, 1.5, -2.5, 3j, 1 + 2j,
            complex(-1, -1), "hello", "concat", b"bytes", "", 1, 2, "x",
            bytearray(b"bytes"), [], 1.0]
for s in subjects:
    print("describe", repr(s), "->", describe(s))


# True/False/None match by identity; 1 does not match True and vice versa
def identity_check(subject):
    match subject:
        case True:
            return "is True"
        case 1:
            return "equals 1"
        case _:
            return "other"


print("1 vs True", identity_check(1), identity_check(True), identity_check(1.0))


def none_check(subject):
    match subject:
        case None:
            return "None"
        case _:
            return "not None"


class EqualsEverything:
    def __eq__(self, other):
        return True

    def __repr__(self):
        return "EqualsEverything()"


# literal patterns use ==, so a custom __eq__ affects numeric/string literals but not None
print("eq everything vs None", none_check(EqualsEverything()))


def eq_literal(subject):
    match subject:
        case 42:
            return "42 matched via =="
        case _:
            return "no"


print("eq everything vs 42", eq_literal(EqualsEverything()))


# int subclasses and float/int equality
class MyInt(int):
    pass


print("MyInt(0)", describe(MyInt(0)), "MyInt(1)", identity_check(MyInt(1)))

# NaN never matches a literal pattern
print("nan", describe(float("nan")))


# raw strings, f-string free literals, escapes
def strings(s):
    match s:
        case r"\d":
            return "raw backslash d"
        case "\t":
            return "tab"
        case "é":
            return "e-acute"
        case _:
            return "?"


print("strings", [strings(x) for x in ("\\d", "\t", "é", "d")])


# OR of literals and negative zero
def small(n):
    match n:
        case 0 | 1 | 2:
            return "small"
        case -1 | -2:
            return "small negative"
        case _:
            return "big"


print("or literals", [small(n) for n in (0, 2, -0.0, -2, 5, 2.0)])
