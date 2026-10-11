"""ExceptionGroup/BaseExceptionGroup construction, .exceptions, split, subgroup, derive, nested groups."""


def show(g, indent=""):
    if g is None:
        return "None"
    if isinstance(g, BaseExceptionGroup):
        inner = ", ".join(show(x) for x in g.exceptions)
        return f"{type(g).__name__}({g.message!r}: [{inner}])"
    return f"{type(g).__name__}({g.args[0] if g.args else ''})"


eg = ExceptionGroup("msg", [ValueError("v1"), TypeError("t1"), ValueError("v2")])
print("group:", show(eg))
print("message:", eg.message, "| args[0]:", eg.args[0], "| len:", len(eg.args[1]))
print("exceptions type:", type(eg.exceptions).__name__, len(eg.exceptions))
print("str:", str(eg))
print("is Exception:", isinstance(eg, Exception), isinstance(eg, BaseExceptionGroup))

# BaseExceptionGroup picks ExceptionGroup when all members are Exceptions.
auto = BaseExceptionGroup("auto", [ValueError("x")])
print("BaseExceptionGroup of Exceptions ->", type(auto).__name__)
base = BaseExceptionGroup("base", [KeyboardInterrupt(), ValueError("x")])
print("BaseExceptionGroup with BaseException ->", type(base).__name__,
      isinstance(base, Exception))

for label, build in [
    ("ExceptionGroup with KeyboardInterrupt", lambda: ExceptionGroup("m", [KeyboardInterrupt()])),
    ("empty list", lambda: ExceptionGroup("m", [])),
    ("non-exception member", lambda: ExceptionGroup("m", [ValueError(), 1])),
    ("non-str message", lambda: ExceptionGroup(5, [ValueError()])),
    ("non-sequence", lambda: ExceptionGroup("m", ValueError())),
]:
    try:
        build()
        print(label, "built")
    except Exception as e:
        print(label, "->", type(e).__name__)

nested = ExceptionGroup("outer", [
    KeyError("k"),
    ExceptionGroup("inner", [ValueError("iv"), TypeError("it")]),
    ExceptionGroup("other", [OSError("io")]),
])
print("nested:", show(nested))

match, rest = nested.split(ValueError)
print("split ValueError match:", show(match))
print("split ValueError rest:", show(rest))

match, rest = nested.split((KeyError, OSError))
print("split tuple match:", show(match))
print("split tuple rest:", show(rest))

match, rest = nested.split(lambda e: isinstance(e, Exception) and not isinstance(e, BaseExceptionGroup)
                           and str(e).startswith("i"))
print("split predicate match:", show(match))
print("split predicate rest:", show(rest))

match, rest = nested.split(Exception)
print("split everything: same object", match is nested, "rest", rest)
match, rest = nested.split(ZeroDivisionError)
print("split nothing:", match, "rest is nested", rest is nested)

print("subgroup TypeError:", show(nested.subgroup(TypeError)))
print("subgroup none:", nested.subgroup(ArithmeticError))
print("subgroup whole:", nested.subgroup(Exception) is nested)
# Predicates also receive the groups themselves; matching a group keeps it whole.
print("subgroup group predicate:",
      show(nested.subgroup(lambda e: isinstance(e, ExceptionGroup) and e.message == "inner")))

# Metadata copied by split: __cause__, __context__, __notes__ (but not __traceback__ semantics here)
nested.add_note("group note")
nested.__cause__ = RuntimeError("cause")
m, _ = nested.split(KeyError)
print("split copies notes/cause:", m.__notes__, repr(m.__cause__), m is not nested)


# derive and subclassing
class MyGroup(ExceptionGroup):
    def __new__(cls, message, excs, tag):
        self = super().__new__(cls, message, excs)
        self.tag = tag
        return self

    def __init__(self, message, excs, tag):
        super().__init__(message, excs)

    def derive(self, excs):
        return MyGroup(self.message, excs, self.tag + "*")


mg = MyGroup("mine", [ValueError("a"), KeyError("b")], "T")
m, r = mg.split(ValueError)
print("derived:", type(m).__name__, m.tag, show(m), "|", type(r).__name__, r.tag)


class PlainSub(ExceptionGroup):
    pass


ps = PlainSub("plain", [ValueError(), KeyError()])
m, r = ps.split(ValueError)
print("default derive returns:", type(m).__name__, type(r).__name__)
print("derive direct:", show(eg.derive([OSError("d")])))

# Catching a group with ordinary except.
try:
    raise ExceptionGroup("plain catch", [ValueError()])
except ExceptionGroup as g:
    print("plain except caught group:", g.message, len(g.exceptions))

try:
    raise ExceptionGroup("not matched", [ValueError()])
except ValueError:
    print("wrong")
except Exception as g:
    print("ordinary except ValueError does not match group:", type(g).__name__)
