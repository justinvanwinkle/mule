"""globals(), locals(), and vars() reads and writes."""

existing = "existing global"
print("globals read", globals()["existing"])
print("globals is module dict", globals() is vars())

globals()["created_by_dict"] = "created"
print("write creates name", created_by_dict)

globals()["existing"] = "rewritten"
print("write rewrites name", existing)

globals().update({"g1": 1, "g2": 2})
print("update creates names", g1 + g2)

del globals()["g1"]
try:
    g1
except NameError as e:
    print("deleted via dict", type(e).__name__)

print("setdefault", globals().setdefault("g3", "default"), g3)
print("module-level locals is globals", locals() is globals())


def function_sees_dict_global():
    return created_by_dict


print("function reads dict-created global", function_sees_dict_global())


def function_writes_global():
    globals()["from_function"] = "written in function"


function_writes_global()
print("function wrote global", from_function)


def locals_snapshot(a, b=2):
    c = 3
    snap = locals()
    d = 4
    return sorted(snap), sorted(locals())


print("locals snapshot", locals_snapshot(1))


def locals_write_does_not_affect():
    x = 1
    locals()["x"] = 99
    return x


print("locals write ignored", locals_write_does_not_affect())


def locals_independent_snapshots():
    x = 1
    first = locals()
    x = 2
    second = locals()
    return first["x"], second["x"], first is second


print("PEP 667 snapshots", locals_independent_snapshots())


def locals_includes_free_vars():
    captured = "free"

    def inner():
        own = 1
        captured
        return sorted(locals().items())

    return inner()


print("locals includes free variables", locals_includes_free_vars())


class WithVars:
    cls_attr = 1

    def __init__(self):
        self.a = 1
        self.b = 2


w = WithVars()
print("vars(instance)", vars(w))
vars(w)["c"] = 3
print("vars write adds attribute", w.c)
print("vars(class) has cls_attr", "cls_attr" in vars(WithVars))
try:
    vars(WithVars)["new"] = 1
except TypeError as e:
    print("class vars is read-only proxy", type(e).__name__)


class Slotted:
    __slots__ = ("x",)


try:
    vars(Slotted())
except TypeError as e:
    print("vars on slotted", type(e).__name__)

try:
    vars(5)
except TypeError as e:
    print("vars on int", type(e).__name__)


def vars_no_args():
    p = "p"
    return sorted(vars())


print("vars() no args in function", vars_no_args())


# class body locals() is the class namespace
class NS:
    a = 1
    snapshot = sorted(k for k in locals() if not k.startswith("__"))
    locals()["injected"] = "via locals"


print("class body locals", NS.snapshot, getattr(NS, "injected", "missing"))

# module attributes through sys.modules
import sys

this = sys.modules[__name__]
this.via_module = "set via module object"
print("module setattr", via_module)
print("module getattr", this.existing)

# globals() from an imported module function refers to that module
import json

print("other module globals differ", json.dumps.__globals__ is globals(), "dumps" in json.dumps.__globals__)

# function __globals__ is the defining module dict
print("__globals__ is globals()", function_sees_dict_global.__globals__ is globals())

# names created via globals() inside a loop
for idx in range(3):
    globals()[f"dyn_{idx}"] = idx * 11
print("dynamic names", dyn_0, dyn_1, dyn_2)
print("sorted dyn names", sorted(k for k in globals() if k.startswith("dyn_")))
