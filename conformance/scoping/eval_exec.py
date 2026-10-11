"""eval and exec with explicit namespaces: expressions, statements, functions, classes."""

module_value = "module"

print("eval simple", eval("1 + 2 * 3"))
print("eval default globals", eval("module_value.upper()"))
print("eval explicit globals", eval("a + b", {"a": 1, "b": 2}))
print("eval locals over globals", eval("a", {"a": "global"}, {"a": "local"}))

ns = {}
eval("1", ns)
print("eval inserts __builtins__", "__builtins__" in ns)

try:
    eval("module_value", {})
except NameError as e:
    print("explicit globals hide module", type(e).__name__)

restricted = {"__builtins__": {}}
try:
    eval("len([])", restricted)
except NameError as e:
    print("empty builtins", type(e).__name__)
print("custom builtins", eval("len([1])", {"__builtins__": {"len": lambda x: "fake len"}}))

# exec statements and definitions into a namespace
g = {}
exec("x = 10\ny = x * 2\ndef double(v):\n    return v * 2\n", g)
print("exec assignments", g["x"], g["y"], g["double"](21))
print("exec function globals are the dict", g["double"].__globals__ is g)

# function defined by exec sees later updates to its globals dict
exec("def get_z():\n    return z\n", g)
g["z"] = "late z"
print("exec function late global", g["get_z"]())

# exec with separate globals and locals: definitions go to locals
gl, lo = {}, {}
exec("value = 1\ndef f():\n    return 'f'\n", gl, lo)
print("separate locals", sorted(lo), "value" in gl)

# function defined with separate locals cannot see other locals (they are not globals)
gl, lo = {}, {}
exec("helper = 5\ndef uses_helper():\n    return helper\n", gl, lo)
try:
    lo["uses_helper"]()
except NameError as e:
    print("exec locals not visible to functions", type(e).__name__)

# exec creating classes
ns = {}
exec(
    "class Point:\n"
    "    dims = 2\n"
    "    def __init__(self, x, y):\n"
    "        self.x, self.y = x, y\n"
    "    def __repr__(self):\n"
    "        return f'Point({self.x}, {self.y})'\n"
    "p = Point(1, 2)\n",
    ns,
)
print("exec class", ns["p"], ns["Point"].dims, ns["Point"].__module__)
ns2 = {"__name__": "custom_module"}
exec("class K: pass\nmodname = K.__module__", ns2)
print("exec class module from __name__", ns2["modname"])

# exec at module level with default namespaces modifies module globals
exec("created_by_exec = 'hello'")
print("exec default globals", created_by_exec)


# exec inside a function does not create function locals (PEP 667)
def exec_in_function():
    exec("inner_var = 1")
    try:
        return inner_var
    except NameError as e:
        return type(e).__name__


print("exec in function", exec_in_function())


def exec_with_locals_dict():
    ns = {}
    exec("inner_var = 2", globals(), ns)
    return ns["inner_var"]


print("exec into explicit locals", exec_with_locals_dict())


def exec_reads_function_locals():
    local_value = 7
    return eval("local_value * 6")


print("eval sees function locals", exec_reads_function_locals())

# exec returns None; eval of statements is a SyntaxError
print("exec returns", exec("pass"))
try:
    eval("x = 1")
except SyntaxError as e:
    print("eval statement", type(e).__name__)

# compile then exec/eval
code = compile("result = sum(range(n))", "<compiled>", "exec")
space = {"n": 5}
exec(code, space)
print("compiled exec", space["result"])
expr = compile("n ** 2", "<expr>", "eval")
print("compiled eval", eval(expr, {"n": 7}))

# exec of code with global/nonlocal semantics in the namespace dict
ns = {"counter": 0}
exec("def bump():\n    global counter\n    counter += 1\nbump()\nbump()\n", ns)
print("exec global statement", ns["counter"])

# mapping as locals
class LoggingLocals(dict):
    def __getitem__(self, key):
        return "mapped:" + key


print("custom mapping locals", eval("anything", {}, LoggingLocals()))

# exec of comprehension inside class-like namespace
ns = {}
exec("data = [1, 2, 3]\nsquares = [d * d for d in data]", ns)
print("exec comprehension", ns["squares"], "d" in ns)

# exceptions propagate out of exec/eval
try:
    exec("raise KeyError('inner')", {})
except KeyError as e:
    print("exec raised", type(e).__name__)

# eval with lambda closures over exec locals
ns = {}
exec("factor = 3\nscale = lambda v: v * factor", ns)
print("lambda from exec", ns["scale"](4))
