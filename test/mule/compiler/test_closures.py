"""Nested functions capture enclosing variables with Python's cell semantics."""

from mule.compiler.main import compile_toplevel
from test.mule.compiler.test_modules import execute


def test_nested_functions_share_cells_and_copy_constant_captures(tmp_path):
    result = execute(
        tmp_path,
        {},
        """
def counter(start):
    count = start
    def bump(step):
        nonlocal count
        count = count + step
        return count
    def read():
        return count
    return bump, read
bump, read = counter(10)
print(bump(1), bump(5), read())
def late():
    x = 1
    def get():
        return x
    x = 2
    return get()
print(late())
def recursive_inner(n):
    def fact(k):
        if k <= 1:
            return 1
        return k * fact(k - 1)
    return fact(n)
print(recursive_inner(5))
def captured_parameter(a, b):
    def add():
        return a + b
    return add
print(captured_parameter(2, 3)())
def levels():
    x = 1
    def middle():
        def inner():
            return x
        return inner()
    return middle()
print(levels())
def unbound():
    def get():
        return later
    try:
        get()
    except NameError as error:
        print('unbound', error.args[0] == 'free variable referenced before assignment in enclosing scope')
    later = 1
    return get()
print(unbound())
def loop_capture():
    fs = []
    for i in range(3):
        def f():
            return i
        fs.append(f)
    return fs[0](), fs[1](), fs[2]()
print(loop_capture())
def deleted():
    value = 1
    def get():
        return value
    del value
    try:
        return get()
    except NameError:
        return 'deleted'
print(deleted())
def keyword_closure(base):
    def add(amount=1, *, scale=1):
        return (base + amount) * scale
    return add
add = keyword_closure(10)
print(add(), add(2), add(amount=3, scale=2))
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == (
        "11 16 16\n2\n120\n5\n1\nunbound True\n1\n(2, 2, 2)\ndeleted\n11 12 26\n"
    )


def test_environment_analysis_marks_assigned_captures_indirect():
    compilation = compile_toplevel(
        "def outer(a, b):\n"
        " c = 1\n"
        " def inner():\n"
        "  nonlocal c\n"
        "  c = a\n"
        "  return b + c\n"
        " return inner\n"
    )
    outer, inner = compilation.lambdas[1], compilation.lambdas[2]
    variables = {var.name: var for var in outer.locals}
    assert variables["c"].indirect, "assigned from the nested function"
    assert not variables["a"].indirect, "read-only capture is copied"
    assert not variables["b"].indirect
    assert not variables["inner"].indirect, "never captured"
    assert {var.name for var in inner.environment.closure} == {"a", "b", "c"}
    assert outer.environment.closure == []
