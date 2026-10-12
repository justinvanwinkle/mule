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
def keyword_without_defaults(base):
    def add(amount, *, scale):
        return (base + amount) * scale
    return add
print(keyword_without_defaults(1)(amount=2, scale=3))
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == (
        "11 16 16\n2\n120\n5\n1\nunbound True\n1\n(2, 2, 2)\ndeleted\n11 12 26\n9\n"
    )


def test_closure_exposes_a_cell_for_each_captured_variable(tmp_path):
    # A nonlocal declaration alone makes the variable free, as types.py's
    # CellType idiom relies on; reassigned captures expose their shared cell.
    result = execute(
        tmp_path,
        {},
        """
def factory():
    a = 1
    def unused():
        nonlocal a
    return unused.__closure__
cells = factory()
print(len(cells), cells[0].cell_contents)
def shared():
    count = 0
    def bump():
        nonlocal count
        count += 1
        return count
    return bump
bump = shared()
cell = bump.__closure__[0]
bump()
print(cell.cell_contents)
cell.cell_contents = 10
print(bump())
def plain():
    return 1
print(plain.__closure__)
CellType = type(cells[0])
print(CellType.__name__, CellType(5).cell_contents)
try:
    CellType().cell_contents
except ValueError as error:
    print(error)
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "1 1\n1\n11\nNone\ncell 5\nCell is empty\n"


def test_lambdas_bind_defaults_early_and_keep_their_own_scope(tmp_path):
    result = execute(
        tmp_path,
        {},
        """
x = 1
early = lambda a=x: a
x = 2
print(early(), (lambda: x)())
print((lambda a, /, b, *rest, c=5: (a, b, rest, c))(1, 2, 3, c=4))
curry = lambda a: lambda b: lambda c: a + b + c
print(curry(1)(2)(3))
def counter():
    count = 0
    peek = lambda: count
    def bump():
        nonlocal count
        count += 1
    bump()
    bump()
    return peek()
print(counter())
z = 'global'
def walrus_stays_inside():
    f = lambda: (z := 5)
    return f(), z
print(walrus_stays_inside())
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "1 2\n(1, 2, (3,), 4)\n6\n2\n(5, 'global')\n"


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
    assert inner.environment is not None
    assert outer.environment is not None
    assert {var.name for var in inner.environment.closure} == {"a", "b", "c"}
    assert outer.environment.closure == []
