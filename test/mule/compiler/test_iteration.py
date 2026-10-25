"""Sequence protocols and loops execute as native code, including error edges."""

import pytest

from test.mule.compiler.test_modules import execute


def test_range_membership_matches_python_for_bounds_steps_and_numeric_values(tmp_path):
    cases = [
        (0, 5, 1, 0),
        (0, 5, 1, 4),
        (0, 5, 1, 5),
        (2, 10, 3, 8),
        (2, 10, 3, 7),
        (8, -2, -3, 2),
        (8, -2, -3, -1),
        (8, -2, -3, -2),
        (5, 0, 1, 3),
        (0, 5, -1, 3),
        (3, 3, 1, 3),
        (0, 3, 1, True),
        (0, 3, 1, False),
        (0, 3, 1, 2.0),
        (0, 3, 1, 2.5),
        (0, 3, 1, "2"),
        (0, 3, 1, None),
        (0, 10**30, 3, 10**30 - 1),
        (0, 10**30, 3, 10**30 - 2),
        (10**30, 0, -3, 1),
    ]
    result = execute(
        tmp_path,
        {},
        f"for start, stop, step, value in {cases!r}:\n"
        " values = range(start, stop, step)\n"
        " print(value in values, value not in values)\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "".join(
        f"{value in range(start, stop, step)} {value not in range(start, stop, step)}\n"
        for start, stop, step, value in cases
    )


def test_range_membership_uses_equality_without_converting_other_objects(tmp_path):
    result = execute(
        tmp_path,
        {},
        "class Equal:\n"
        " def __init__(self):\n"
        "  self.seen = []\n"
        " def __eq__(self, other):\n"
        "  self.seen.append(other)\n"
        "  return other == 3\n"
        "needle = Equal()\n"
        "print(needle in range(1, 6), needle.seen)\n"
        "class IndexOnly:\n"
        " def __index__(self):\n"
        "  raise ValueError('must not convert')\n"
        "print(IndexOnly() in range(3))\n"
        "class EqualInt(int):\n"
        " def __eq__(self, other):\n"
        "  return other == 2\n"
        "print(EqualInt(99) in range(3))\n"
        "class Broken:\n"
        " def __eq__(self, other):\n"
        "  raise ValueError('comparison failed')\n"
        "try:\n"
        " Broken() in range(3)\n"
        "except ValueError:\n"
        " print('comparison error')\n",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "True [1, 2, 3]\nFalse\nTrue\ncomparison error\n"


def test_indexing_assignment_and_augmented_assignment_order(tmp_path):
    result = execute(
        tmp_path,
        {},
        """
values = [10, 20, 30]
print(values[0], values[-1], (4, 5)[-2], 'aé🐍'[-1])
def receiver():
    print('receiver')
    return values
def index():
    print('index')
    return 1
def value():
    print('value')
    return 7
receiver()[index()] = value()
receiver()[index()] += value()
values.append(40)
print(values)
""",
    )
    assert result.returncode == 0, result.stderr
    assert (
        result.stdout
        == "10 30 4 🐍\nvalue\nreceiver\nindex\nreceiver\nindex\nvalue\n[10, 14, 30, 40]\n"
    )


def test_for_loops_break_continue_else_and_unpacking(tmp_path):
    result = execute(
        tmp_path,
        {},
        """
total = 0
for a, b in [(1, 2), (3, 4), (5, 6)]:
    if a == 3:
        continue
    total += a + b
else:
    print('exhausted', total, a, b)
for item in (9, 8, 7):
    print(item)
    break
else:
    print('wrong')
for item in []:
    print('wrong')
else:
    print('empty')
x, (y, z) = [1, (2, 3)]
print(x, y, z)
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "exhausted 14 5 6\n9\nempty\n1 2 3\n"


def test_unpack_captures_elements_before_target_side_effects(tmp_path):
    result = execute(
        tmp_path,
        {},
        """
class Target:
    def __setitem__(self, key, value):
        values[1] = 99
        print('target', value)
target = Target()
values = [10, 20]
target[0], second = values
print(second, values[1])
values = [30, 40]
pair = (values, None)
target[0], second = pair
print(second, values[1])
a, (b, c) = (1, (2, 3))
print(a, b, c)
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == ("target 10\n20 99\ntarget [30, 99]\nNone 99\n1 2 3\n")


def test_unpack_cardinality_and_custom_iterator_effects(tmp_path):
    result = execute(
        tmp_path,
        {},
        """
for values in [(), (1,), (1, 2, 3)]:
    a = 10
    b = 20
    try:
        a, b = values
    except ValueError:
        print('mismatch', a, b)
class Iterator:
    def __init__(self, count):
        self.count = count
        self.index = 0
    def __iter__(self):
        print('iter')
        return self
    def __next__(self):
        print('next', self.index)
        if self.index == self.count:
            raise StopIteration
        self.index += 1
        return self.index
a, b = Iterator(2)
print(a, b)
try:
    a, b = Iterator(3)
except ValueError:
    print('excess', a, b)
() = ()
try:
    () = (1,)
except ValueError:
    print('nonempty')
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == (
        "mismatch 10 20\nmismatch 10 20\nmismatch 10 20\n"
        "iter\nnext 0\nnext 1\nnext 2\n1 2\n"
        "iter\nnext 0\nnext 1\nnext 2\nexcess 1 2\nnonempty\n"
    )


def test_unpack_keeps_tuple_alive_and_preserves_partial_nested_assignment(tmp_path):
    result = execute(
        tmp_path,
        {},
        """
class Target:
    def __setitem__(self, key, value):
        global pair
        pair = None
        print('stored', value)
target = Target()
pair = ([1], [2])
target[0], second = pair
print(second)
a = 10
b = 20
c = 30
try:
    a, (b, c) = (1, (2,))
except ValueError:
    print(a, b, c)
try:
    target[0], (b, c) = ([3], ())
except ValueError:
    print('alive', b, c)
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "stored [1]\n[2]\n1 20 30\nstored [3]\nalive 20 30\n"


def test_unpack_errors_preserve_targets_and_release_captures(tmp_path):
    result = execute(
        tmp_path,
        {},
        """
class Target:
    def __setitem__(self, key, value):
        print('received', value)
        raise ValueError('setter')
target = Target()
try:
    target[0], second = (['owned'], [2])
except ValueError:
    print('caught setter')
first, second = ([3], [4])
print(first, second)
calls = 0
def source():
    global calls
    calls += 1
    return ([5], [6])
first, second = source()
print(calls, first, second)
def unbound():
    try:
        first, second = missing
    except UnboundLocalError:
        print('unbound')
    missing = ([], [])
unbound()
class Broken:
    def __init__(self, fail):
        self.fail = fail
        self.step = 0
    def __iter__(self):
        return self
    def __next__(self):
        self.step += 1
        if self.step == self.fail:
            raise RuntimeError('iterator')
        return [self.step]
for fail in (2, 3):
    first, second = (['first'], ['second'])
    try:
        first, second = Broken(fail)
    except RuntimeError:
        print(first, second)
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == (
        "received ['owned']\ncaught setter\n[3] [4]\n1 [5] [6]\nunbound\n"
        "['first'] ['second']\n['first'] ['second']\n"
    )


def test_custom_iteration_and_getitem_fallback(tmp_path):
    result = execute(
        tmp_path,
        {},
        """
class Countdown:
    def __init__(self, n):
        self.n = n
    def __iter__(self):
        print('iter')
        return self
    def __next__(self):
        if self.n == 0:
            raise StopIteration
        self.n -= 1
        return self.n
class Indexed:
    def __getitem__(self, n):
        if n == 3:
            raise IndexError('end')
        return n * 2
for value in Countdown(3):
    print(value)
for value in Indexed():
    print(value)
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "iter\n2\n1\n0\n0\n2\n4\n"


def test_range_enumerate_and_iter_next(tmp_path):
    result = execute(
        tmp_path,
        {},
        """
for i, value in enumerate(range(7, -1, -3), 10):
    print(i, value)
for i in range(3):
    print(i)
iterator = iter(['a', 'b'])
print(next(iterator), next(iterator), next(iterator, 'done'))
print(list(range(2, 5)), tuple('aé'))
""",
    )
    assert result.returncode == 0, result.stderr
    assert (
        result.stdout == "10 7\n11 4\n12 1\n0\n1\n2\na b done\n[2, 3, 4] ('a', 'é')\n"
    )


@pytest.mark.parametrize(
    "statement,kind",
    [
        ("print([1][2])", "IndexError"),
        ("print((1,)[-2])", "IndexError"),
        ('print([1]["x"])', "TypeError"),
        ("(1,)[0] = 3", "TypeError"),
        ("for x in 42:\n    pass", "TypeError"),
        ("for x in range(1, 3, 0):\n    pass", "ValueError"),
        ("a, b = [1]", "ValueError"),
        ("a, b = [1, 2, 3]", "ValueError"),
    ],
)
def test_sequence_errors_are_catchable(tmp_path, statement, kind):
    source = "try:\n" + "".join("    " + line + "\n" for line in statement.splitlines())
    source += f'except {kind}:\n    print("caught")\nprint("alive")\n'
    result = execute(tmp_path, {}, source)
    assert result.returncode == 0, result.stderr
    assert result.stdout == "caught\nalive\n"


def test_loop_does_not_swallow_body_or_assignment_stopiteration(tmp_path):
    result = execute(
        tmp_path,
        {},
        """
try:
    for x in [1, 2]:
        raise StopIteration('body')
    else:
        print('wrong')
except StopIteration:
    print('body caught')
class Target:
    def __setitem__(self, key, value):
        raise StopIteration('target')
target = Target()
try:
    for target[0] in [1]:
        print('wrong')
except StopIteration:
    print('target caught')
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "body caught\ntarget caught\n"


def test_builtin_shadowing_and_iterator_mutation(tmp_path):
    result = execute(
        tmp_path,
        {},
        """
StopIteration = 'shadowed'
values = [1, 2]
iterator = iter(values)
print(next(iterator))
values[1] = 3
values.append(4)
for item in iterator:
    print(item)
def range(n):
    print('custom range', n)
    return [8, 9]
for item in range(2):
    print(item)
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "1\n3\n4\ncustom range 2\n8\n9\n"


def test_variadic_entries_and_index_protocol(tmp_path):
    result = execute(
        tmp_path,
        {},
        """
def collect(first, *rest):
    return (first, rest)
print(collect(1), collect(1, 2, 3))
class Collector:
    def __init__(self, *values):
        self.values = values
    def collect(self, *values):
        return values
print(Collector(1, 2).values, Collector().collect(3, 4))
class Index:
    def __index__(self):
        return -1
print([8, 9][Index()], [8, 9][True])
print([1, [2]] == [1, [2]], (1, 2) == (1, 2), [] == ())
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "(1, ()) (1, (2, 3))\n(1, 2) (3, 4)\n9 9\nTrue True False\n"


def test_range_boundaries_bigints_nested_loops_and_else(tmp_path):
    result = execute(
        tmp_path,
        {},
        """
for i in range(2**62 - 2, 2**62 - 1, 2**62 - 1):
    print(i)
else:
    print('high done')
for i in range(-2**62 + 1, -2**62, -2**62):
    print(i)
else:
    print('low done')
for i in range(2**70, 2**70 + 2):
    print(i)
for i in range(3, 1):
    print('wrong')
else:
    print('empty')
for i in range(3):
    for j in range(2):
        if i == 1:
            break
        print(i, j)
""",
    )
    assert result.returncode == 0, result.stderr
    assert (
        result.stdout
        == "4611686018427387902\nhigh done\n-4611686018427387903\nlow done\n1180591620717411303424\n1180591620717411303425\nempty\n0 0\n0 1\n2 0\n2 1\n"
    )


def test_range_callee_and_argument_evaluation_order(tmp_path):
    result = execute(
        tmp_path,
        {},
        """
saved = range
def replacement(n):
    return ['replacement']
def argument():
    global range
    range = replacement
    print('argument')
    return 2
for i in range(argument()):
    print(i)
for i in range(1):
    print(i)
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "argument\n0\n1\nreplacement\n"


def test_variadic_numeric_annotations_apply_to_elements(tmp_path):
    result = execute(
        tmp_path,
        {},
        """
import mule_types
@mule_types.optimize(safety=3)
def collect(*values: int):
    return values
print(collect(), collect(1, 2))
try:
    collect(1, 'wrong')
except TypeError:
    print('bad element')
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "() (1, 2)\nbad element\n"
