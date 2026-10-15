"""Python signatures bind in compiled external entries before entering bodies."""

import pytest

from test.mule.compiler.test_annotations import contradiction_warnings
from test.mule.compiler.test_modules import execute


def test_defaults_capture_once_and_share_mutable_values(tmp_path):
    result = execute(
        tmp_path,
        {},
        """
def capture():
    print('capture')
    return []
def record(value, values=capture(), *, suffix='end'):
    values.append(value)
    print(values, suffix)
record(1)
record(value=2, suffix='last')
record(3, [], suffix='fresh')
record(4)
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "capture\n[1] end\n[1, 2] last\n[3] fresh\n[1, 2, 4] end\n"


def test_parameter_kinds_and_rest(tmp_path):
    result = execute(
        tmp_path,
        {},
        """
def show(a, /, b=2, *rest, c, d=None):
    print(a, b, rest, c, d)
show(1, c=3)
show(4, 5, 6, 7, d=8, c=9)
show(10, b=11, c=12)
def only(*, required, optional=13):
    print(required, optional)
only(optional=14, required=15)
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "1 2 () 3 None\n4 5 (6, 7) 9 8\n10 11 () 12 None\n15 14\n"


def test_internal_calls_transfer_heap_values_defaults_and_aliases(tmp_path):
    result = execute(
        tmp_path,
        {},
        """
def identity(value):
    return value
print(identity(['owned']))
print(identity(identity) is identity)
def pair(first, second=['default']):
    return first, second
first, second = pair(second=['second'], first=['first'])
print(first, second, pair(['new'])[1])
def capture(seed):
    def get(value=seed):
        return value
    return get
get = capture(['captured'])
print(get(), get(value=['replacement']))
class Receiver:
    def get(self, value=['method']):
        return value
print(Receiver().get(), Receiver().get(value=['supplied']))
def failing(value):
    raise ValueError('body')
try:
    failing(['temporary'])
except ValueError:
    print('caught body')
def recursive(value):
    return recursive(value)
try:
    recursive(['deep'])
except RecursionError:
    print('caught frame')
print(identity(['alive']))
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == (
        "['owned']\nTrue\n['first'] ['second'] ['default']\n"
        "['captured'] ['replacement']\n['method'] ['supplied']\n"
        "caught body\ncaught frame\n['alive']\n"
    )


def test_keyword_evaluation_order_and_dynamic_callee(tmp_path):
    result = execute(
        tmp_path,
        {},
        """
def value(label, n):
    print(label)
    return n
def first(a=1, b=2):
    print('first', a, b)
def second(a=3, b=4):
    print('second', a, b)
def replace():
    global target
    target = second
    print('replace')
    return 8
target = first
target(b=value('b', 9), a=replace())
target()
saved = first
first = second
saved(b=10)
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "b\nreplace\nfirst 8 9\nsecond 3 4\nfirst 1 10\n"


@pytest.mark.parametrize(
    "call",
    [
        "f()",
        "f(1, 2, 3, c=4)",
        "f(1, a=2, c=3)",
        "f(1, b=2, unknown=3, c=4)",
        "f(1)",
        "f(1, 2, b=3, c=4)",
    ],
)
def test_binding_failures_are_catchable_and_do_not_enter_body(tmp_path, call):
    result = execute(
        tmp_path,
        {},
        f"""
def f(a, /, b=2, *, c):
    print('wrong')
try:
    {call}
except TypeError:
    print('caught')
print('alive')
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "caught\nalive\n"


def test_methods_constructors_and_callable_instances(tmp_path):
    result = execute(
        tmp_path,
        {},
        """
class Counter:
    def __init__(self, start=1, *, step=2):
        self.value = start
        self.step = step
    def add(self, amount=3, *, scale=1):
        self.value += amount * scale
        return self.value
    def __call__(self, amount=4):
        return self.add(amount=amount)
a = Counter(step=5, start=10)
print(a.add(scale=2), a(amount=7))
b = Counter()
method = b.add
print(method(), method(scale=3, amount=2))
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "16 23\n4 10\n"


def test_repeated_definitions_keep_distinct_default_values(tmp_path):
    result = execute(
        tmp_path,
        {},
        """
functions = []
for value in (1, 2, 3):
    def f(x=value):
        return x
    functions.append(f)
for function in functions:
    print(function(), function(x=9))
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "1 9\n2 9\n3 9\n"


def test_native_class_defaults_keyword_binding_and_inheritance(tmp_path):
    result = execute(
        tmp_path,
        {},
        """
import mule
@mule.optimize(speed=3, safety=3)
class Counter:
    value: mule.int64
    def __init__(self, value: mule.int64=10, *, offset: mule.int64=2):
        self.value = value + offset
    def add(self, amount: mule.int64=3) -> mule.int64:
        self.value += amount
        return self.value
print(Counter().add(), Counter(offset=5, value=20).add(amount=7))
class Base:
    def __init__(self, value=4):
        self.value = value
    def add(self, *, amount=2):
        return self.value + amount
class Child(Base):
    pass
print(Child(value=8).add(amount=3))
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "15 32\n11\n"


def test_default_failure_preserves_existing_binding_and_order(tmp_path):
    result = execute(
        tmp_path,
        {},
        """
def f():
    return 'original'
def value(name):
    print(name)
    return None
def fail():
    print('fail')
    raise ValueError('default')
try:
    def f(a=value('first'), b=fail(), *, c=value('wrong')):
        print('wrong')
except ValueError:
    print('caught')
print(f())
def g(a=value('a'), b=value('b'), *, c=value('c')):
    print(a, b, c)
g()
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "first\nfail\ncaught\noriginal\na\nb\nc\nNone None None\n"


def test_body_errors_and_all_argument_side_effects_survive_binding_errors(tmp_path):
    result = execute(
        tmp_path,
        {},
        """
def value(label):
    print(label)
    return 1
def f(a):
    raise ValueError('body')
try:
    f(value('position'), unknown=value('keyword'))
except TypeError:
    print('binding')
try:
    f(a=1)
except ValueError:
    print('body')
print('alive')
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "position\nkeyword\nbinding\nbody\nalive\n"


def test_empty_signatures_and_positional_only_rest_reject_keywords(tmp_path):
    result = execute(
        tmp_path,
        {},
        """
def empty():
    print('wrong')
def rest(a, /, *values):
    print(a, values)
rest(1, 2, 3)
try:
    empty(x=1)
except TypeError:
    print('empty')
try:
    rest(1, a=2)
except TypeError:
    print('rest')
try:
    print(unsupported_keyword=' ')
except TypeError:
    print('unsupported builtin keyword')
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "1 (2, 3)\nempty\nrest\nunsupported builtin keyword\n"


def test_keyword_values_obey_annotation_safety_after_binding(tmp_path):
    result = execute(
        tmp_path,
        {},
        """
import mule
@mule.optimize(safety=3)
def require(value: mule.int64=4, *, other: mule.int64=5):
    return value + other
def unknown():
    return 'bad'
print(require(), require(other=7, value=6))
try:
    require(value=unknown())
except TypeError:
    print('checked')
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "9 13\nchecked\n"


def test_keyword_binding_across_imported_modules(tmp_path):
    result = execute(
        tmp_path,
        {
            "helpers.py": """
def combine(left=2, *, right=3):
    return left * 10 + right
"""
        },
        """
from helpers import combine
print(combine(), combine(right=8, left=7))
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "23 78\n"


@pytest.mark.parametrize(
    "source",
    [
        "import mule\ndef f(x: mule.int64='bad'):\n    return x\n",
        "import mule\ndef f(x: mule.int64):\n    return x\nf(x='bad')\n",
    ],
)
def test_proven_annotation_contradictions_in_defaults_and_keywords_warn(caplog, source):
    assert contradiction_warnings(caplog, source, "contracts.py")


def test_complete_keyword_calls_and_captured_defaults_have_direct_entries():
    from mule.compiler import emit_llvm

    llvm = emit_llvm(
        "def combine(a=1, *, b=2):\n    return a + b\nprint(combine(b=4, a=3), combine())\n"
    )
    # These are calls in the user module body, before generated binding entries.
    module_body = llvm.split('define internal ptr @"mule_module.body"(')[1]
    module_body = module_body.split("\n}\n", 1)[0]
    assert "call ptr @mule_function_default(" in module_body
    assert 'call ptr @"mule_fn_combine.body"(' in module_body
    assert "call ptr @mule_call_keywords(" in module_body  # Identity-guard fallback.


def test_native_call_selection_preserves_keyword_only_restrictions(tmp_path):
    result = execute(
        tmp_path,
        {},
        """
import mule
@mule.optimize(speed=3, safety=3)
class Counter:
    value: mule.int64
    def __init__(self, value: mule.int64):
        self.value = value
    def add(self, *, amount: mule.int64) -> mule.int64:
        return self.value + amount
def wrong(value: mule.int64) -> mule.int64:
    instance = Counter(value)
    return instance.add(2)
try:
    wrong(1)
except TypeError:
    print('caught')
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "caught\n"


def test_repeated_explicit_keyword_is_a_compile_error():
    from mule.compiler.main import compile_source

    with pytest.raises(SyntaxError, match="keyword argument repeated: value"):
        compile_source("def f(value):\n    pass\nf(value=1, value=2)\n")


def test_generated_external_binder_calls_body_without_argument_tuple():
    from mule.compiler import emit_llvm

    llvm = emit_llvm("def f(a, b=2):\n    return a + b\nf(a=1)\n")
    entries = llvm.split("\ndefine ")[1:]
    binder = next(
        entry for entry in entries if entry.startswith('ptr @"mule_fn_f.fn"(')
    )
    assert 'call ptr @"mule_fn_f.body"(' in binder
    assert "call ptr @mule_sequence_tuple(" not in binder


def test_implicit_receiver_cannot_bind_to_keyword_only_parameter(tmp_path):
    result = execute(
        tmp_path,
        {},
        """
class Odd:
    def method(*, receiver):
        return 7
instance = Odd()
try:
    instance.method()
except TypeError:
    print('caught')
print(Odd.method(receiver=instance))
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "caught\n7\n"
