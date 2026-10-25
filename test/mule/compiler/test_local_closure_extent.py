"""Local closure environments use the enclosing activation when none escape."""

import re

import pytest

from mule.compiler import emit_llvm
from test.mule.compiler.rewrite_support import build_case

LOCAL_COUNTER = """\
def local_counter(start: int, n: int) -> int:
    current = start
    def bump(step: int) -> int:
        nonlocal current
        current += step
        return current
    def read() -> int:
        return current
    total = 0
    for i in range(n):
        total += bump(i) + read()
    return total

value = int(input())
print(local_counter(value, 12))
"""


def _counter_bodies(llvm):
    return "\n".join(
        match.group()
        for match in re.finditer(
            r'^define[^\n]*@"mule_fn_local_counter\.(?:body|spec\.\d+)"\([^\n]*\{\n.*?^}',
            llvm,
            re.MULTILINE | re.DOTALL,
        )
    )


def test_local_mutable_closures_share_frame_slots_without_heap_environments(tmp_path):
    generated = _counter_bodies(emit_llvm(LOCAL_COUNTER))
    assert generated, "The normal compiler must emit the actual enclosing function."
    assert "@mule_function_closure(" not in generated
    assert "@mule_sequence_tuple_owned(" not in generated
    assert "mule.protocol.cell_new" not in generated
    assert "@mule_function(" not in generated
    assert "@mule_function_defaults(" not in generated

    case = build_case(tmp_path, LOCAL_COUNTER, optimize=2)
    optimized = case.function_ir(r"^mule_fn_local_counter\.(?:body|spec\.\d+)$")
    assert "@mule_function_closure(" not in optimized
    assert "@mule_sequence_tuple_owned(" not in optimized
    assert "mule.protocol.cell_new" not in optimized
    assert "@mule_function(" not in optimized
    assert "@mule_function_defaults(" not in optimized
    assert "add i64" in optimized or "llvm.sadd.with.overflow.i64" in optimized
    for value in ("3\n", "-7\n"):
        case.compare(value)


def test_local_function_alias_and_sibling_reads_preserve_unbound_errors(tmp_path):
    source = """\
def conditional(flag):
    def value():
        return 1
    if flag:
        alias = value
    return alias()

def before_definition():
    def caller():
        return later()
    try:
        caller()
    except NameError:
        print('unbound sibling')
    def later():
        return 2
    return caller()

for flag in (False, True):
    try:
        print(conditional(flag))
    except UnboundLocalError:
        print('unbound alias')
print(before_definition())
"""
    case = build_case(tmp_path, source, optimize=2)
    case.compare()


@pytest.mark.parametrize("replacement", ("1", "None"))
def test_local_capture_replacement_releases_previous_heap_value(tmp_path, replacement):
    source = """\
import _weakref

class Watched:
    pass

def outer():
    value = Watched()
    observed = _weakref.ref(value)
    def replace():
        nonlocal value
        value = REPLACEMENT
    replace()
    print(observed() is None)

outer()
""".replace("REPLACEMENT", replacement)
    case = build_case(tmp_path, source, optimize=2)
    case.compare()


def test_readonly_capture_does_not_extend_object_lifetime_after_last_call(tmp_path):
    source = """\
import _weakref

class Watched:
    pass

def outer(value):
    observed = _weakref.ref(value)
    def touch():
        return value is None
    print(touch())
    print(observed() is None)

outer(Watched())
"""
    case = build_case(tmp_path, source, optimize=2)
    case.compare()


def test_recursive_transitive_and_escaping_closures_preserve_shared_state(tmp_path):
    source = """\
def recursive(n):
    def factorial(k):
        if k < 2:
            return 1
        return k * factorial(k - 1)
    return factorial(n)

def mutual(n):
    def even(k):
        if k == 0:
            return True
        return odd(k - 1)
    def odd(k):
        if k == 0:
            return False
        return even(k - 1)
    return even(n)

def transitive(value):
    def middle():
        def inner():
            nonlocal value
            value += 3
            return value
        return inner()
    return middle() + middle()

def sibling(value):
    def bump():
        nonlocal value
        value += 1
    def read():
        return value
    bump()
    return read

def unbound():
    def read():
        return value
    try:
        read()
    except NameError:
        print('not initialized')
    value = 3
    del value
    try:
        read()
    except NameError:
        print('deleted')

number = int(input())
print(recursive(number))
print(mutual(number))
print(transitive(number))
print(sibling(number)())
unbound()
"""
    case = build_case(tmp_path, source, optimize=2)
    for value in ("4\n", "5\n"):
        case.compare(value)


def test_observable_function_and_suspended_owner_keep_object_environments(tmp_path):
    source = """\
def decorated(function):
    function.label = 'visible'
    return function

def factory(value):
    @decorated
    def with_default(amount=2):
        return value + amount
    print(with_default.label, len(with_default.__closure__))
    return with_default

def suspended(value):
    def read():
        return value
    yield read()
    value += 1
    yield read()

number = int(input())
function = factory(number)
print(function(), function(3))
iterator = suspended(number)
print(next(iterator), next(iterator))
"""
    case = build_case(tmp_path, source, optimize=2)
    for value in ("3\n", "-7\n"):
        case.compare(value)


def test_local_annotation_elision_preserves_lazy_expressions_and_default_effects(
    tmp_path,
):
    source = """\
events = []

def record():
    events.append('evaluated')
    return int

def outer():
    def increment(value: record()) -> record():
        return value + 1
    print(increment(3), events)
    def metadata(format, default=record()):
        return {}
    increment.__annotate__ = metadata
    print(increment(4), events)

outer()
"""
    case = build_case(tmp_path, source, optimize=2)
    case.compare()


def test_local_calls_preserve_native_scalar_and_unpacked_return_entries(tmp_path):
    source = """\
from mule_types import int64, optimize

@optimize(speed=3, safety=0)
def outer(value):
    @optimize(speed=3, safety=0)
    def twice(number: int64) -> int64:
        return number + number
    def pair(number):
        return twice(number), twice(number + 1)
    left, right = pair(value)
    return left + right

number = int(input())
print(outer(number))
"""
    case = build_case(tmp_path, source, optimize=2)
    owner = case.function_ir(r"^mule_fn_outer\.body$")
    assert "pair.body.unpacked" in owner
    pair = case.function_ir(
        r"^mule_fn_outer\.<locals>\.pair\.body\.unpacked$", optimized=False
    )
    assert "@mule_sequence_tuple_owned(" not in pair
    assert "@mule_sequence_tuple_owned(" not in owner
    for value in ("3\n", "-7\n"):
        case.compare(value)


@pytest.mark.parametrize("mutable", (False, True))
def test_local_environment_calls_preserve_numeric_internal_conventions(
    tmp_path, mutable
):
    operation = (
        "nonlocal base\n        base += delta\n        return base"
        if mutable
        else "return base + delta"
    )
    source = f"""\
def total(text):
    base = float(text)
    def add(delta):
        {operation}
    return add(1.25) + add(2.5)

print(total(input()))
"""
    case = build_case(tmp_path, source, optimize=2)
    owner = case.function_ir(r"^mule_fn_total\.body$", optimized=False)
    if not mutable:
        assert "call double" in owner
        assert "add.numeric." in owner
    assert "mule.protocol.cell_new" not in owner
    assert "@mule_function_closure(" not in owner
    for value in ("2.5\n", "-0.0\n"):
        case.compare(value)


@pytest.mark.parametrize("annotated", (False, True))
def test_loop_local_closure_elides_allocations_in_native_and_fallback_iteration(
    tmp_path, annotated
):
    annotation = ": int" if annotated else ""
    returned = " -> int" if annotated else ""
    source = f"""\
def kernel(size{annotation}){returned}:
    total = 0
    for index in range(size):
        current = index
        def add(delta{annotation}){returned}:
            nonlocal current
            current += delta
            return current
        total += add(2) + add(3)
    return total

builtin_range = range
def replacement_range(size):
    return list(builtin_range(size))

number = int(input())
print(kernel(number))
globals()['range'] = replacement_range
print(kernel(number))
"""
    llvm = emit_llvm(source)
    bodies = "\n".join(
        match.group()
        for match in re.finditer(
            r'^define[^\n]*@"mule_fn_kernel\.(?:body|spec\.\d+)"\([^\n]*\{\n.*?^}',
            llvm,
            re.MULTILINE | re.DOTALL,
        )
    )
    assert bodies
    assert "@mule_function_closure(" not in bodies
    assert "@mule_sequence_tuple_owned(" not in bodies
    assert "@mule_function_defaults(" not in bodies
    assert "mule.protocol.cell_new" not in bodies
    case = build_case(tmp_path, source, optimize=2)
    optimized = case.function_ir(r"^mule_fn_kernel\.(?:body|spec\.\d+)$")
    assert "@mule_function_closure(" not in optimized
    assert "@mule_sequence_tuple_owned(" not in optimized
    assert "@mule_function_defaults(" not in optimized
    assert "mule.protocol.cell_new" not in optimized
    for value in ("3\n", "7\n"):
        case.compare(value)


def test_loop_alternative_lowerings_share_code_but_keep_fresh_defaults_and_functions(
    tmp_path,
):
    source = """\
events = []
def record(value):
    events.append(value)
    return value

def collect(size):
    functions = []
    for index in range(size):
        def held(value=record(index)):
            return value
        functions.append(held)
    return functions

builtin_range = range
def replacement_range(size):
    return list(builtin_range(size))

first = collect(2)
globals()['range'] = replacement_range
second = collect(2)
print(first[0] is first[1], first[0] is second[0])
print(first[0].__code__ is second[0].__code__)
print(first[0](), first[1](), second[0](), second[1](), events)

def distinct(flag):
    if flag:
        def chosen():
            return 1
    else:
        def chosen():
            return 2
    return chosen()

print(distinct(True), distinct(False))
"""
    case = build_case(tmp_path, source, optimize=2)
    case.compare()


def test_direct_and_loop_local_lambdas_use_local_environments(tmp_path):
    source = """\
def direct(value):
    current = value
    return (lambda delta: current + delta)(3)

def loop(size):
    total = 0
    for index in range(size):
        current = index
        total += (lambda delta: current + delta)(3)
        named = lambda delta: current + delta
        total += named(4)
    return total

def escaping(value):
    return lambda: value

number = int(input())
print(direct(number), loop(number), escaping(number)())
print((lambda amount=number + 2: amount)())
print(((lambda: 1) if number > 0 else (lambda: 2))())
"""
    llvm = emit_llvm(source)
    owners = "\n".join(
        match.group()
        for match in re.finditer(
            r'^define[^\n]*@"mule_fn_(?:direct|loop)\.(?:body|spec\.\d+)"\([^\n]*\{\n.*?^}',
            llvm,
            re.MULTILINE | re.DOTALL,
        )
    )
    assert owners
    assert "@mule_function_closure(" not in owners
    assert "@mule_sequence_tuple_owned(" not in owners
    assert "@mule_function_defaults(" not in owners
    assert "mule.protocol.cell_new" not in owners
    case = build_case(tmp_path, source, optimize=2)
    optimized = case.function_ir(r"^mule_fn_(?:direct|loop)\.(?:body|spec\.\d+)$")
    assert "@mule_function_closure(" not in optimized
    assert "@mule_sequence_tuple_owned(" not in optimized
    assert "@mule_function_defaults(" not in optimized
    assert "mule.protocol.cell_new" not in optimized
    for value in ("3\n", "7\n", "-2\n"):
        case.compare(value)
