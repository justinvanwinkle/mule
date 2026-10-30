"""Ownership-specialized entries borrow independently rooted caller values."""

import re

from mule.compiler import emit_llvm
from mule.compiler.driver import compile_program
from mule.compiler.escape import analyze_escapes
from mule.compiler.main import compile_source
from test.mule.compiler.test_exception_groups import compare
from test.mule.compiler.test_llvm_boxing import function_body

SOURCE = """
def is_none(value):
    return value is None
def caller(value):
    result = is_none(value)
    return result, value
"""


def test_rooted_argument_requests_ownership_specialized_body():
    llvm = emit_llvm(SOURCE)
    borrowed = function_body(llvm, '"mule_fn_is_none.borrow.general.0"')
    caller = function_body(llvm, '"mule_fn_caller.body"')
    assert "@mule.fast.retain" not in caller
    assert "@mule.fast.release" not in borrowed
    assert "icmp eq ptr" in borrowed


def test_typed_borrowed_entry_uses_backend_context_and_preserves_owners(tmp_path):
    source = """
def add_if_none(count, value):
    return count + 1 if value is None else count
def caller(value, count):
    result = add_if_none(count, value)
    return result, value
"""
    borrowed = function_body(emit_llvm(source), '"mule_fn_add_if_none.borrow.fixnum.1"')
    assert "icmp eq ptr" in borrowed
    compare(
        tmp_path,
        source
        + """
for value in ([], None):
    for count in (2, -3, 1.5):
        print(caller(value, count))
""",
    )


def test_unknown_truth_and_callable_callbacks_keep_escaping_contract():
    compilation = compile_source("""
def truth(value):
    return 1 if value else 0
def invoke(value):
    return value()
""")
    facts = analyze_escapes(compilation.lambdas)
    for function in compilation.lambdas[1:]:
        assert facts.parameter_escapes(function, 0)


def test_temporary_argument_keeps_owning_entry_and_mixed_failure_releases_owner():
    # Debug 2 keeps the mixed entry fallible without requiring checks on leaves.
    llvm = emit_llvm(
        "from mule_types import optimize\n"
        + SOURCE
        + """
def temporary():
    return is_none([])
@optimize(debug=2)
def choose(owner, token):
    return token if owner is None else False
def mixed(value):
    result = choose(value, [])
    return result, value
"""
    )
    helpers = re.findall(
        r'^define internal ptr @"mule.direct.[^\n]*\{\n.*?^\}',
        llvm,
        re.MULTILINE | re.DOTALL,
    )
    owning = next(
        body for body in helpers if 'call ptr @"mule_fn_is_none.body"' in body
    )
    assert "@mule_enter_frame" not in owning
    owning_body = function_body(llvm, '"mule_fn_is_none.body"')
    assert "call void @mule.fast.release" in owning_body
    mixed = next(
        body for body in helpers if 'call ptr @"mule_fn_choose.borrow.' in body
    )
    failure = mixed.split("failed:\n", 1)[1].split("fallback:\n", 1)[0]
    assert "release(ptr %arg.1)" in failure
    assert "release(ptr %arg.0)" not in failure
    fallback = mixed.split("fallback:\n", 1)[1]
    assert "retain(ptr %arg.0)" in fallback
    assert "retain(ptr %arg.1)" not in fallback


def test_guarded_borrowed_call_preserves_rebound_fallback_and_owners(tmp_path):
    compare(
        tmp_path,
        SOURCE
        + """
value = []
print(caller(value), value)
original = is_none
def replacement(value):
    value.append(3)
    return False
is_none = replacement
print(caller(value), value)
is_none = original
print(caller(None))
print(is_none([]))
""",
    )


def test_callee_copy_across_callback_acquires_owned_local_reference(tmp_path):
    source = """
def copied(value):
    saved = value
    print('copy')
    return saved is None
def caller(value):
    result = copied(value)
    return result, value
"""
    body = function_body(emit_llvm(source), '"mule_fn_copied.borrow.general.0"')
    assert "call ptr @mule.fast.retain" in body
    assert "call void @mule.fast.release" in body
    compare(
        tmp_path,
        source
        + """
value = []
print(caller(value), value)
print(caller(None))
""",
    )


def test_exact_dict_lookup_releases_consumed_temporary(tmp_path):
    compare(
        tmp_path,
        """
import gc
marker = []
def lookup(value):
    mapping = {'unique-owned-dict-probe': value}
    return mapping['unique-owned-dict-probe']
result = lookup(marker)
found = False
for obj in gc.get_objects():
    if type(obj) is dict and obj.get('unique-owned-dict-probe') is marker:
        found = True
print(result is marker, found)
""",
    )


def test_mixed_borrowed_call_recursion_failure_and_exception_cleanup(tmp_path):
    compare(
        tmp_path,
        """
def choose(owner, token):
    return token if owner is None else False
def recurse(owner, count):
    result = choose(owner, [])
    if count:
        return recurse(owner, count - 1)
    return result
marker = []
try:
    recurse(marker, 2000)
except RecursionError:
    print('limit', marker == [], choose(marker, 17))
def caller(owner):
    try:
        result = choose(owner, [])
    except ValueError:
        result = 'caught'
    return result, owner
def replacement(owner, token):
    owner.append(3)
    raise ValueError('expected')
globals()['choose'] = replacement
print(caller(marker), marker)
""",
    )


def test_exact_dict_protocols_borrow_rooted_operands_and_preserve_lifetimes(tmp_path):
    source = """
def update_word(word):
    counts = {"alpha": 1}
    if word in counts:
        counts[word] = counts[word] + 1
    else:
        counts[word] = counts.get(word, 0) + 1
    return counts[word]
"""
    llvm = emit_llvm(source)
    body = function_body(llvm, '"mule_fn_update_word.body"')
    assert 'call ptr @"mule.protocol.contains"' in body
    assert "call ptr @mule.fast.getitem(" in body
    assert "@mule.attr.method(" in body
    assert "call ptr @mule.fast.setitem" in body
    compare(
        tmp_path,
        source
        + """
for item in ["alpha", "beta"]:
    print(update_word(item))
""",
    )


def test_keyword_default_and_builtin_method_consumers_keep_parameters_borrowed():
    program = compile_program("""
import math

def top_helper(value, scale: int = 1):
    return (value is None) or (scale == 0)

def caller_kw_and_default(first, second):
    def helper(value, scale=1):
        return (value is None) or (scale == 0)

    def string_methods(text: str, sub: str, sep: str) -> int:
        if not text:
            return 0
        a = text.find(sub)
        b = text.startswith(sub)
        parts = text.split(sep)
        head, _, tail = text.partition(sep)
        return a + (1 if b else 0) + len(parts) + len(head) + len(tail)

    def container_methods(
        items: list[int], buf: bytearray, mapping: dict[str, int], key: str
    ) -> int:
        items.append(1)
        popped = items.pop()
        buf.append(65)
        got = mapping.get(key, 0)
        return popped + got + len(items) + len(buf)

    def math_and_compare(x: float, y: int) -> bool:
        return math.sqrt(x) > 0.0 and y == 3 and not y

    s = string_methods("alpha,beta", "a", ",")
    c = container_methods([1, 2], bytearray(b"x"), {"k": 1}, "k")
    m = math_and_compare(4.0, 3)
    return (
        helper(value=first)
        and helper(second, scale=2)
        and (s + c > 0 or m)
    )
""")
    facts = analyze_escapes(program.lambdas)
    by_name = {fn.code.name: fn for fn in program.lambdas if fn.code is not None}
    assert not facts.parameter_escapes(by_name["top_helper"], 0)
    assert not facts.parameter_escapes(by_name["helper"], 0)
    assert not facts.parameter_escapes(by_name["helper"], 1)
    assert not facts.parameter_escapes(by_name["caller_kw_and_default"], 0)
    assert not facts.parameter_escapes(by_name["caller_kw_and_default"], 1)
    assert not facts.parameter_escapes(by_name["string_methods"], 0)
    assert not facts.parameter_escapes(by_name["string_methods"], 1)
    assert not facts.parameter_escapes(by_name["string_methods"], 2)
    assert not facts.parameter_escapes(by_name["container_methods"], 0)
    assert not facts.parameter_escapes(by_name["container_methods"], 1)
    assert not facts.parameter_escapes(by_name["container_methods"], 2)
    assert not facts.parameter_escapes(by_name["container_methods"], 3)
    assert not facts.parameter_escapes(by_name["math_and_compare"], 0)
    assert not facts.parameter_escapes(by_name["math_and_compare"], 1)


def test_cross_module_foreign_function_escape_summary_borrows_parameter(tmp_path):
    lib_path = tmp_path / "helper_mod.py"
    lib_path.write_text(
        """\
def inspect_only(value, extra: int = 1):
    return (value is None) or (extra == 0)

def captures_arg(value):
    return [value]
""",
        encoding="utf-8",
    )
    main_source = """\
import helper_mod

def pass_to_borrowed(item):
    return helper_mod.inspect_only(item)

def pass_to_borrowed_kw(item):
    return helper_mod.inspect_only(value=item, extra=2)

def pass_to_capturing(item):
    return helper_mod.captures_arg(item)

def caller(item):
    return (
        pass_to_borrowed(item),
        pass_to_borrowed_kw(item),
        pass_to_capturing(item),
        item,
    )
"""
    llvm = emit_llvm(main_source, roots=(tmp_path,))
    borrowed = function_body(llvm, '"mule_fn_pass_to_borrowed.borrow.general.0"')
    assert "store ptr %arg.0, ptr %local.0" not in borrowed
    assert "release(ptr %arg.0)" not in borrowed
    kw_borrowed = function_body(llvm, '"mule_fn_pass_to_borrowed_kw.borrow.general.0"')
    assert "store ptr %arg.0, ptr %local.0" not in kw_borrowed
    assert "release(ptr %arg.0)" not in kw_borrowed
    assert '"mule_fn_pass_to_capturing.borrow.' not in llvm
